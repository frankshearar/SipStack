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
  IdConnectionBindings, IdInterfacedObject, IdSipCore, IdSipMessage,
  IdSipMockBindingDatabase, IdSipMockTransactionDispatcher, IdSipRegistration,
  SysUtils, TestFramework, TestFrameworkSip, TestFrameworkSipTU;

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
    procedure TestReceiveGruu;
    procedure TestReceiveGruuSips;
    procedure TestReceiveInvite;
    procedure TestReceiveRegister;
    procedure TestReceiveExpireTooShort;
    procedure TestReceiveExpireParamTooShort;
    procedure TestReceiveWildcard;
    procedure TestReceiveWildcardWithExtraContacts;
    procedure TestReceiveWildcardWithNonzeroExpiration;
    procedure TestReceiveWildcardWithZeroExpiresHeader;
    procedure TestReceiveWildcardWithZeroExpiresHeaderButNonzeroExpiresParam;
    procedure TestRegisterAddsBindings;
    procedure TestRegisterAddsMultipleBindings;
    procedure TestRejectRegisterWithReplacesHeader;
    procedure TestUARequiresGruuRegistrarDoesntSupportGruu;
    procedure TestUnauthorizedUser;
  end;

  TestTIdSipOutboundRegisterModule = class(TTestCaseTU)
  private
    Module:       TIdSipOutboundRegisterModule;
    Registrars:   TIdSipRegistrations;
    RegistrarOne: TIdSipUri;
    RegistrarTwo: TIdSipUri;
    RemoteUri:    TIdSipURI;

    function  ActualContact: TIdSipContactHeader;
    procedure CheckEquals(Expected, Received: TIdSipContacts); overload;
    procedure CheckRequestsSent(ExpectedCount: Cardinal; Msg: String);
    procedure CheckUnregister(Expected: TIdSipContactHeader; Reg: TIdSipRequest; MsgPrefix: String);
    procedure ReceiveOkFromRegistrar(Reg: TIdSipRequest;
                                     Contacts: TIdSipContacts);
    procedure RegisterWith(Registrar: TIdSipUri;
                           Contact: TIdSipContactHeader); overload;
    procedure RegisterWith(Registrar: TIdSipUri;
                           Contacts: TIdSipContacts); overload;
    procedure UnregisterFrom(Registrar: TIdSipUri;
                             Contact: TIdSipContactHeader); overload;
    procedure UnregisterFrom(Registrar: TIdSipUri;
                             Contacts: TIdSipContacts); overload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCleanUpUnregisters;
    procedure TestCleanUpUnregistersOnlyCurrentlyRegisteredContacts;
    procedure TestCreateRegister;
    procedure TestCreateRegisterReusesCallIDForSameRegistrar;
    procedure TestCreateRegisterWithGruu;
    procedure TestCreateRegisterWithRequiredGruu;
    procedure TestUnregisterFrom;
    procedure TestUnregisterFromMultipleBindings;
  end;

  TestTIdSipRegisterModule = class(TTestCaseTU)
  private
    Module: TIdSipRegisterModule;
    Params: TIdSipHeaderParameters;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConfigureMinTime;
    procedure TestConfigureMinTimeBadValue;
    procedure TestConfigureRegTime;
    procedure TestConfigureRegTimeBadValue;
    procedure TestConfigureUseGruu;
    procedure TestConfigureUseGruuBadValue;
  end;

  TestTIdSipRegistration = class(TestTIdSipAction)
  private
    RegisterModule: TIdSipRegisterModule;
  protected
    Contacts: TIdSipContacts;

    procedure CheckLocalAddress(ExpectedIP: String; DestinationIP: String; Msg: String; ExpectedSentBy: String = ''); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsRegistration; override;
  end;

  TestTIdSipInboundRegistration = class(TestTIdSipRegistration)
  private
    RegisterAction: TIdSipInboundRegistration;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsInbound; override;
    procedure TestIsInvite; override;
    procedure TestIsOptions; override;
    procedure TestIsOutbound; override;
    procedure TestIsOwned; override;
    procedure TestIsRegistration; override;
    procedure TestIsSession; override;
    procedure TestMethod;
    procedure TestTerminateSignalled; override;
  end;

  TestTIdSipOutboundRegisterBase = class(TestTIdSipRegistration,
                                         IIdSipOwnedActionListener)
  private
    MinExpires: Cardinal;
    Redirected: Boolean;
    Registrar:  TIdSipRegistrar;
    Succeeded:  Boolean;

    procedure OnFailure(Action: TIdSipAction;
                        Response: TIdSipResponse;
                        const Reason: String);
    procedure OnRedirect(Action: TIdSipAction;
                         Redirect: TIdSipResponse);
    procedure OnSuccess(Action: TIdSipAction;
                        Msg: TIdSipMessage);
    procedure ReceiveRemoteIntervalTooBrief;
  protected
    function RegistrarAddress: TIdSipUri;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsOwned; override;
    procedure TestMethod;
    procedure TestReceiveFail;
    procedure TestReceiveIntervalTooBrief;
    procedure TestReceiveMovedPermanently;
    procedure TestReceiveOK;
    procedure TestSequenceNumberIncrements;
  end;

  TExpiryProc = procedure(ExpiryTime: Cardinal) of object;

  TestTIdSipOutboundRegister = class(TestTIdSipOutboundRegisterBase)
  protected
    function  CreateAction: TIdSipAction; override;
  published
    procedure TestReceiveIntervalTooBriefForOneContact;
    procedure TestRegister;
    procedure TestRegisterWithInstanceID;
  end;

  TestTIdSipOutboundRegisterQuery = class(TestTIdSipOutboundRegisterBase)
  protected
    function CreateAction: TIdSipAction; override;
  published
    procedure TestFindCurrentBindings;
  end;

  TestTIdSipOutboundUnregister = class(TestTIdSipOutboundRegisterBase)
  private
    Bindings: TIdSipContacts;
    WildCard: Boolean;
  protected
    function CreateAction: TIdSipAction; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUnregisterAll;
    procedure TestUnregisterSeveralContacts;
  end;

  TOutboundRegistrationBaseTestCase = class(TestTIdSipRegistration,
                                            IIdSipRegistrationListener)
  protected
    ErrorCode: Cardinal;
    Failed:    Boolean;
    Reason:    String;
    Registrar: TIdSipRegistrar;

    function  CreateRegistrationWithoutSend: TIdSipOutboundRegistrationBase; virtual;
    function  CreateAction: TIdSipAction; override;
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistrationBase;
                        ErrorCode: Cardinal;
                        const Reason: String);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistrationBase;
                        CurrentBindings: TIdSipContacts);
    function  RegistrarAddress: TIdSipUri;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddListener;
    procedure TestCircularRedirect;
    procedure TestDoubleRedirect;
    procedure TestRedirectMultipleOks;
    procedure TestRedirectNoMoreTargets;
    procedure TestRedirectWithMaxForwards;
    procedure TestRedirectWithMultipleContacts;
    procedure TestRedirectWithNoSuccess;
    procedure TestRegisterWithMaxForwards;
    procedure TestRemoveListener;
  end;

  TestTIdSipOutboundRegistration = class(TOutboundRegistrationBaseTestCase)
  private
    procedure CheckAutoReregister(ReceiveResponse: TExpiryProc;
                                  EventIsScheduled: Boolean;
                                  const MsgPrefix: String);
    procedure ReceiveOkWithContactExpiresOf(ExpiryTime: Cardinal);
    procedure ReceiveOkWithExpiresOf(ExpiryTime: Cardinal);
    procedure ReceiveOkWithNoExpires(ExpiryTime: Cardinal);
  published
    procedure TestAutoReregister;
    procedure TestAutoReregisterContactHasExpires;
    procedure TestAutoReregisterNoExpiresValue;
    procedure TestAutoReregisterSwitchedOff;
    procedure TestFailWithMultipleListeners;
    procedure TestNetworkFailureWithMultipleListeners;
    procedure TestReceiveGruu;
    procedure TestReceiveNoGruu;
    procedure TestRegisterUsesUAsFrom;
    procedure TestReregisterTime;
    procedure TestSuccessWithMultipleListeners;
  end;

  TestTIdSipOutboundRegistrationQuery = class(TOutboundRegistrationBaseTestCase)
  protected
    function CreateRegistrationWithoutSend: TIdSipOutboundRegistrationBase; override;
  end;

  TestRegistrationMethod = class(TActionMethodTestCase)
  protected
    Bindings: TIdSipContacts;
    Reg:      TIdSipOutboundRegistrationBase;
    Listener: TIdSipTestRegistrationListener;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSipRegistrationFailedMethod = class(TestRegistrationMethod)
  private
    Method: TIdSipRegistrationFailedMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipRegistrationSucceededMethod = class(TestRegistrationMethod)
  private
    Method: TIdSipRegistrationSucceededMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSipReregisterWait = class(TTestCaseTU)
  private
    AOR:  TIdSipAddressHeader;
    Wait: TIdSipReregisterWait;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
  end;

  TExceptionCatchingActionListener = class(TIdInterfacedObject,
                                           IIdSipActionListener,
                                           IIdSipTransactionUserListener)
  private
    fExceptionType:    ExceptClass;
    fExceptionMessage: String;

    procedure OnAddAction(UserAgent: TIdSipAbstractCore;
                          Action: TIdSipAction);
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Challenge: TIdSipResponse);
    procedure OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                        Message: TIdSipMessage;
                                        Binding: TIdConnectionBindings);
    procedure OnNetworkFailure(Action: TIdSipAction;
                               ErrorCode: Cardinal;
                               const Reason: String);
    procedure OnRemoveAction(UserAgent: TIdSipAbstractCore;
                             Action: TIdSipAction);
    procedure OnTerminated(Action: TIdSipAction);
  public
    constructor Create; override;

    property ExceptionType:    ExceptClass read fExceptionType;
    property ExceptionMessage: String      read fExceptionMessage;
  end;

implementation

uses
  Classes, DateUtils, IdSipAuthentication, IdTimerQueue;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipRegistrar unit tests');
  Result.AddTest(TestTIdSipRegistrar.Suite);
  Result.AddTest(TestTIdSipOutboundRegisterModule.Suite);
  Result.AddTest(TestTIdSipRegisterModule.Suite);
  Result.AddTest(TestTIdSipInboundRegistration.Suite);
  Result.AddTest(TestTIdSipOutboundRegister.Suite);
  Result.AddTest(TestTIdSipOutboundRegisterQuery.Suite);
  Result.AddTest(TestTIdSipOutboundUnregister.Suite);
  Result.AddTest(TestTIdSipOutboundRegistration.Suite);
  Result.AddTest(TestTIdSipOutboundRegistrationQuery.Suite);
  Result.AddTest(TestTIdSipRegistrationFailedMethod.Suite);
  Result.AddTest(TestTIdSipRegistrationSucceededMethod.Suite);
  Result.AddTest(TestTIdSipReregisterWait.Suite);
end;

const
  FortyTwoSeconds = '42';

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
  Self.Registrar.Authenticator := TIdSipNullAuthenticator.Create;
  Self.Registrar.BindingDB := Self.DB;
  Self.Registrar.Dispatcher := Self.Dispatch;
  Self.Registrar.MinimumExpiryTime := 3600;

  Self.Request := TIdSipRequest.Create;
  Self.Request.Method := MethodRegister;
  Self.Request.RequestUri.Uri := 'sip:tessier-ashpool.co.luna';
  Self.Request.AddHeader(ViaHeaderFull).Value := 'SIP/2.0/UDP proxy.tessier-ashpool.co.luna;branch='
                                               + BranchMagicCookie + 'f00L';
  Self.Request.ToHeader.Address.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Request.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';
  Self.Request.CSeq.Method := Self.Request.Method;
  Self.Request.CallID := '1@selftest.foo';
  Self.Request.From.Address.Uri := 'sip:case@fried.neurons.org';

  Self.FirstContact := Self.Request.FirstHeader(ContactHeaderFull) as TIdSipContactHeader;

  // No A/AAAA records mean no possible locations!
  Self.Dispatch.MockLocator.AddA(Self.Request.LastHop.SentBy, '127.0.0.1');
end;

procedure TestTIdSipRegistrar.TearDown;
begin
  // The registrar will free Self.DB.
  Self.Request.Free;
  Self.Registrar.Authenticator.Free;
  Self.Registrar.Free;
  Self.Dispatch.Free;

  inherited TearDown;
end;

//* TestTIdSipRegistrar Private methods ****************************************

procedure TestTIdSipRegistrar.CheckResponse(Received: TIdSipContacts;
                                            const Msg: String);
var
  Diff:     Int64;
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

      // We use a complicated Int64 method because the expected Expires might
      // be less than the received Expires. Simply doing
      // Abs(Expected - Received) will use a temporary Cardinal, which will
      // overflow if Expected < Received.
      Diff := Expected.CurrentContact.Expires;
      Diff := Diff - Received.CurrentContact.Expires;

      // The "2" here means we look for a difference in the Expires times of
      // two seconds: the registrar should have received our request and
      // responded within that time.
      CheckNotEquals(0, Expected.CurrentContact.Expires, 'Expected Expires = 0?');
      Check(Abs(Diff) < 2, 'Expires param; I = ' + IntToStr(I));

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

procedure TestTIdSipRegistrar.TestReceiveGruu;
const
  OurUrn = 'urn:uuid:00000000-0000-0000-0000-000000000000';
var
  ServerResponse: TIdSipResponse;
begin
  // Does the GRUU-supporting registrar actually return GRUUs for each Contact
  // in a REGISTER?

  Self.Registrar.UseGruu := true;
  Self.Request.AddHeader(SupportedHeaderFull).Value := ExtensionGruu;
  Self.Request.FirstContact.SipInstance := OurUrn;
  Self.SimulateRemoteRequest;

  Self.CheckServerReturned(SIPOK, 'REGISTER with GRUU');
  ServerResponse := Self.Dispatch.Transport.LastResponse;
  Check(ServerResponse.HasHeader(RequireHeader),
        'draft-ietf-sip-gruu-10 says the response MUST have a Require header '
      + '(section 7.1.2.1); RFC 3261 section 20 says only requests can have '
      + 'one.');

  ServerResponse.Contacts.First;
  while ServerResponse.Contacts.HasNext do begin
    Check(ServerResponse.Contacts.CurrentContact.HasParameter(GruuParam),
          'Binding ' + ServerResponse.Contacts.CurrentContact.FullValue
        + ' has no "gruu" param');

    ServerResponse.Contacts.Next;
  end;
end;

procedure TestTIdSipRegistrar.TestReceiveGruuSips;
const
  OurUrn = 'urn:uuid:00000000-0000-0000-0000-000000000000';
var
  ServerResponse: TIdSipResponse;
begin
  // Does the GRUU-supporting registrar actually return SIPS GRUUs for each SIPS
  // Contact in a REGISTER?

  Self.Registrar.UseGruu := true;
  Self.Request.AddHeader(SupportedHeaderFull).Value := ExtensionGruu;
  Self.Request.FirstContact.Address.Scheme := SipsScheme;
  Self.Request.FirstContact.SipInstance := OurUrn;
  Self.SimulateRemoteRequest;

  Self.CheckServerReturned(SIPOK, 'REGISTER with SIPS GRUU');
  ServerResponse := Self.Dispatch.Transport.LastResponse;
  ServerResponse.Contacts.First;
  while ServerResponse.Contacts.HasNext do begin
    Check(ServerResponse.Contacts.CurrentContact.HasParameter(GruuParam),
          'Binding ' + ServerResponse.Contacts.CurrentContact.FullValue
        + ' has no "gruu" param');
    Check(ServerResponse.Contacts.CurrentContact.Address.IsSipsUri,
          'Binding ' + ServerResponse.Contacts.CurrentContact.FullValue
        + ' is not a SIPS URI');

    ServerResponse.Contacts.Next;
  end;
end;

procedure TestTIdSipRegistrar.TestReceiveInvite;
begin
  Self.Request.Method      := MethodInvite;
  Self.Request.CSeq.Method := Self.Request.Method;
  Self.SimulateRemoteRequest;

  Self.CheckServerReturned(SIPNotImplemented,
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
              Response.MinExpires.NumericValue,
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
              Response.MinExpires.NumericValue,
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

procedure TestTIdSipRegistrar.TestReceiveWildcardWithZeroExpiresHeader;
begin
  Self.DB.AddBindings(Self.Request);
  // Remember the rules of RFC 3261 section 10.3 step 6!
  Self.Request.CallID := Self.Request.CallID + '1';

  Self.FirstContact.IsWildCard := true;
  Self.Request.AddHeader(ExpiresHeader).Value := '0';
  Self.SimulateRemoteRequest;

  Self.CheckServerReturnedOK('Wildcard contact with Expires header');

  CheckEquals(0, Self.DB.BindingCount, 'No bindings removed');
end;

procedure TestTIdSipRegistrar.TestReceiveWildcardWithZeroExpiresHeaderButNonzeroExpiresParam;
var
  OldBindingCount: Integer;
begin
  Self.DB.AddBindings(Self.Request);
  OldBindingCount := Self.DB.BindingCount;
  // Remember the rules of RFC 3261 section 10.3 step 6!
  Self.Request.CallID := Self.Request.CallID + '1';

  Self.FirstContact.IsWildCard := true;
  Self.FirstContact.Expires := 1;
  Self.Request.AddHeader(ExpiresHeader).Value := '0';
  Self.SimulateRemoteRequest;

  Self.CheckServerReturned(SIPBadRequest, 'Wildcard contact with (zero) Expires header and (nonzero) "expires" parameter.');

  CheckEquals(OldBindingCount, Self.DB.BindingCount, 'Bindings removed');
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

procedure TestTIdSipRegistrar.TestRejectRegisterWithReplacesHeader;
begin
  Self.Request.AddHeader(ReplacesHeader).Value := '1;from-tag=2;to-tag=3';
  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPBadRequest,
                           'Replaces header in a REGISTER');
end;

procedure TestTIdSipRegistrar.TestUARequiresGruuRegistrarDoesntSupportGruu;
begin
  Self.Request.Require.Values.Add(ExtensionGruu);
  Self.Registrar.UseGruu := false;

  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPBadExtension,
                           'Registrar doesn''t understand GRUU, but UA '
                         + 'required it');
end;

procedure TestTIdSipRegistrar.TestUnauthorizedUser;
begin
  Self.DB.Authorized := false;
  Self.SimulateRemoteRequest;
  Self.CheckServerReturned(SIPForbidden,
                           'Unauthorized user''s request not rejected');
end;

//******************************************************************************
//* TestTIdSipOutboundRegisterModule                                           *
//******************************************************************************
//* TestTIdSipOutboundRegisterModule Public methods ****************************

procedure TestTIdSipOutboundRegisterModule.SetUp;
const
  RegistrarOneUri = 'sip:reg1@tessier-ashpool.co.luna';
  RegistrarTwoUri = 'sip:proxy.leo-ix.net';
begin
  inherited SetUp;

  Self.Module       := Self.Core.RegisterModule;
  Self.RegistrarOne := TIdSipUri.Create(RegistrarOneUri);
  Self.Registrars   := TIdSipRegistrations.Create;
  Self.RegistrarTwo := TIdSipUri.Create(RegistrarTwoUri);
  Self.RemoteUri    := TIdSipURI.Create('sip:wintermute@tessier-ashpool.co.luna');
end;

procedure TestTIdSipOutboundRegisterModule.TearDown;
begin
  Self.RemoteUri.Free;
  Self.RegistrarTwo.Free;
  Self.Registrars.Free;
  Self.RegistrarOne.Free;
  // Self.Module is freed by Self.Core.

  inherited TearDown;
end;

//* TestTIdSipOutboundRegisterModule Private methods ***************************

function TestTIdSipOutboundRegisterModule.ActualContact: TIdSipContactHeader;
begin
  Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;

  Result := Self.LastSentRequest.FirstContact;
end;

procedure TestTIdSipOutboundRegisterModule.CheckEquals(Expected, Received: TIdSipContacts);
begin
  Expected.First;
  Received.First;

  while (Expected.HasNext and Received.HasNext) do begin
    CheckEquals(Expected.CurrentContact.Address.AsString,
                Received.CurrentContact.Address.AsString,
                'Incorrect Contact');

    Expected.Next;
    Received.Next;
  end;
  Check(Expected.HasNext = Received.HasNext,
        'Either not all Contacts in the un-REGISTER, or too many contacts');
end;

procedure TestTIdSipOutboundRegisterModule.CheckRequestsSent(ExpectedCount: Cardinal; Msg: String);
begin
  CheckEquals(ExpectedCount, Self.SentRequestCount - Self.RequestCount, Msg);
end;

procedure TestTIdSipOutboundRegisterModule.CheckUnregister(Expected: TIdSipContactHeader; Reg: TIdSipRequest; MsgPrefix: String);
begin
  CheckEquals(MethodRegister, Reg.Method,
              MsgPrefix + ': Unexpected request sent');
  CheckEquals(0, Reg.QuickestExpiry,
              MsgPrefix + ': Expiry time indicates this isn''t an un-REGISTER');
  CheckEquals(1, Reg.Contacts.Count,
              MsgPrefix + ': Unexpected Contact count');
  CheckEquals(Expected.Address, Reg.FirstContact.Address,
              MsgPrefix + ': Unexpected Contact');
end;

procedure TestTIdSipOutboundRegisterModule.ReceiveOkFromRegistrar(Reg: TIdSipRequest;
                                                                  Contacts: TIdSipContacts);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Reg);
  try
    Response.Contacts := Contacts;

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.RegisterWith(Registrar: TIdSipUri;
                                                        Contact: TIdSipContactHeader);
var
  C: TIdSipContacts;
begin
  C := TIdSipContacts.Create;
  try
    C.Add(Contact);

    Self.RegisterWith(Registrar, C);
  finally
    C.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.RegisterWith(Registrar: TIdSipUri;
                                                        Contacts: TIdSipContacts);
var
  CurrentBindings: TIdSipContacts;
begin
  // Register Contacts with Registrar and receive a 200 OK from Registrar.
  // In other words, this is the call flow around a successful registration.

  Self.Registrars.AddKnownRegistrar(Registrar, '', 0);

  Self.MarkSentRequestCount;
  Self.Module.RegisterWith(Registrar, Contacts).Send;
  CheckRequestSent(Format('No REGISTER sent to <%s>', [Registrar.AsString]));

  Self.Registrars.AddBindings(Registrar, Contacts);

  CurrentBindings := TIdSipContacts.Create;
  try
    Self.Registrars.BindingsFor(Registrar, CurrentBindings);

    Self.ReceiveOkFromRegistrar(Self.LastSentRequest, CurrentBindings);
  finally
    CurrentBindings.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.UnregisterFrom(Registrar: TIdSipUri;
                                                          Contact: TIdSipContactHeader);
var
  C: TIdSipContacts;
begin
  C := TIdSipContacts.Create;
  try
    C.Add(Contact);

    Self.UnregisterFrom(Registrar, C);
  finally
    C.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.UnregisterFrom(Registrar: TIdSipUri;
                                                          Contacts: TIdSipContacts);
var
  RemainingBindings: TIdSipContacts;
begin
  // This is the call flow around a successful UNregistration.

  Self.Registrars.AddKnownRegistrar(Registrar, '', 0);

  Self.MarkSentRequestCount;
  Self.Module.UnregisterFrom(Registrar, Contacts).Send;
  CheckRequestSent(Format('No (un)REGISTER sent to <%s>', [Registrar.AsString]));

  Self.Registrars.RemoveBindings(Registrar, Contacts);

  RemainingBindings := TIdSipContacts.Create;
  try
    Self.Registrars.BindingsFor(Registrar, RemainingBindings);
    Self.ReceiveOkFromRegistrar(Self.LastSentRequest, RemainingBindings);
  finally
    RemainingBindings.Free;
  end;
end;

//* TestTIdSipOutboundRegisterModule Published methods *************************

procedure TestTIdSipOutboundRegisterModule.TestCleanUpUnregisters;
var
  ArbitraryContact: TIdSipContactHeader;
begin
  // Registering to multiple registrars results in un-REGISTERs being sent to
  // each registrar.

  ArbitraryContact := TIdSipContactHeader.Create;
  try
    ArbitraryContact.Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';

    Self.RegisterWith(Self.RegistrarOne, ArbitraryContact);
    Self.RegisterWith(Self.RegistrarTwo, ArbitraryContact);

    Self.MarkSentRequestCount;
    Self.Module.CleanUp;
    CheckRequestsSent(2, 'No REGISTERs sent');
    CheckUnregister(ArbitraryContact, Self.LastSentRequest, 'Last REGISTER');
    CheckUnregister(ArbitraryContact, Self.SecondLastSentRequest, '2nd last REGISTER');
  finally
    ArbitraryContact.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.TestCleanUpUnregistersOnlyCurrentlyRegisteredContacts;
const
  BaseUri = 'sip:%s@example.com';
var
  Contacts: TIdSipContacts;
begin
  // Registering to a registrar, and unregistering only SOME of those Contacts
  // means that when the module cleans up, only the REMAINING Contacts are
  // unregistered.

  Contacts := TIdSipContacts.Create;
  try
    Contacts.Add(ContactHeaderFull).Value := Format(BaseUri, ['one']);
    Contacts.Add(ContactHeaderFull).Value := Format(BaseUri, ['two']);

    Self.RegisterWith(Self.RegistrarOne, Contacts);

    Contacts.First;
    Self.UnregisterFrom(Self.RegistrarOne, Contacts.CurrentContact);

    Self.MarkSentRequestCount;
    Self.Module.CleanUp;
    CheckRequestsSent(1, 'Wrong number of REGISTERs sent');

    Contacts.Next;
    CheckUnregister(Contacts.CurrentContact, Self.LastSentRequest, 'Unregister');
  finally
    Contacts.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.TestCreateRegister;
const
  MaxForwards = 42;
var
  Reg: TIdSipRequest;
begin
  Reg := Self.Module.CreateRegister(Self.Core.From, Self.Destination, MaxForwards);
  try
    CheckEquals(MaxForwards,    Reg.MaxForwards,         'Max-Forwards');
    CheckEquals(MethodRegister, Reg.Method,              'Incorrect method');
    CheckEquals(MethodRegister, Reg.CSeq.Method,         'Incorrect CSeq method');
    CheckEquals('',             Reg.RequestUri.Username, 'Request-URI Username');
    CheckEquals('',             Reg.RequestUri.Password, 'Request-URI Password');

    // Note: we don't check the Contact here because that could change when sent
    // to the network.

    CheckEquals(Self.Core.From.Value,
                Reg.From.Value,
                'From');
    CheckEquals(Reg.From.Value,
                Reg.ToHeader.Value,
                'To');
  finally
    Reg.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.TestCreateRegisterReusesCallIDForSameRegistrar;
var
  FirstCallID:  String;
  Reg:          TIdSipRequest;
  SecondCallID: String;
begin
  Reg := Self.Module.CreateRegister(Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    FirstCallID := Reg.CallID;
  finally
    Reg.Free;
  end;

  Reg := Self.Module.CreateRegister(Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    SecondCallID := Reg.CallID;
  finally
    Reg.Free;
  end;

  CheckEquals(FirstCallID,
              SecondCallID,
              'Call-ID SHOULD be the same for same registrar');

  Self.Destination.Address.Uri := 'sip:enki.org';
  Reg := Self.Module.CreateRegister(Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    CheckNotEquals(FirstCallID,
                   Reg.CallID,
                   'Call-ID SHOULD be different for new registrar');
  finally
    Reg.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.TestCreateRegisterWithGruu;
const
  ZeroURN = 'urn:uuid:00000000-0000-0000-0000-000000000000';
var
  Reg: TIdSipRequest;
begin
  Self.Module.UserAgent.UseGruu    := true;
  Self.Module.UserAgent.InstanceID := ZeroURN;

  // cf. draft-ietf-sip-gruu-10, section 7.1.1.1
  Reg := Self.Module.CreateRegister(Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    Check(Reg.FirstContact.HasParameter(SipInstanceParam),
          'Contact doesn''t have the "' + SipInstanceParam + '" parameter');
    CheckEquals(ZeroURN,
                Reg.FirstContact.Params[SipInstanceParam],
                SipInstanceParam);
    Check(not Reg.FirstContact.Address.HasParameter(GridParam),
          'Contact URI SHOULD NOT have the "' + GridParam + '" parameter');
    Check(not Reg.FirstContact.HasParameter(GruuParam),
          'Contact SHOULD NOT have the "' + GruuParam + '" parameter');
    Check(Reg.HasHeader(SupportedHeaderFull),
          'REGISTER lacks a Supported header');
    CheckNotEquals(ItemNotFoundIndex,
                   Reg.Supported.Values.IndexOf(ExtensionGruu),
                   'Supported header doesn''t indicate support of GRUU');
  finally
    Reg.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.TestCreateRegisterWithRequiredGruu;
var
  Reg: TIdSipRequest;
begin
  Self.Module.UserAgent.UseGruu := true;
  Self.Module.RequireGRUU       := true;

  Reg := Self.Module.CreateRegister(Self.Core.From, Self.Destination, TIdSipRequest.DefaultMaxForwards);
  try
    Check(Reg.HasHeader(RequireHeader),
          'REGISTER lacks a Require header');
    CheckNotEquals(ItemNotFoundIndex,
                   Reg.Require.Values.IndexOf(ExtensionGruu),
                   'Require header doesn''t demand support of GRUU');
  finally
    Reg.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.TestUnregisterFrom;
var
  OurBindings: TIdSipContacts;
begin
  OurBindings := TIdSipContacts.Create;
  try
    OurBindings.Add(Self.ActualContact);

    Self.MarkSentRequestCount;
    Self.Module.UnregisterFrom(Self.RemoteUri, Self.LastSentRequest.FirstContact).Send;
    CheckRequestSent('No REGISTER sent');
    CheckEquals(MethodRegister,
                Self.LastSentRequest.Method,
                'Unexpected sent request');

    CheckEquals(OurBindings, Self.LastSentRequest.Contacts);
  finally
    OurBindings.Free;
  end;
end;

procedure TestTIdSipOutboundRegisterModule.TestUnregisterFromMultipleBindings;
var
  OurBindings: TIdSipContacts;
begin
  OurBindings := TIdSipContacts.Create;
  try
    OurBindings.Add(Self.ActualContact);
    OurBindings.Add(ContactHeaderFull).Value := 'sip:wintermute@silver-bust.tessier-ashpool.co.luna';

    Self.MarkSentRequestCount;
    Self.Module.UnregisterFrom(Self.RemoteUri, OurBindings).Send;
    CheckRequestSent('No REGISTER sent');
    CheckEquals(MethodRegister,
                Self.LastSentRequest.Method,
                'Unexpected sent request');

    CheckEquals(OurBindings, Self.LastSentRequest.Contacts);
  finally
    OurBindings.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipRegisterModule                                                   *
//******************************************************************************
//* TestTIdSipRegisterModule Public methods ************************************

procedure TestTIdSipRegisterModule.SetUp;
begin
  inherited SetUp;

  Self.Module := Self.Core.AddModule(TIdSipRegisterModule) as TIdSipRegisterModule;
  Self.Params := TIdSipHeaderParameters.Create;

  Self.Module.BindingDB := TIdSipMockBindingDatabase.Create;
end;

procedure TestTIdSipRegisterModule.TearDown;
begin
  Self.Params.Free;
  // Core will free Self.Module.

  inherited TearDown;
end;

//* TestTIdSipRegisterModule Published methods *********************************

procedure TestTIdSipRegisterModule.TestConfigureMinTime;
begin
  Self.Params.AddParam(MinTimeParam, FortyTwoSeconds);
  Self.Module.Configure(Self.Params);

  CheckEquals(StrToInt(FortyTwoSeconds), Self.Module.MinimumExpiryTime,
              'mintime parameter ignored');
end;

procedure TestTIdSipRegisterModule.TestConfigureMinTimeBadValue;
var
  OriginalExpiryTime: Cardinal;
begin
  Self.Params.AddParam(MinTimeParam, FortyTwoSeconds);
  Self.Module.Configure(Self.Params);

  OriginalExpiryTime := Self.Module.MinimumExpiryTime;
  Self.Params[MinTimeParam] := 'foo';
  Self.Module.Configure(Self.Params);

  CheckEquals(OriginalExpiryTime, Self.Module.MinimumExpiryTime,
              'Expiry time changed even though mintime had bad value');
end;

procedure TestTIdSipRegisterModule.TestConfigureRegTime;
begin
  Self.Params.AddParam(RegTimeParam, FortyTwoSeconds);
  Self.Module.Configure(Self.Params);

  CheckEquals(StrToInt(FortyTwoSeconds), Self.Module.BindingDB.DefaultExpiryTime,
              'regtime parameter ignored');
end;

procedure TestTIdSipRegisterModule.TestConfigureRegTimeBadValue;
var
  OriginalExpiryTime: Cardinal;
begin
  Self.Params.AddParam(RegTimeParam, FortyTwoSeconds);
  Self.Module.Configure(Self.Params);

  OriginalExpiryTime := Self.Module.BindingDB.DefaultExpiryTime;
  Self.Params[RegTimeParam] := 'foo';
  Self.Module.Configure(Self.Params);

  CheckEquals(OriginalExpiryTime, Self.Module.BindingDB.DefaultExpiryTime,
              'Expiry time changed even though regtime had bad value');
end;

procedure TestTIdSipRegisterModule.TestConfigureUseGruu;
var
  I: Integer;
begin
  for I := Low(TrueBoolStrs) to High(TrueBoolStrs) do begin
    Self.Params[UseGruuParam] := TrueBoolStrs[I];
    Self.Module.Configure(Self.Params);
    Check(Self.Module.UseGruu, 'usegruu=' + TrueBoolStrs[I]);

    Self.Params[UseGruuParam] := FalseBoolStrs[I];
    Self.Module.Configure(Self.Params);
    Check(not Self.Module.UseGruu, 'usegruu=' + FalseBoolStrs[I]);
  end;
end;

procedure TestTIdSipRegisterModule.TestConfigureUseGruuBadValue;
begin
  Self.Params.AddParam(UseGruuParam, 'true');
  Self.Module.Configure(Self.Params);
  Check(Self.Module.UseGruu, 'usegruu=true');

  Self.Params[UseGruuParam] := 'ture'; // typo!
  Self.Module.Configure(Self.Params);
  Check(Self.Module.UseGruu, 'usegruu changed; bad-valued parameter not ignored');

end;

//******************************************************************************
//*  TestTIdSipRegistration                                                    *
//******************************************************************************
//*  TestTIdSipRegistration Public methods *************************************

procedure TestTIdSipRegistration.SetUp;
begin
  inherited SetUp;

  Self.RegisterModule := Self.Core.AddModule(TIdSipRegisterModule) as TIdSipRegisterModule;
  Self.RegisterModule.BindingDB := TIdSipMockBindingDatabase.Create;

  Self.Contacts := TIdSipContacts.Create;
  Self.Contacts.Add(ContactHeaderFull).Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';
end;

procedure TestTIdSipRegistration.TearDown;
begin
  // The RegisterModule will free the BindingDB.
  Self.Contacts.Free;

  inherited TearDown;
end;

//*  TestTIdSipRegistration Protected methods **********************************

procedure TestTIdSipRegistration.CheckLocalAddress(ExpectedIP: String; DestinationIP: String; Msg: String; ExpectedSentBy: String = '');
var
  Action:    TIdSipAction;
  ClassName: String;
begin
  // This test only makes sense for OUTbound actions.
  if Self.IsInboundTest then Exit;

  Self.AdjustNameResolutionForInDialogActions(DestinationIP);

  // Make sure the target name resolves to the IP address against which we wish
  // to test.
  // Some tests use Self.Destination in CreateAction, others use Self.Invite
  Self.Locator.RemoveNameRecords(Self.Destination.Address.Host);
  Self.Locator.AddA(Self.Destination.Address.Host, DestinationIP);
  Self.Locator.RemoveNameRecords(Self.Invite.FirstContact.Address.Host);
  Self.Locator.AddA(Self.Invite.FirstContact.Address.Host, DestinationIP);

  Self.MarkSentRequestCount;

  Action := Self.CreateAction;
  ClassName := Action.ClassName;

  CheckRequestSent(ClassName + ': ' + Msg + ': didn''t send request');

  // Some REGISTER requests don't have Contact headers: when you wish to query
  // your bindings, for instance. 
  if not Self.LastSentRequest.Contacts.IsEmpty then begin
    Self.Contacts.First;
    CheckEquals(Self.Contacts.CurrentContact.Address.Host,
                Self.LastSentRequest.FirstContact.Address.Host,
                ClassName + ': ' + Msg + ': Contact host overwritten');
  end;

  CheckEquals(ExpectedIP,
              Self.LastSentRequest.LastHop.SentBy,
              ClassName + ': ' + Msg + ': sent-by not set');
end;

//*  TestTIdSipRegistration Published methods **********************************

procedure TestTIdSipRegistration.TestIsRegistration;
var
  Action: TIdSipAction;
begin
  // Self.UA owns the action!
  Action := Self.CreateAction;
  Check(Action.IsRegistration,
        Action.ClassName + ' marked as a Registration');
end;

//******************************************************************************
//*  TestTIdSipInboundRegistration                                             *
//******************************************************************************
//*  TestTIdSipInboundRegistration Public methods ******************************

procedure TestTIdSipInboundRegistration.SetUp;
begin
  inherited SetUp;

  Self.Invite.Method := MethodRegister;
  Self.RegisterAction := TIdSipInboundRegistration.CreateInbound(Self.Core, Self.Invite, Self.Binding);
end;

procedure TestTIdSipInboundRegistration.TearDown;
begin
  Self.RegisterAction.Free;

  inherited TearDown;
end;

//*  TestTIdSipInboundRegistration Published methods ***************************

procedure TestTIdSipInboundRegistration.TestIsInbound;
begin
  Check(Self.RegisterAction.IsInbound,
        Self.RegisterAction.ClassName + ' not marked as inbound');
end;

procedure TestTIdSipInboundRegistration.TestIsInvite;
begin
  Check(not Self.RegisterAction.IsInvite,
        Self.RegisterAction.ClassName + ' marked as an Invite');
end;

procedure TestTIdSipInboundRegistration.TestIsOptions;
begin
  Check(not Self.RegisterAction.IsOptions,
        Self.RegisterAction.ClassName + ' marked as an Options');
end;

procedure TestTIdSipInboundRegistration.TestIsOutbound;
begin
  Check(not Self.RegisterAction.IsOutbound,
        Self.RegisterAction.ClassName + ' marked as outbound');
end;

procedure TestTIdSipInboundRegistration.TestIsOwned;
begin
  Check(not Self.RegisterAction.IsOwned,
        Self.RegisterAction.ClassName + ' not marked as being owned');
end;

procedure TestTIdSipInboundRegistration.TestIsRegistration;
begin
  Check(Self.RegisterAction.IsRegistration,
        Self.RegisterAction.ClassName + ' not marked as a Registration');
end;

procedure TestTIdSipInboundRegistration.TestIsSession;
begin
  Check(not Self.RegisterAction.IsSession,
        Self.RegisterAction.ClassName + ' marked as a Session');
end;

procedure TestTIdSipInboundRegistration.TestMethod;
begin
  CheckEquals(MethodRegister,
              Self.RegisterAction.Method,
              'Inbound registration; Method');
end;

procedure TestTIdSipInboundRegistration.TestTerminateSignalled;
begin
  Check(true, 'TIdSipInboundRegistration terminates as soon as it receives a message: you can''t attach a listener to it. Thus this test makes no sense.');
end;

//******************************************************************************
//*  TestTIdSipOutboundRegisterBase                                            *
//******************************************************************************
//*  TestTIdSipOutboundRegisterBase Public methods *****************************

procedure TestTIdSipOutboundRegisterBase.SetUp;
const
  TwoHours = 7200;
begin
  inherited SetUp;

  Self.Registrar := TIdSipRegistrar.Create;
  Self.Destination.Address.Uri := 'sip:talking-head.tessier-ashpool.co.luna';
  Self.Registrar.BindingDB := TIdSipMockBindingDatabase.Create;

  Self.Redirected := false;
  Self.Succeeded  := false;
  Self.MinExpires := TwoHours;
end;

procedure TestTIdSipOutboundRegisterBase.TearDown;
begin
  // The RegisterModule will free the BindingDB.
  Self.Registrar.Free;

  inherited TearDown;
end;

//*  TestTIdSipOutboundRegisterBase Protected methods **************************

function TestTIdSipOutboundRegisterBase.RegistrarAddress: TIdSipUri;
begin
  Result := Self.Destination.Address;
end;

//*  TestTIdSipOutboundRegisterBase Private methods ****************************

procedure TestTIdSipOutboundRegisterBase.OnFailure(Action: TIdSipAction;
                                               Response: TIdSipResponse;
                                               const Reason: String);
begin
  Self.ActionFailed := true;
end;

procedure TestTIdSipOutboundRegisterBase.OnRedirect(Action: TIdSipAction;
                                                Redirect: TIdSipResponse);
begin
  Self.Redirected := true;
end;

procedure TestTIdSipOutboundRegisterBase.OnSuccess(Action: TIdSipAction;
                                               Msg: TIdSipMessage);
begin
  Self.Succeeded := true;
end;

procedure TestTIdSipOutboundRegisterBase.ReceiveRemoteIntervalTooBrief;
var
  Response: TIdSipResponse;
begin
  Response := Self.Registrar.CreateResponse(Self.LastSentRequest,
                                            SIPIntervalTooBrief);
  try
    Response.AddHeader(MinExpiresHeader).Value := IntToStr(Self.MinExpires);

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

//*  TestTIdSipOutboundRegisterBase Published methods **************************

procedure TestTIdSipOutboundRegisterBase.TestIsOwned;
var
  Reg: TIdSipAction;
begin
  Reg := Self.CreateAction;

  Check(Reg.IsOwned,
        Reg.ClassName + ' not marked as being owned');
end;

procedure TestTIdSipOutboundRegisterBase.TestMethod;
begin
  CheckEquals(MethodRegister,
              Self.CreateAction.Method,
              'Outbound registration; Method');
end;

procedure TestTIdSipOutboundRegisterBase.TestReceiveFail;
begin
  Self.CreateAction;
  Self.ReceiveResponse(SIPInternalServerError);
  Check(Self.ActionFailed, 'Registration succeeded');
end;

procedure TestTIdSipOutboundRegisterBase.TestReceiveIntervalTooBrief;
const
  OneHour = 3600;
begin
  Self.Contacts.First;
  Self.Contacts.CurrentContact.Expires := OneHour;
  Self.CreateAction;

  Self.MarkSentRequestCount;
  Self.ReceiveRemoteIntervalTooBrief;

  CheckRequestSent('No re-request issued');
  Check(Self.LastSentRequest.HasExpiry,
        'Re-request has no expiry');
  CheckEquals(Self.MinExpires,
              Self.LastSentRequest.QuickestExpiry,
              'Re-request minimum expires');

  Self.ReceiveOk(Self.LastSentRequest);
  Check(Self.Succeeded, '(Re-)Registration failed');
end;

procedure TestTIdSipOutboundRegisterBase.TestReceiveMovedPermanently;
begin
  Self.Locator.AddAAAA('fried.neurons.org', '::1');

  Self.CreateAction;
  Self.MarkSentRequestCount;
  Self.ReceiveMovedPermanently('sip:case@fried.neurons.org');
  Check(Self.Redirected, 'OnRedirect didn''t fire');
end;

procedure TestTIdSipOutboundRegisterBase.TestReceiveOK;
var
  RegistrationCount: Integer;
begin
  Self.CreateAction;

  RegistrationCount := Self.Core.RegisterModule.RegistrationCount;

  Self.ReceiveOk(Self.LastSentRequest);
  Check(Self.Succeeded, 'Registration failed');
  Check(Self.Core.RegisterModule.RegistrationCount < RegistrationCount,
        'REGISTER action not terminated');
end;

procedure TestTIdSipOutboundRegisterBase.TestSequenceNumberIncrements;
var
  SeqNo: Cardinal;
begin
  Self.CreateAction;
  SeqNo := Self.LastSentRequest.CSeq.SequenceNo;
  Self.CreateAction;
  Check(SeqNo + 1 = Self.LastSentRequest.CSeq.SequenceNo,
        'CSeq sequence number didn''t increment');
end;

//******************************************************************************
//* TestTIdSipOutboundRegister                                                 *
//******************************************************************************
//* TestTIdSipOutboundRegister Protected methods *******************************

function TestTIdSipOutboundRegister.CreateAction: TIdSipAction;
var
  Reg: TIdSipOutboundRegister;
begin
  Reg := Self.Core.AddOutboundAction(TIdSipOutboundRegister) as TIdSipOutboundRegister;
  Reg.AddActionListener(Self);
  Reg.AddOwnedActionListener(Self);
  Reg.Bindings   := Self.Contacts;
  Reg.LocalParty := Self.Core.From;
  Reg.Registrar  := Self.RegistrarAddress;
  Reg.Send;

  Result := Reg;
end;

//* TestTIdSipOutboundRegister Published methods *******************************

procedure TestTIdSipOutboundRegister.TestReceiveIntervalTooBriefForOneContact;
const
  OneHour = 3600;
var
  RequestContacts:      TIdSipContacts;
  SecondContactExpires: Cardinal;
begin
  // We try to be tricky: One contact has a (too-brief) expires of one hour.
  // The other has an expires of three hours. The registrar accepts a minimum
  // expires of two hours. We expect the registrar to reject the request with
  // a 423 Interval Too Brief, and for the SipRegistration to re-issue the
  // request leaving the acceptable contact alone and only modifying the
  // too-short contact.

  SecondContactExpires := OneHour*3;

  Self.Contacts.First;
  Self.Contacts.CurrentContact.Expires := OneHour;
  Self.Contacts.Add(ContactHeaderFull).Value := 'sip:wintermute@talking-head-2.tessier-ashpool.co.luna;expires='
                                              + IntToStr(SecondContactExpires);
  Self.CreateAction;

  Self.MarkSentRequestCount;
  Self.ReceiveRemoteIntervalTooBrief;

  CheckRequestSent('No re-request issued');
  Check(Self.LastSentRequest.HasExpiry,
        'Re-request has no expiry');
  CheckEquals(Self.MinExpires,
              Self.LastSentRequest.QuickestExpiry,
              'Re-request minimum expires');
  RequestContacts := TIdSipContacts.Create(Self.LastSentRequest.Headers);
  try
    RequestContacts.First;
    Check(RequestContacts.HasNext,
          'No Contacts');
    Check(RequestContacts.CurrentContact.WillExpire,
          'First contact missing expires');
    CheckEquals(Self.MinExpires,
                RequestContacts.CurrentContact.Expires,
                'First (too brief) contact');
    RequestContacts.Next;
    Check(RequestContacts.HasNext, 'Too few Contacts');
    Check(RequestContacts.CurrentContact.WillExpire,
          'Second contact missing expires');
    CheckEquals(SecondContactExpires,
                RequestContacts.CurrentContact.Expires,
                'Second, acceptable, contact');
  finally
    RequestContacts.Free;
  end;

  Self.ReceiveOk(Self.LastSentRequest);
  Check(Self.Succeeded, '(Re-)Registration failed');
end;

procedure TestTIdSipOutboundRegister.TestRegister;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  CheckEquals(Self.RegistrarAddress.Uri,
              Request.RequestUri.Uri,
              'Request-URI');
  CheckEquals(MethodRegister, Request.Method, 'Method');
  CheckEquals(Self.Core.From.Address.AsString,
              Request.From.Address.AsString,
              'REGISTER''s From header doesn''t match our Address of Record');
  CheckEquals(Self.Core.From.Address.AsString,
              Request.ToHeader.Address.AsString,
              'REGISTER''s To header doesn''t match our Address of Record');
  Check(Request.Contacts.Equals(Self.Contacts),
        'Bindings');
end;

procedure TestTIdSipOutboundRegister.TestRegisterWithInstanceID;
var
  Request: TIdSipRequest;
begin
  Self.Contacts.First;
  Self.Contacts.CurrentContact.SipInstance := 'a_totally_bogus_instance_id';
  Self.Contacts.CurrentContact.Gruu := 'fake_gruu_param_value';
  Self.Contacts.CurrentContact.Address.Grid := 'fake_grid';
  Self.Contacts.CurrentContact.Address.IsGruu := true;

  Self.Contacts.Add(ContactHeaderFull).Value := '<sip:seconduri.leo-ix.net;gruu>;gruu';

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  Check(not Request.FirstContact.HasParameter(GruuParam),
        '"gruu" parameter not removed from 1st Contact');
  Check(not Request.FirstContact.Address.HasParameter(GridParam),
        '"grid" parameter not removed from 1st Contact URI');
  Check(not Request.FirstContact.Address.HasParameter(GruuParam),
        '"gruu" parameter not removed from 1st Contact URI');
  Check(Request.FirstContact.HasParameter(SipInstanceParam),
        '"+sip.instance" parameter removed from first Contact URI');

  Request.Contacts.First;
  Request.Contacts.Next;
  Check(not Request.Contacts.CurrentContact.HasParameter(GruuParam),
        '"gruu" parameter not removed from 2nd Contact');
  Check(not Request.Contacts.CurrentContact.Address.HasParameter(GridParam),
        '"grid" parameter not removed from 2nd Contact URI');
  Check(not Request.Contacts.CurrentContact.Address.HasParameter(GruuParam),
        '"gruu" parameter not removed from 2nd Contact URI');
end;

//******************************************************************************
//* TestTIdSipOutboundRegisterQuery                                            *
//******************************************************************************
//* TestTIdSipOutboundRegisterQuery Protected methods **************************

function TestTIdSipOutboundRegisterQuery.CreateAction: TIdSipAction;
var
  Reg: TIdSipOutboundRegisterQuery;
begin
  Reg := Self.Core.AddOutboundAction(TIdSipOutboundRegisterQuery) as TIdSipOutboundRegisterQuery;
  Reg.AddActionListener(Self);
  Reg.AddOwnedActionListener(Self);
  Reg.Registrar := Self.RegistrarAddress;
  Reg.Send;

  Result := Reg;
end;

//* TestTIdSipOutboundRegisterQuery Published methods **************************

procedure TestTIdSipOutboundRegisterQuery.TestFindCurrentBindings;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  Check(Request.Contacts.IsEmpty,
        'Contact headers present');
end;

//******************************************************************************
//* TestTIdSipOutboundUnregister                                               *
//******************************************************************************
//* TestTIdSipOutboundUnregister Public methods ********************************

procedure TestTIdSipOutboundUnregister.SetUp;
begin
  inherited SetUp;

  Self.Bindings := TIdSipContacts.Create;
  Self.WildCard := false;
end;

procedure TestTIdSipOutboundUnregister.TearDown;
begin
  Self.Bindings.Free;

  inherited TearDown;
end;

//* TestTIdSipOutboundUnregister Protected methods *****************************

function TestTIdSipOutboundUnregister.CreateAction: TIdSipAction;
var
  Reg: TIdSipOutboundUnregister;
begin
  Reg := Self.Core.AddOutboundAction(TIdSipOutboundUnregister) as TIdSipOutboundUnregister;

  Reg.Bindings   := Self.Bindings;
  Reg.IsWildCard := Self.WildCard;
  Reg.Registrar  := Self.RegistrarAddress;
  Reg.AddActionListener(Self);
  Reg.AddOwnedActionListener(Self);
  Reg.Send;

  Result := Reg;
end;

//* TestTIdSipOutboundUnregister Published methods *****************************

procedure TestTIdSipOutboundUnregister.TestUnregisterAll;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.WildCard := true;
  Self.CreateAction;
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  CheckEquals(Self.RegistrarAddress.Uri,
              Request.RequestUri.Uri,
              'Request-URI');
  CheckEquals(MethodRegister, Request.Method, 'Method');
  CheckEquals(1, Request.Contacts.Count,
             'Contact count');
  Check(Request.FirstContact.IsWildCard,
        'First Contact');
  CheckEquals(0, Request.QuickestExpiry,
             'Request expiry');
end;

procedure TestTIdSipOutboundUnregister.TestUnregisterSeveralContacts;
var
  Request: TIdSipRequest;
begin
  Self.MarkSentRequestCount;
  Self.Bindings.Add(ContactHeaderFull).Value := 'sip:case@fried.neurons.org';
  Self.Bindings.Add(ContactHeaderFull).Value := 'sip:wintermute@tessier-ashpool.co.luna';

  Self.CreateAction;
  CheckRequestSent('No request sent');

  Request := Self.LastSentRequest;
  CheckEquals(Self.RegistrarAddress.Uri,
              Request.RequestUri.Uri,
              'Request-URI');
  CheckEquals(MethodRegister, Request.Method, 'Method');

  Request.Contacts.First;
  Self.Bindings.First;

  while Request.Contacts.HasNext do begin
    CheckEquals(Self.Bindings.CurrentContact.Value,
                Request.Contacts.CurrentContact.Value,
                'Different Contact');

    CheckEquals(0,
                Request.Contacts.CurrentContact.Expires,
                'Expiry of ' + Request.Contacts.CurrentContact.Value);
    Request.Contacts.Next;
    Self.Bindings.Next;
  end;

  CheckEquals(Self.Bindings.Count, Request.Contacts.Count,
             'Contact count');
end;

//******************************************************************************
//* TOutboundRegistrationBaseTestCase                                          *
//******************************************************************************
//* TOutboundRegistrationBaseTestCase Public methods ***************************

procedure TOutboundRegistrationBaseTestCase.SetUp;
begin
  inherited SetUp;

  Self.Contacts.Add(ContactHeaderFull).Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';

  Self.Registrar := TIdSipRegistrar.Create;
  Self.Destination.Address.Uri := 'sip:talking-head.tessier-ashpool.co.luna';
  Self.Registrar.BindingDB := TIdSipMockBindingDatabase.Create;

  Self.Failed := false;
end;

procedure TOutboundRegistrationBaseTestCase.TearDown;
begin
  Self.Registrar.Free;

  inherited TearDown;
end;

//* TOutboundRegistrationBaseTestCase Protected methods ************************

function TOutboundRegistrationBaseTestCase.CreateRegistrationWithoutSend: TIdSipOutboundRegistrationBase;
begin
  Result := Self.Core.RegisterModule.RegisterWith(Self.RegistrarAddress, Self.Contacts);

  Result.AddActionListener(Self);
  Result.AddListener(Self);
  Result.Bindings   := Self.Contacts;
  Result.LocalParty := Self.Core.From;
  Result.Registrar  := Self.RegistrarAddress;
end;

function TOutboundRegistrationBaseTestCase.CreateAction: TIdSipAction;
begin
  Result := Self.CreateRegistrationWithoutSend;
  Result.Send;
end;

procedure TOutboundRegistrationBaseTestCase.OnFailure(RegisterAgent: TIdSipOutboundRegistrationBase;
                                                      ErrorCode: Cardinal;
                                                      const Reason: String);
begin
  Self.ErrorCode := ErrorCode;
  Self.Failed    := true;
  Self.Reason    := Reason;
end;

procedure TOutboundRegistrationBaseTestCase.OnSuccess(RegisterAgent: TIdSipOutboundRegistrationBase;
                                                      CurrentBindings: TIdSipContacts);
begin
  Self.Contacts.Clear;
  Self.Contacts.Add(CurrentBindings);
end;

function TOutboundRegistrationBaseTestCase.RegistrarAddress: TIdSipUri;
begin
  Result := Self.Destination.Address;
end;

//* TOutboundRegistrationBaseTestCase Published methods ************************

procedure TOutboundRegistrationBaseTestCase.TestAddListener;
var
  L1, L2:       TIdSipTestRegistrationListener;
  Registration: TIdSipOutboundRegistrationBase;
begin
  Registration := Self.CreateAction as TIdSipOutboundRegistrationBase;

  L1 := TIdSipTestRegistrationListener.Create;
  try
    L2 := TIdSipTestRegistrationListener.Create;
    try
      Registration.AddListener(L1);
      Registration.AddListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Success, 'L1 not informed of success');
      Check(L2.Success, 'L2 not informed of success');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TOutboundRegistrationBaseTestCase.TestCircularRedirect;
var
  Action:    TIdSipAction;
  ClassName: String;
begin
  //  ---   REGISTER (original)   --->
  // <---  302 Moved Temporarily  ---
  //  --- REGISTER (redirect #1)  --->
  // <---  302 Moved Temporarily  ---
  //  --- REGISTER (redirect #2)  --->
  // <---  302 Moved Temporarily  ---
  //  --- REGISTER (redirect #1)  ---> again!
  // <---  302 Moved Temporarily  ---

  Action := Self.CreateAction;
  ClassName := Action.ClassName;
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  Self.ReceiveMovedTemporarily('sip:bar@bar.org');

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  CheckNoRequestSent('The ' + ClassName + ' accepted the run-around');
end;

procedure TOutboundRegistrationBaseTestCase.TestDoubleRedirect;
var
  Action: TIdSipAction;
  Method: String;
begin
  //  ---   REGISTER (original)   --->
  // <---  302 Moved Temporarily  ---
  //  --- REGISTER (redirect #1)  --->
  // <---  302 Moved Temporarily  ---
  //  --- REGISTER (redirect #2)  --->
  // <---  302 Moved Temporarily  ---

  Action := Self.CreateAction;
  Method := Action.Method;
  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:foo@bar.org');
  CheckRequestSent('No redirected ' + Method + ' #1 sent: ' + Self.FailReason);
  CheckEquals('sip:foo@bar.org',
              Self.LastSentRequest.RequestUri.Uri,
              'Request-URI of redirect #1');

  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily('sip:baz@quaax.org');
  CheckRequestSent('No redirected ' + Method + ' #2 sent: ' + Self.FailReason);
  CheckEquals('sip:baz@quaax.org',
              Self.LastSentRequest.RequestUri.Uri,
              'Request-URI of redirect #2');
end;

procedure TOutboundRegistrationBaseTestCase.TestRedirectMultipleOks;
const
  FirstRegister  = 0;
  FirstRedirect  = 1;
  SecondRedirect = 2;
  ThirdRedirect  = 3;
var
  Action:           TIdSipAction;
  ClassName:        String;
  Contacts:         array of String;
  ExpectedResponse: TIdSipResponse;
begin
  //                               Request number:
  //  ---      REGISTER       ---> #0
  // <---   302 (foo,bar,baz) ---
  //  ---    REGISTER(foo)    ---> #1
  //  ---    REGISTER(bar)    ---> #2
  //  ---    REGISTER(baz)    ---> #3
  // <---      200 (bar)      ---
  // <---      200 (foo)      ---

  // In summary, we send an REGISTER. The redirect server (or whatever)
  // redirects us to foo, bar and baz. The REGISTER to bar succeeds first, so
  // we make sure that the first 200 OK is the message reported as succeeding:
  // we don't care about any further 2xx responses.

  SetLength(Contacts, 3);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';
  Contacts[2] := 'sip:baz@bar.org';

  Action := Self.CreateAction;
  ClassName := Action.ClassName;
  Self.MarkSentRequestCount;
  Self.ReceiveMovedTemporarily(Contacts);

  // ARG! Why do they make Length return an INTEGER? And WHY Abs() too?
  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts)),
              Self.SentRequestCount,
              ClassName + ' didn''t attempt to contact all Contacts: ' + Self.FailReason);

  Self.MarkSentRequestCount;
  Self.ReceiveOkFrom(Self.SentRequestAt(SecondRedirect), Contacts[1]);

  ExpectedResponse := TIdSipResponse.Create;
  try
    ExpectedResponse.Assign(Self.LastSentResponse);

    Self.ReceiveOkFrom(Self.SentRequestAt(FirstRedirect),  Contacts[0]);

    Self.Contacts.First;
    CheckEquals(Contacts[1],
                Self.Contacts.CurrentContact.Address.AsString,
                'Unexpected contact returned for first successful response');
  finally
    ExpectedResponse.Free;
  end;
end;

procedure TOutboundRegistrationBaseTestCase.TestRedirectNoMoreTargets;
var
  Action:    TIdSipAction;
  ClassName: String;
  Contacts:  array of String;
  Method:    String;
begin
  //                                           Request number:
  //  ---             REGISTER            ---> #0
  // <---          302 (foo,bar)          ---
  //  ---          REGISTER(foo)          ---> #1
  //  ---          REGISTER(bar)          ---> #2
  // <--- 302 (from foo, referencing bar) ---
  // <--- 302 (from bar, referencing foo) ---

  SetLength(Contacts, 2);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';

  Action := Self.CreateAction;
  ClassName := Action.ClassName;
  Method    := Action.Method;
  Self.ReceiveMovedTemporarily(Contacts);

  Check(Self.SentRequestCount >= 3,
        'Not enough requests sent: 1 + 2 ' + Method + 's: ' + Self.FailReason);

  Self.ReceiveMovedTemporarily(Self.SentRequestAt(1), Contacts[1]);
  Self.ReceiveMovedTemporarily(Self.SentRequestAt(2), Contacts[0]);

  Check(Self.Failed,
        ClassName + ' didn''t notify listeners of ended redirection');
  CheckEquals(RedirectWithNoMoreTargets, Self.ErrorCode,
              ClassName + ' reported wrong error code for no more redirect targets');
  CheckNotEquals('',
                 Self.Reason,
                 'Reason param not set');
end;

procedure TOutboundRegistrationBaseTestCase.TestRedirectWithMaxForwards;
const
  MaxForwards = 42;
var
  Reg: TIdSipOutboundRegistrationBase;
begin
  Reg := Self.CreateRegistrationWithoutSend;
  Reg.MaxForwards := MaxForwards;

  Self.MarkSentRequestCount;
  Reg.Send;
  CheckRequestSent('No REGISTER sent');
  CheckEquals(MaxForwards, Self.LastSentRequest.MaxForwards, 'Max-Forwards not overridden');
end;

procedure TOutboundRegistrationBaseTestCase.TestRedirectWithMultipleContacts;
var
  Action:    TIdSipAction;
  ClassName: String;
  Contacts:  array of String;
begin
  SetLength(Contacts, 2);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';

  Action := Self.CreateAction;
  ClassName := Action.ClassName;
  Self.MarkSentRequestCount;

  Self.ReceiveMovedTemporarily(Contacts);

  // ARG! Why do they make Length return an INTEGER? And WHY Abs() too?
  CheckEquals(Self.RequestCount + Cardinal(Length(Contacts)),
              Self.SentRequestCount,
              ClassName + ' didn''t attempt to contact all Contacts: ' + Self.FailReason);
end;

procedure TOutboundRegistrationBaseTestCase.TestRedirectWithNoSuccess;
var
  Action:    TIdSipAction;
  Contacts:  array of String;
  ClassName: String;
  Method:    String;
begin
  //                             Request number:
  //  ---      REGISTER     ---> #0
  // <---   302 (foo,bar)   ---
  //  ---   REGISTER (foo)  ---> #1
  //  ---   REGISTER (bar)  ---> #2
  // <---     486 (foo)     ---
  // <---     486 (bar)     ---

  SetLength(Contacts, 2);
  Contacts[0] := 'sip:foo@bar.org';
  Contacts[1] := 'sip:bar@bar.org';

  Action := Self.CreateAction;
  ClassName := Action.ClassName;
  Method    := Action.Method;
  Self.ReceiveMovedTemporarily(Contacts);

  Check(Self.SentRequestCount >= 3,
        'Not enough requests sent: 1 + 2 ' + Method + 's: ' + Self.FailReason);

  Self.ReceiveBusyHere(Self.SentRequestAt(1));
  Self.ReceiveBusyHere(Self.SentRequestAt(2));

  Check(Self.Failed,
        ClassName + ' didn''t notify listeners of ended attempt');
  CheckEquals(RedirectWithNoSuccess, Self.ErrorCode,
              ClassName + ' reported wrong error code for no successful rings');
  CheckNotEquals('',
                 Self.Reason,
                 'Reason param not set');
end;

procedure TOutboundRegistrationBaseTestCase.TestRegisterWithMaxForwards;
const
  MaxForwards = 42;
var
  Reg: TIdSipOutboundRegistrationBase;
begin
  Reg := Self.CreateRegistrationWithoutSend;
  Reg.MaxForwards := MaxForwards;

  Self.MarkSentRequestCount;
  Reg.Send;
  CheckRequestSent('No REGISTER sent');
  CheckEquals(MaxForwards, Self.LastSentRequest.MaxForwards, 'Max-Forwards not overridden');
end;

procedure TOutboundRegistrationBaseTestCase.TestRemoveListener;
var
  L1, L2:       TIdSipTestRegistrationListener;
  Registration: TIdSipOutboundRegistrationBase;
begin
  Registration := Self.CreateAction as TIdSipOutboundRegistrationBase;
  L1 := TIdSipTestRegistrationListener.Create;
  try
    L2 := TIdSipTestRegistrationListener.Create;
    try
      Registration.AddListener(L1);
      Registration.AddListener(L2);
      Registration.RemoveListener(L2);

      Self.ReceiveOk(Self.LastSentRequest);

      Check(L1.Success,
            'First listener not notified');
      Check(not L2.Success,
            'Second listener erroneously notified, ergo not removed');
    finally
      L2.Free
    end;
  finally
    L1.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOutboundRegistration                                             *
//******************************************************************************
//* TestTIdSipOutboundRegistration Private methods *****************************

procedure TestTIdSipOutboundRegistration.CheckAutoReregister(ReceiveResponse: TExpiryProc;
                                                             EventIsScheduled: Boolean;
                                                             const MsgPrefix: String);
const
  ExpiryTime = 42;
var
  EventCount:  Integer;
  LatestEvent: TIdWait;
  Registrar:   String;
begin
  Self.CreateAction;
  Registrar := Self.LastSentRequest.RequestUri.Uri;

  EventCount := DebugTimer.EventCount;
  ReceiveResponse(ExpiryTime);

  Self.DebugTimer.LockTimer;
  try
    if EventIsScheduled then begin
      Check(EventCount < Self.DebugTimer.EventCount,
            MsgPrefix + ': No timer added');

      LatestEvent := Self.DebugTimer.LastEventScheduled(TIdSipReregisterWait);

      Check(Assigned(LatestEvent),
            MsgPrefix + ': Wrong notify event');
      Check(LatestEvent.DebugWaitTime > 0,
            MsgPrefix + ': Bad wait time (' + IntToStr(LatestEvent.DebugWaitTime) + ')');

      Self.MarkSentRequestCount;
      Self.DebugTimer.TriggerAllEventsOfType(TIdSipReregisterWait);
      CheckRequestSent('No REGISTER sent');
      CheckEquals(Registrar,
                  Self.LastSentRequest.RequestUri.Uri,
                  'REGISTER re-registering to a different registrar');
    end
    else begin
      LatestEvent := Self.DebugTimer.LastEventScheduled(TIdSipReregisterWait);

      Check(not Assigned(LatestEvent),
            MsgPrefix + ': Timer erroneously added');
    end;
  finally
    Self.DebugTimer.UnlockTimer;
  end;
end;

procedure TestTIdSipOutboundRegistration.ReceiveOkWithContactExpiresOf(ExpiryTime: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Self.LastSentRequest);
  try
    Response.Contacts := Self.LastSentRequest.Contacts;
    Response.FirstContact.Expires := ExpiryTime;

    Response.AddHeader(ContactHeaderFull).Value := Response.FirstContact.AsAddressOfRecord
                                                 + '1;expires=' + IntToStr(ExpiryTime + 1);

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundRegistration.ReceiveOkWithExpiresOf(ExpiryTime: Cardinal);
var
  Response: TIdSipResponse;
begin
  Response := Self.CreateRemoteOk(Self.LastSentRequest);
  try
    Response.Contacts := Self.LastSentRequest.Contacts;
    Response.Expires.NumericValue := ExpiryTime;

    Self.ReceiveResponse(Response);
  finally
    Response.Free;
  end;
end;

procedure TestTIdSipOutboundRegistration.ReceiveOkWithNoExpires(ExpiryTime: Cardinal);
begin
  Self.ReceiveOk(Self.LastSentRequest);
end;

//* TestTIdSipOutboundRegistration Published methods ***************************

procedure TestTIdSipOutboundRegistration.TestAutoReregister;
begin
  Self.Core.RegisterModule.AutoReRegister := true;
  Self.CheckAutoReregister(Self.ReceiveOkWithExpiresOf,
                           true,
                           'Expires header');
end;

procedure TestTIdSipOutboundRegistration.TestAutoReregisterContactHasExpires;
begin
  Self.Core.RegisterModule.AutoReRegister := true;
  Self.CheckAutoReregister(Self.ReceiveOkWithContactExpiresOf,
                           true,
                           'Contact expires param');
end;

procedure TestTIdSipOutboundRegistration.TestAutoReregisterNoExpiresValue;
begin
  Self.Core.RegisterModule.AutoReRegister := true;
  Self.CheckAutoReregister(Self.ReceiveOkWithNoExpires,
                           false,
                           'No Expires header or expires param');
end;

procedure TestTIdSipOutboundRegistration.TestAutoReregisterSwitchedOff;
begin
  Self.Core.RegisterModule.AutoReRegister := false;
  Self.CheckAutoReregister(Self.ReceiveOkWithExpiresOf,
                           false,
                           'Expires header; Autoreregister = false');
end;

procedure TestTIdSipOutboundRegistration.TestFailWithMultipleListeners;
var
  L: TExceptionCatchingActionListener;
begin
  // See the comment in TestNetworkFailureWithMultipleListeners.

  L := TExceptionCatchingActionListener.Create;
  try
    Self.Core.AddListener(L);

    Self.MarkSentRequestCount;
    Self.CreateAction;
    CheckRequestSent('No REGISTER sent');
    Self.ReceiveResponse(SIPBadGateway); // The exact code doesn't matter.

    if (L.ExceptionMessage <> '') then
      Fail(Format('%s raised while removing a listener: %s', [L.ExceptionType.ClassName, L.ExceptionMessage]));
  finally
    Self.Core.RemoveListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipOutboundRegistration.TestNetworkFailureWithMultipleListeners;
var
  L: TExceptionCatchingActionListener;
begin
  // The underlying TIdSipOutboundRegister ends up having two listeners - first,
  // the TIdSipOutboundRegistration that created it, and second, this TestCase.
  // This test demonstrates that the TIdSipOutboundRegistration removes itself
  // as a listener from the TIdSipOutboundRegister by not blowing up when this
  // TestCase tries to remove itself as a listener from the
  // TIdSipOutboundRegister. This is reported as PR 566.

  L := TExceptionCatchingActionListener.Create;
  try
    Self.Core.AddListener(L);
    Self.MarkSentRequestCount;
    Self.CreateAction;
    CheckRequestSent('No REGISTER sent');
    Self.FireConnectionException(Self.LastSentRequest);

    if (L.ExceptionMessage <> '') then
      Fail(Format('%s raised while removing a listener: %s', [L.ExceptionType.ClassName, L.ExceptionMessage]));
  finally
    Self.Core.RemoveListener(L);
    L.Free;
  end;
end;

procedure TestTIdSipOutboundRegistration.TestReceiveGruu;
var
  LocalContact: TIdSipContactHeader;
  Gruu:       TIdSipContactHeader;
  OkWithGruu: TIdSipResponse;
begin
  LocalContact := TIdSipContactHeader.Create;
  try
    Self.MarkSentRequestCount;
    Self.Core.InviteModule.Call(Self.Core.From, Self.Destination, '', '').Send;
    CheckRequestSent('No INVITE sent');
    LocalContact.Assign(Self.LastSentRequest.FirstContact);

    Self.Core.UseGruu := true;
    Self.Contacts.Clear;
    Self.Contacts.Add(LocalContact);
    Self.CreateAction;

    OkWithGruu := TIdSipResponse.InResponseTo(Self.LastSentRequest, SIPOK);
    try
      OkWithGruu.Supported.Values.Add(ExtensionGruu);
      Gruu := OkWithGruu.AddHeader(ContactHeaderFull) as TIdSipContactHeader;
      Gruu.Value := Self.LastSentRequest.FirstContact.FullValue;
      Gruu.Gruu := LocalContact.Address.AsString + ';opaque=foo';

      Self.ReceiveResponse(OkWithGruu);

      CheckNotEquals(Gruu.Gruu,
                     LocalContact.Address.AsString,
                     'The OutboundRegistration action mustn''t touch the AbstractCore''s GRUU');
    finally
      OkWithGruu.Free;
    end;
  finally
    LocalContact.Free;
  end;
end;

procedure TestTIdSipOutboundRegistration.TestReceiveNoGruu;
const
  OurUrn   = 'urn:uuid:00000000-0000-0000-0000-000000000000';
var
  Gruu:       TIdSipContactHeader;
  OldContact: String;
  OkWithGruu: TIdSipResponse;
begin
  Self.UseGruu;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No REGISTER sent');

  OldContact := Self.LastSentRequest.FirstContact.AsString;

  OkWithGruu := TIdSipResponse.InResponseTo(Self.LastSentRequest, SIPOK);
  try
    OkWithGruu.Supported.Values.Add(ExtensionGruu);
    Gruu := OkWithGruu.AddHeader(ContactHeaderFull) as TIdSipContactHeader;
    Gruu.Value := Self.LastSentRequest.FirstContact.FullValue;

    Self.ReceiveResponse(OkWithGruu);
  finally
    OkWithGruu.Free;
  end;

  Self.MarkSentRequestCount;
  Self.CreateAction;
  CheckRequestSent('No REGISTER sent');

  CheckEquals(OldContact, Self.LastSentRequest.FirstContact.AsString, 'UserAgent''s Contact not protected');
end;

procedure TestTIdSipOutboundRegistration.TestRegisterUsesUAsFrom;
var
  R: TIdSipAction;
begin
  Self.MarkSentRequestCount;
  R := Self.CreateAction;
  CheckRequestSent('No REGISTER sent');

  CheckEquals(R.LocalParty.Value, Self.LastSentRequest.From.Value, 'From header');
end;

procedure TestTIdSipOutboundRegistration.TestReregisterTime;
const
  OneMinute     = 60;
  OneHour       = 60*OneMinute;
  OneDay        = 24*OneHour; // Seconds in a day
  FiveMinutes   = 5*OneMinute;
  TwentyMinutes = 20*OneMinute;
var
  Reg: TIdSipOutboundRegistration;
begin
  Reg := Self.CreateAction as TIdSipOutboundRegistration;

  CheckEquals(OneDay - FiveMinutes, Reg.ReregisterTime(OneDay), 'One day');
  CheckEquals(OneHour - FiveMinutes, Reg.ReregisterTime(OneHour), 'One hour');
  CheckEquals(TwentyMinutes - FiveMinutes,
              Reg.ReregisterTime(TwentyMinutes), '20 minutes');

  CheckEquals(FiveMinutes - OneMinute,
              Reg.ReregisterTime(FiveMinutes),
              '5 minutes');

  CheckEquals(4*30 div 5, Reg.ReregisterTime(30), '30 seconds');
  CheckEquals(1,          Reg.ReregisterTime(1), '1 second');
  CheckEquals(1,          Reg.ReregisterTime(0), 'Zero');
end;

procedure TestTIdSipOutboundRegistration.TestSuccessWithMultipleListeners;
var
  L: TExceptionCatchingActionListener;
begin
  // See the comment in TestNetworkFailureWithMultipleListeners.

  L := TExceptionCatchingActionListener.Create;
  try
    Self.Core.AddListener(L);

    Self.MarkSentRequestCount;
    Self.CreateAction;
    CheckRequestSent('No REGISTER sent');
    Self.ReceiveResponse(SIPOK);

    if (L.ExceptionMessage <> '') then
      Fail(Format('%s raised while removing a listener: %s', [L.ExceptionType.ClassName, L.ExceptionMessage]));
  finally
    Self.Core.RemoveListener(L);
    L.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipOutboundRegistrationQuery                                        *
//******************************************************************************
//* TestTIdSipOutboundRegistrationQuery Public methods *************************

function TestTIdSipOutboundRegistrationQuery.CreateRegistrationWithoutSend: TIdSipOutboundRegistrationBase;
begin
  Result := Self.Core.RegisterModule.CurrentRegistrationWith(Self.RegistrarAddress);

  Result.AddActionListener(Self);
  Result.AddListener(Self);
  Result.Bindings  := Self.Contacts;
  Result.Registrar := Self.RegistrarAddress;
end;

//******************************************************************************
//* TestRegistrationMethod                                                     *
//******************************************************************************
//* TestRegistrationMethod Public methods **************************************

procedure TestRegistrationMethod.SetUp;
var
  Registrar: TIdSipUri;
begin
  inherited SetUp;

  Self.Bindings := TIdSipContacts.Create;
  Self.Listener := TIdSipTestRegistrationListener.Create;

  Registrar := TIdSipUri.Create;
  try
    Reg := Self.UA.RegisterModule.RegisterWith(Registrar, Self.Bindings);
  finally
    Registrar.Free;
  end;
end;

procedure TestRegistrationMethod.TearDown;
begin
  Self.Listener.Free;
  Self.Bindings.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSipRegistrationFailedMethod                                         *
//******************************************************************************
//* TestTIdSipRegistrationFailedMethod Public methods **************************

procedure TestTIdSipRegistrationFailedMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipRegistrationFailedMethod.Create;
  Self.Method.ErrorCode    := SIPBadExtension;
  Self.Method.Reason       := RSSIPBadExtension;
  Self.Method.Registration := Self.Reg;
end;

procedure TestTIdSipRegistrationFailedMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipRegistrationFailedMethod Published methods ***********************

procedure TestTIdSipRegistrationFailedMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Failure, 'Listener not notified');
  CheckEquals(Self.Method.ErrorCode,
              Self.Listener.ErrorCodeParam,
              'ErrorCode param');
  CheckEquals(Self.Method.Reason,
              Self.Listener.ReasonParam,
              'Reason param');
  Check(Self.Method.Registration = Self.Listener.RegisterAgentParam,
        'RegisterAgent param');
end;

//******************************************************************************
//* TestTIdSipRegistrationSucceededMethod                                      *
//******************************************************************************
//* TestTIdSipRegistrationSucceededMethod Public methods ***********************

procedure TestTIdSipRegistrationSucceededMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSipRegistrationSucceededMethod.Create;
  Self.Method.CurrentBindings := Self.Bindings;
  Self.Method.Registration    := Self.Reg;
end;

procedure TestTIdSipRegistrationSucceededMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSipRegistrationSucceededMethod Published methods ********************

procedure TestTIdSipRegistrationSucceededMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.Success, 'Listener not notified');
  Check(Self.Method.CurrentBindings = Self.Listener.CurrentBindingsParam,
        'CurrentBindings param');
  Check(Self.Method.Registration = Self.Listener.RegisterAgentParam,
        'RegisterAgent param');
end;

//******************************************************************************
//* TestTIdSipReregisterWait                                                   *
//******************************************************************************
//* TestTIdSipReregisterWait Public methods ************************************

procedure TestTIdSipReregisterWait.SetUp;
begin
  inherited SetUp;

  Self.AOR := TIdSipFromHeader.Create;
  Self.AOR.Value := 'sip:case@fried-neurons.org';

  Self.Wait := TIdSipReregisterWait.Create;
  Self.Wait.AddressOfRecord := Self.AOR;
  Self.Wait.Bindings.Add(ContactHeaderFull).Value := 'sip:case@hilton.tr';
  Self.Wait.RegisterModuleID := Self.Core.RegisterModule.ID;
  Self.Wait.Registrar.Uri := 'sip:gw1.leo-ix.net';
end;

procedure TestTIdSipReregisterWait.TearDown;
begin
  Self.AOR.Free;
  Self.Wait.Free;

  inherited TearDown;
end;

//* TestTIdSipReregisterWait Published methods *********************************

procedure TestTIdSipReregisterWait.TestTrigger;
begin
  Self.MarkSentRequestCount;
  Self.Wait.Trigger;
  CheckRequestSent('No request sent');
  CheckEquals(MethodRegister,
              Self.LastSentRequest.Method,
              'Unexpected request sent');
  CheckEquals(Self.Wait.Registrar.Uri,
              Self.LastSentRequest.RequestUri.Uri,
              'Request-URI');
  CheckEquals(Self.Wait.AddressOfRecord.Address.AsString,
              Self.LastSentRequest.From.Address.AsString,
              'From URI');
end;

//******************************************************************************
//* TExceptionCatchingActionListener                                           *
//******************************************************************************
//* TExceptionCatchingActionListener Public methods ****************************

constructor TExceptionCatchingActionListener.Create;
begin
  inherited Create;

  Self.fExceptionType    := nil;
  Self.fExceptionMessage := '';
end;

//* TExceptionCatchingActionListener Private methods ***************************

procedure TExceptionCatchingActionListener.OnAddAction(UserAgent: TIdSipAbstractCore;
                                                       Action: TIdSipAction);
begin
  Action.AddActionListener(Self);
end;

procedure TExceptionCatchingActionListener.OnAuthenticationChallenge(Action: TIdSipAction;
                                                                     Challenge: TIdSipResponse);
begin
  // Do nothing.
end;

procedure TExceptionCatchingActionListener.OnDroppedUnmatchedMessage(UserAgent: TIdSipAbstractCore;
                                                                     Message: TIdSipMessage;
                                                                     Binding: TIdConnectionBindings);
begin
  // Do nothing.
end;

procedure TExceptionCatchingActionListener.OnNetworkFailure(Action: TIdSipAction;
                                                            ErrorCode: Cardinal;
                                                            const Reason: String);
begin
  // Do nothing.
end;

procedure TExceptionCatchingActionListener.OnRemoveAction(UserAgent: TIdSipAbstractCore;
                                                          Action: TIdSipAction);
begin
  try
    Action.RemoveActionListener(Self);
  except
    on E: EAccessViolation do begin
      Self.fExceptionType    := ExceptClass(E.ClassType);
      Self.fExceptionMessage := E.Message;
    end;
  end;
end;

procedure TExceptionCatchingActionListener.OnTerminated(Action: TIdSipAction);
begin
end;

initialization
  RegisterTest('Registrar', Suite);
end.
