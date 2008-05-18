{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipRegistration;

interface

uses
  Classes, IdSipMessage, IdSipRegistration, IdSipMockBindingDatabase,
  TestFramework, TestFrameworkSip;

type
  TestFunctions = class(TTestCase)
  private
    UriA: TIdSipUri;
    UriB: TIdSipUri;

    procedure CheckMatch(MatchingFunction: TIdSipMatchingFunction; UriOne, UriTwo: String);
    procedure CheckNoMatch(MatchingFunction: TIdSipMatchingFunction; UriOne, UriTwo: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFullUriMatch;
    procedure TestUsernameMatch;
  end;

  TestTIdSipRegistrations = class(TTestCase)
  private
    Regs: TIdSipRegistrations;
    Uri:  TIdSipUri;

    procedure AsString(Contacts: TIdSipContacts; Result: TStringList);
    procedure CheckBindings(Expected: TIdSipContacts; Registrar: TIdSipUri; Msg: String);
    procedure CheckEquals(Expected, Received: TIdSipContacts; MsgPrefix: String); overload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddBindings;
    procedure TestAddBindingsDuplicateBindings;
    procedure TestAddBindingsForUnknownRegistrar;
    procedure TestAddKnownRegistrar;
    procedure TestBindingsForClearsParameter;
    procedure TestBindingsForUnknownRegistrar;
    procedure TestCallIDFor;
    procedure TestCount;
    procedure TestIsEmpty;
    procedure TestKnownRegistrar;
    procedure TestNextSequenceNoFor;
    procedure TestRegistrars;
    procedure TestRemoveBindings;
    procedure TestRemoveBindingsForUnknownRegistrar;
    procedure TestRemoveBindingsNonExtantBindings;
    procedure TestSetBindingsAndBindingsFor;
    procedure TestSetBindingsForUnknownRegistrar;
  end;

  // We test binding rules here. We test those things that could cause a
  // binding removal to fail and other such situations.
  TestTIdSipAbstractBindingDatabase = class(TTestCaseSip)
  private
    DB:      TIdSipAbstractBindingDatabase;
    Request: TIdSipRequest;

    procedure CheckExpiry(Expected: Cardinal;
                          const Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddBindingNotifiesObservers;
    procedure TestAddBindingUpdates;
    procedure TestAddBindingWithExpiryParam;
    procedure TestAddBindingWithExpiryHeader;
    procedure TestAddBindingWithNoExpiries;
    procedure TestAddBindingWithPassword;
    procedure TestAddBindingWithZeroExpiresRemovesBinding;
    procedure TestAddExistingBindingOutOfOrderSeqNo;
    procedure TestBindingExpires;
    procedure TestNotOutOfOrder;
    procedure TestNotOutOfOrderEarlySeqNo;
    procedure TestNotOutOfOrderDifferentCallID;
    procedure TestRemoveAllBindings;
    procedure TestRemoveAllBindingsNotifiesListeners;
  end;

  TestTIdSipMockBindingDatabase = class(TTestCase)
  private
    CaseContact:    TIdSipContactHeader;
    CasesAOR:       TIdSipRequest;
    DB:             TIdSipMockBindingDatabase;
    Wintermute:     TIdSipContactHeader;
    WintermutesAOR: TIdSipRequest;

    procedure SetDBForTwoWintermuteContacts;
    procedure SetDBForTwoWintermuteContactsWithGruus;
    procedure SetExpiryTime(Contacts: TIdSipContacts; ExpiryTime: Cardinal);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddBindings;
    procedure TestAddBindingsAgain;
    procedure TestBindingsFor;
    procedure TestBindingsForClearsListParam;
    procedure TestBindingsForWithGruus;
    procedure TestBindingsForUri;
    procedure TestBindingsForUriClearsListParam;
    procedure TestBindingsForUriWithGruus;
    procedure TestBindingsForWithGruusReusesCreatedGruus;
    procedure TestBindingsForWithGruusSips;
    procedure TestIsAuthorized;
    procedure TestIsValid;
    procedure TestFailAddBinding;
    procedure TestFailBindingsFor;
    procedure TestFailRemoveBinding;
    procedure TestRemoveAllBindings;
    procedure TestRemoveBinding;
    procedure TestRemoveBindingWhenNotPresent;
    procedure TestUpdateBinding;
    procedure TestUpdateBindingWithShorterExpires;
    procedure TestUpdateBindingWithZeroExpires;
  end;

  TestTIdSipNameMatchingMockBindingDatabase = class(TTestCase)
  private
    Bindings:       TIdSipContacts;
    CaseContact:    TIdSipContactHeader;
    CasesAOR:       TIdSipRequest;
    DB:             TIdSipNameMatchingMockBindingDatabase;
    Wintermute:     TIdSipContactHeader;
    WintermutesAOR: TIdSipRequest;

    procedure AddBinding(AOR, ContactUri: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBindingsForNoBindings;
    procedure TestBindingsForSingleBinding;
    procedure TestBindingsForMatchesOnlyUsername;
  end;

implementation

uses
  DateUtils, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipRegistration unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSipRegistrations.Suite);
  Result.AddTest(TestTIdSipAbstractBindingDatabase.Suite);
  Result.AddTest(TestTIdSipMockBindingDatabase.Suite);
  Result.AddTest(TestTIdSipNameMatchingMockBindingDatabase.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Public methods ***********************************************

procedure TestFunctions.SetUp;
begin
  inherited SetUp;

  Self.UriA := TIdSipUri.Create;
  Self.UriB := TIdSipUri.Create;
end;

procedure TestFunctions.TearDown;
begin
  Self.UriB.Free;
  Self.UriA.Free;

  inherited TearDown;
end;

//* TestFunctions Private methods **********************************************

procedure TestFunctions.CheckMatch(MatchingFunction: TIdSipMatchingFunction; UriOne, UriTwo: String);
begin
  Self.UriA.Uri := UriOne;
  Self.UriB.Uri := UriTwo;

  Check(MatchingFunction(Self.UriA, Self.UriB), '<' + UriOne + '> does not match <' + UriTwo + '>');
end;

procedure TestFunctions.CheckNoMatch(MatchingFunction: TIdSipMatchingFunction; UriOne, UriTwo: String);
begin
  Self.UriA.Uri := UriOne;
  Self.UriB.Uri := UriTwo;

  Check(not MatchingFunction(Self.UriA, Self.UriB), '<' + UriOne + '> matches <' + UriTwo + '>');
end;

//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestFullUriMatch;
begin
  CheckMatch(  FullUriMatch, 'sip:foo', 'sip:foo');
  CheckNoMatch(FullUriMatch, 'sip:foo', 'sip:bar@foo');
  CheckNoMatch(FullUriMatch, 'sip:foo', 'sip:foo;transport=tcp');
  CheckNoMatch(FullUriMatch, 'sip:foo', 'sip:FOO');
end;

procedure TestFunctions.TestUsernameMatch;
begin
  CheckMatch(  UsernameMatch, 'sip:foo',     'sip:foo');
  CheckMatch(  UsernameMatch, 'sip:bar@foo', 'sip:bar@foo');
  CheckNoMatch(UsernameMatch, 'sip:foo',     'sip:bar@foo');
  CheckMatch(  UsernameMatch, 'sip:foo',     'sip:foo;transport=tcp');
  CheckMatch(  UsernameMatch, 'sip:bar@foo', 'sip:bar@foo;transport=tcp');
  CheckMatch(  UsernameMatch, 'sip:bar@foo', 'sip:bar@FOO');
end;

//******************************************************************************
//* TestTIdSipRegistrations                                                    *
//******************************************************************************
//* TestTIdSipRegistrations Public methods *************************************

procedure TestTIdSipRegistrations.SetUp;
begin
  inherited SetUp;

  Self.Regs := TIdSipRegistrations.Create;
  Self.Uri  := TIdSipUri.Create('sip:registrar.tessier-ashpool.co.luna');
end;

procedure TestTIdSipRegistrations.TearDown;
begin
  Self.Uri.Free;
  Self.Regs.Free;

  inherited TearDown;
end;

//* TestTIdSipRegistrations Private methods ************************************

procedure TestTIdSipRegistrations.AsString(Contacts: TIdSipContacts; Result: TStringList);
begin
  Result.Clear;
  Contacts.First;
  while Contacts.HasNext do begin
    Result.Add(Contacts.CurrentContact.AsString);
    Contacts.Next;
  end;

  Result.Sort;
end;

procedure TestTIdSipRegistrations.CheckBindings(Expected: TIdSipContacts; Registrar: TIdSipUri; Msg: String);
var
  Actual: TIdSipContacts;
begin
  Actual := TIdSipContacts.Create;
  try
    Self.Regs.BindingsFor(Registrar, Actual);

    CheckEquals(Expected, Actual, Msg);
  finally
    Actual.Free;
  end;
end;

procedure TestTIdSipRegistrations.CheckEquals(Expected, Received: TIdSipContacts; MsgPrefix: String);
var
  ExpectedAsString: TStringList;
  I:                Integer;
  ReceivedAsString: TStringList;
begin
  ExpectedAsString := TStringList.Create;
  try
    ReceivedAsString := TStringList.Create;
    try
      Self.AsString(Expected, ExpectedAsString);
      Self.AsString(Received, ReceivedAsString);

      if (ExpectedAsString.Count < ReceivedAsString.Count) then
        Fail('Too few Contacts')
      else if (ExpectedAsString.Count > ReceivedAsString.Count) then
        Fail('Too many Contacts');

      for I := 0 to ExpectedAsString.Count - 1 do
        CheckEquals(ExpectedAsString[I], ReceivedAsString[I], Format('Contact #%d differs', [I + 1]));
    finally
      ReceivedAsString.Free;
    end;
  finally
    ExpectedAsString.Free;
  end;
end;

//* TestTIdSipRegistrations Published methods **********************************

procedure TestTIdSipRegistrations.TestAddBindings;
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);

    Bindings.Add(ContactHeaderFull).Value := 'sip:foo@example.com';
    Bindings.Add(ContactHeaderFull).Value := 'sip:bar@example.com';

    Self.Regs.AddBindings(Self.Uri, Bindings);
    CheckBindings(Bindings, Self.Uri, 'AddBindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipRegistrations.TestAddBindingsDuplicateBindings;
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);

    Bindings.Add(ContactHeaderFull).Value := 'sip:foo@example.com';
    Bindings.Add(ContactHeaderFull).Value := 'sip:bar@example.com';

    Self.Regs.AddBindings(Self.Uri, Bindings);
    Self.Regs.AddBindings(Self.Uri, Bindings);
    CheckBindings(Bindings, Self.Uri, 'Second AddBindings added duplicate bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipRegistrations.TestAddBindingsForUnknownRegistrar;
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    try
      Self.Regs.AddBindings(Self.Uri, Bindings);
      Fail('Failed to bail out on AddBindings for unknown registrar');
    except
      on EIdSipRegistrarNotFound do;
    end;
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipRegistrations.TestAddKnownRegistrar;
begin
  try
    Self.Regs.CallIDFor(Self.Uri);
  except
    on EIdSipRegistrarNotFound do;
  end;

  Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);

  Self.Regs.CallIDFor(Self.Uri);
end;

procedure TestTIdSipRegistrations.TestBindingsForClearsParameter;
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);

    Bindings.Add(ContactHeaderFull).Value := 'sip:example.com';
    Self.Regs.BindingsFor(Self.Uri, Bindings);

    Check(Bindings.IsEmpty, 'Bindings not cleared');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipRegistrations.TestBindingsForUnknownRegistrar;
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    try
      Self.Regs.BindingsFor(Self.Uri, Bindings);
      Fail('Failed to bail out on BindingsFor for unknown registrar');
    except
      on EIdSipRegistrarNotFound do;
    end;
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipRegistrations.TestCallIDFor;
const
  CallID = '329087234@casephone.fried-neurons.org';
begin
  // Registrar not known:
  try
    Self.Regs.CallIDFor(Self.Uri);
      Fail('Failed to bail out on CallIDFor for unknown registrar');
  except
    on EIdSipRegistrarNotFound do;
  end;

  Self.Regs.AddKnownRegistrar(Self.Uri, CallID, 0);
  CheckEquals(CallID,
              Self.Regs.CallIDFor(Self.Uri),
              'Call-ID');
end;

procedure TestTIdSipRegistrations.TestCount;
begin
  CheckEquals(0, Self.Regs.Count, 'New Registrations');

  Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);
  CheckEquals(1, Self.Regs.Count, 'After AddKnownRegistrar');

  Self.Uri.Uri := 'sip:another-registrar.tessier-ashpool.co.luna';
  Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);
  CheckEquals(2, Self.Regs.Count, 'After another AddKnownRegistrar');
end;

procedure TestTIdSipRegistrations.TestIsEmpty;
begin
  Check(Self.Regs.IsEmpty, 'New Registrations');

  Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);

  Check(not Self.Regs.IsEmpty, 'After AddKnownRegistrar');
end;

procedure TestTIdSipRegistrations.TestKnownRegistrar;
begin
  Check(not Self.Regs.KnownRegistrar(Self.Uri), 'Empty Registrations');

  Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);

  Check(Self.Regs.KnownRegistrar(Self.Uri), 'Registrar wasn''t added?');
end;

procedure TestTIdSipRegistrations.TestNextSequenceNoFor;
const
  SequenceNo = $decafbad;
var
  I: Cardinal;
begin
  // Registrar not known:
  try
    Self.Regs.NextSequenceNoFor(Self.Uri);
  except
    on EIdSipRegistrarNotFound do;
  end;

  Self.Regs.AddKnownRegistrar(Self.Uri, '', SequenceNo);

  for I := 0 to 9 do
  CheckEquals(IntToHex(SequenceNo + I, 8),
              IntToHex(Self.Regs.NextSequenceNoFor(Self.Uri), 8),
              'Next sequence number #' + IntToStr(I + 1));
end;

procedure TestTIdSipRegistrations.TestRegistrars;
var
  OtherRegistrar: TIdSipUri;
begin
  OtherRegistrar := TIdSipUri.Create('sip:example.com');
  try
    Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);
    Self.Regs.AddKnownRegistrar(OtherRegistrar, '', 0);

    CheckEquals(Self.Uri.AsString,       Self.Regs[0].AsString, 'First registrar URI');
    CheckEquals(OtherRegistrar.AsString, Self.Regs[1].AsString, 'Second registrar URI');
  finally
    OtherRegistrar.Free;
  end;
end;

procedure TestTIdSipRegistrations.TestRemoveBindings;
const
  UriOne = 'sip:bob@cubicle10a.biloxi.com';
  UriTwo = 'sip:bob@freephone.example.com';
var
  NewBindings:        TIdSipContacts;
  OldBindings:        TIdSipContacts;
  RegisteredBindings: TIdSipContacts;
begin
  Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);

  NewBindings := TIdSipContacts.Create;
  try
    OldBindings := TIdSipContacts.Create;
    try
      RegisteredBindings := TIdSipContacts.Create;
      try
        RegisteredBindings.Add(ContactHeaderFull).Value := UriOne;
        RegisteredBindings.Add(ContactHeaderFull).Value := UriTwo;

        Self.Regs.SetBindings(Self.Uri, RegisteredBindings);

        OldBindings.Add(ContactHeaderFull).Value := UriOne;

        Self.Regs.RemoveBindings(Self.Uri, OldBindings);

        // Remove the first Contact
        RegisteredBindings.First;
        RegisteredBindings.Remove(RegisteredBindings.CurrentContact);

        Self.Regs.BindingsFor(Self.Uri, NewBindings);
        CheckEquals(RegisteredBindings, NewBindings, 'Bindings not removed');
      finally
        RegisteredBindings.Free;
      end;
    finally
      OldBindings.Free;
    end;
  finally
    NewBindings.Free;
  end;
end;

procedure TestTIdSipRegistrations.TestRemoveBindingsForUnknownRegistrar;
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    try
      Self.Regs.RemoveBindings(Self.Uri, Bindings);
      Fail('Failed to bail out on RemoveBindings for unknown registrar');
    except
      on EIdSipRegistrarNotFound do;
    end;
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipRegistrations.TestRemoveBindingsNonExtantBindings;
const
  UnknownUri = 'sip:case@unit55.jakes.example.com';
  UriOne     = 'sip:bob@cubicle10a.biloxi.com';
  UriTwo     = 'sip:bob@freephone.example.com';
var
  NewBindings:        TIdSipContacts;
  OldBindings:        TIdSipContacts;
  UnknownBindings: TIdSipContacts;
begin
  Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);

  NewBindings := TIdSipContacts.Create;
  try
    OldBindings := TIdSipContacts.Create;
    try
      UnknownBindings := TIdSipContacts.Create;
      try
        OldBindings.Add(ContactHeaderFull).Value := UriOne;
        OldBindings.Add(ContactHeaderFull).Value := UriTwo;
        UnknownBindings.Add(ContactHeaderFull).Value := UnknownUri;

        Self.Regs.SetBindings(Self.Uri, OldBindings);

        Self.Regs.RemoveBindings(Self.Uri, UnknownBindings);

        Self.Regs.BindingsFor(Self.Uri, NewBindings);
        CheckEquals(OldBindings, NewBindings, 'Unknown Bindings not removed');
      finally
        UnknownBindings.Free;
      end;
    finally
      OldBindings.Free;
    end;
  finally
    NewBindings.Free;
  end;
end;

procedure TestTIdSipRegistrations.TestSetBindingsAndBindingsFor;
var
  ActualBindings:   TIdSipContacts;
  ExpectedBindings: TIdSipContacts;
begin
  ActualBindings := TIdSipContacts.Create;
  try
    ExpectedBindings := TIdSipContacts.Create;
    try
      Self.Regs.AddKnownRegistrar(Self.Uri, '', 0);

      Self.Regs.SetBindings(Self.Uri, ExpectedBindings);
      Self.Regs.BindingsFor(Self.Uri, ActualBindings);
      CheckEquals(ExpectedBindings, ActualBindings, 'Empty Contacts');

      ExpectedBindings.Add(ContactHeaderFull).Value := Self.Uri.AsString;
      Self.Regs.SetBindings(Self.Uri, ExpectedBindings);
      Self.Regs.BindingsFor(Self.Uri, ActualBindings);
      CheckEquals(ExpectedBindings, ActualBindings, 'Single Contact');

      ExpectedBindings.Add(ContactHeaderFull).Value := 'sip:example.com';
      Self.Regs.SetBindings(Self.Uri, ExpectedBindings);
      Self.Regs.BindingsFor(Self.Uri, ActualBindings);
      CheckEquals(ExpectedBindings, ActualBindings, 'Multiple Contacts');
    finally
      ExpectedBindings.Free;
    end;
  finally
    ActualBindings.Free;
  end;
end;

procedure TestTIdSipRegistrations.TestSetBindingsForUnknownRegistrar;
var
  Bindings: TIdSipContacts;
begin
  Bindings := TIdSipContacts.Create;
  try
    try
      Self.Regs.SetBindings(Self.Uri, Bindings);
      Fail('Failed to bail out on SetBindings for unknown registrar');
    except
      on EIdSipRegistrarNotFound do;
    end;
  finally
    Bindings.Free;
  end;
end;

//******************************************************************************
//* TestTIdSipAbstractBindingDatabase                                          *
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

procedure TestTIdSipAbstractBindingDatabase.TestAddBindingNotifiesObservers;
var
  L1, L2: TIdSipTestObserver;
begin
  L1 := TIdSipTestObserver.Create;
  try
    L2 := TIdSipTestObserver.Create;
    try
      Self.DB.AddObserver(L1);
      Self.DB.AddObserver(L2);
      Self.DB.AddBindings(Self.Request);

      Check(L1.Changed, 'First listener not notified');
      Check(L2.Changed, 'Second listener not notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

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

procedure TestTIdSipAbstractBindingDatabase.TestAddBindingWithPassword;
var
  Contacts: TIdSipContacts;
begin
  Self.Request.ToHeader.Address.Password := 'foo';
  Self.DB.AddBindings(Self.Request);

  Contacts := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.Request, Contacts);
    Check(not Contacts.IsEmpty, 'Bindings not added, or not collected');
  finally
    Contacts.Free;
  end;
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

procedure TestTIdSipAbstractBindingDatabase.TestBindingExpires;
var
  ExpireTime: Cardinal;
begin
  ExpireTime := 600; // ten minutes
  Self.Request.FirstContact.Expires := ExpireTime;
  Self.DB.AddBindings(Self.Request);

  // We compare two floats below. One second difference lies within the
  // realm of acceptable.
  CheckEquals(Now + OneSecond*ExpireTime,
              Self.DB.BindingExpires(Self.Request.AddressOfRecord,
                                     Self.Request.FirstContact.AsAddressOfRecord),
              OneSecond,
              'Expires time');
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

procedure TestTIdSipAbstractBindingDatabase.TestRemoveAllBindingsNotifiesListeners;
var
  L1, L2: TIdSipTestObserver;
begin
  Self.DB.AddBindings(Self.Request);

  Self.Request.CSeq.Increment;
  Self.Request.FirstContact.Expires := 0;
  Self.Request.FirstContact.IsWildCard := true;

  L1 := TIdSipTestObserver.Create;
  try
    L2 := TIdSipTestObserver.Create;
    try
      Self.DB.AddObserver(L1);
      Self.DB.AddObserver(L2);

      Check(Self.DB.RemoveAllBindings(Self.Request),
            'Notification of failure');

      Check(L1.Changed, 'First listener not notified');
      Check(L2.Changed, 'Second listener not notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
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
  Self.CaseContact.Value := 'Case <sip:terminal1>';

  Self.WintermutesAOR := TIdSipRequest.Create;
  Self.WintermutesAOR.Method         := MethodRegister;
  Self.WintermutesAOR.ToHeader.Value := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.WintermutesAOR.RequestUri.Uri := 'sip:tessier-ashpool.co.luna';
  Self.Wintermute := Self.WintermutesAOR.AddHeader(ContactHeaderFull) as TIdSipContactHeader;
  Self.Wintermute.Value := 'Wintermute <sip:talking-head.tessier-ashpool.co.luna>';
end;

procedure TestTIdSipMockBindingDatabase.TearDown;
begin
  Self.WintermutesAOR.Free;
  Self.CasesAOR.Free;
  Self.DB.Free;

  inherited TearDown;
end;

//* TestTIdSipMockBindingDatabase Private methods ******************************

procedure TestTIdSipMockBindingDatabase.SetDBForTwoWintermuteContacts;
begin
  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.Wintermute.Value := 'Wintermute <sip:wintermute@talking-head.tessier-ashpool.co.luna>';
  Self.DB.AddBindings(Self.WintermutesAOR);
end;

procedure TestTIdSipMockBindingDatabase.SetDBForTwoWintermuteContactsWithGruus;
begin
  Self.DB.UseGruu := true;
  Self.WintermutesAOR.Supported.Values.Add(ExtensionGruu);

  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.Wintermute.Value := 'Wintermute <sip:wintermute@talking-head.tessier-ashpool.co.luna>';
  Self.DB.AddBindings(Self.WintermutesAOR);

  Self.DB.AddBindings(Self.CasesAOR);
end;

procedure TestTIdSipMockBindingDatabase.SetExpiryTime(Contacts: TIdSipContacts; ExpiryTime: Cardinal);
begin
  Contacts.First;

  while Contacts.HasNext do begin
    Contacts.CurrentContact.Expires := ExpiryTime;
    Contacts.Next;
  end;
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

procedure TestTIdSipMockBindingDatabase.TestAddBindingsAgain;
var
  OriginalCount: Integer;
begin
  Self.WintermutesAOR.AddHeader(Self.CaseContact);
  Self.DB.AddBindings(Self.WintermutesAOR);

  OriginalCount := Self.DB.BindingCount;

  Self.WintermutesAOR.CSeq.Increment;
  Self.DB.AddBindings(Self.WintermutesAOR);

  CheckEquals(OriginalCount, Self.DB.BindingCount, 'Old bindings re-added');
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsFor;
var
  Bindings: TIdSipContacts;
begin
  Self.SetDBForTwoWintermuteContacts;

  Self.DB.AddBindings(Self.CasesAOR);

  Bindings := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.WintermutesAOR, Bindings);

    CheckEquals(2, Bindings.Count, 'Wrong number of bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsForClearsListParam;
var
  Bindings: TIdSipContacts;
begin
  Self.SetDBForTwoWintermuteContacts;

  Bindings := TIdSipContacts.Create;
  try
    Bindings.Add(Self.Wintermute);

    Self.DB.BindingsFor(Self.CasesAOR, Bindings);
    CheckEquals(0, Bindings.Count, 'Wrong number of bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsForWithGruus;
var
  Bindings: TIdSipContacts;
  Gruu:     TIdSipUri;
begin
  Self.SetDBForTwoWintermuteContactsWithGruus;

  Gruu := TIdSipUri.Create('');
  try
    Bindings := TIdSipContacts.Create;
    try
      Self.DB.BindingsFor(Self.WintermutesAOR, Bindings);

      CheckEquals(2, Bindings.Count, 'Wrong number of bindings');

      Bindings.First;
      while Bindings.HasNext do begin
        Check(Bindings.CurrentContact.HasParameter(GruuParam),
              'No "gruu" parameter: ' + Bindings.CurrentContact.FullValue);
        CheckNotEquals('',
                       Bindings.CurrentContact.Gruu,
                      'Empty "gruu" parameter: ' + Bindings.CurrentContact.FullValue);
        Gruu.Uri := Bindings.CurrentContact.Gruu;
        Check(not Gruu.IsMalformed,
              'Malformed "gruu" parameter: ' + Bindings.CurrentContact.Gruu);

        Bindings.Next;
      end;
    finally
      Bindings.Free;
    end;
  finally
    Gruu.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsForUri;
var
  Bindings: TIdSipContacts;
begin
  Self.SetDBForTwoWintermuteContacts;

  Self.DB.AddBindings(Self.CasesAOR);

  Bindings := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.WintermutesAOR.ToHeader.Address, Bindings, false);

    CheckEquals(2, Bindings.Count, 'Wrong number of bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsForUriClearsListParam;
var
  Bindings: TIdSipContacts;
begin
  Self.SetDBForTwoWintermuteContacts;

  Bindings := TIdSipContacts.Create;
  try
    Bindings.Add(Self.Wintermute);

    Self.DB.BindingsFor(Self.CasesAOR.ToHeader.Address, Bindings, false);
    CheckEquals(0, Bindings.Count, 'Wrong number of bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsForUriWithGruus;
var
  Bindings: TIdSipContacts;
  Gruu:     TIdSipUri;
begin
  Self.SetDBForTwoWintermuteContactsWithGruus;

  Gruu := TIdSipUri.Create('');
  try
    Bindings := TIdSipContacts.Create;
    try
      Self.DB.BindingsFor(Self.WintermutesAOR.ToHeader.Address, Bindings, true);

      CheckEquals(2, Bindings.Count, 'Wrong number of bindings');

      Bindings.First;
      while Bindings.HasNext do begin
        Check(Bindings.CurrentContact.HasParameter(GruuParam),
              'No "gruu" parameter: ' + Bindings.CurrentContact.FullValue);
        CheckNotEquals('',
                       Bindings.CurrentContact.Gruu,
                      'Empty "gruu" parameter: ' + Bindings.CurrentContact.FullValue);
        Gruu.Uri := Bindings.CurrentContact.Gruu;
        Check(not Gruu.IsMalformed,
              'Malformed "gruu" parameter: ' + Bindings.CurrentContact.Gruu);

        Bindings.Next;
      end;
    finally
      Bindings.Free;
    end;
  finally
    Gruu.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsForWithGruusReusesCreatedGruus;
var
  Bindings: TIdSipContacts;
  Gruu:     String;
begin
  Self.DB.UseGruu := true;
  Self.WintermutesAOR.Supported.Values.Add(ExtensionGruu);

  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.Wintermute.Value := 'Wintermute <sip:wintermute@talking-head.tessier-ashpool.co.luna>';
  Self.DB.AddBindings(Self.WintermutesAOR);

  Self.DB.AddBindings(Self.CasesAOR);

  Bindings := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.WintermutesAOR, Bindings);

    Check(not Bindings.IsEmpty, 'BindingsFor returned no bindings');

    Bindings.First;
    Gruu := Bindings.CurrentContact.Gruu;

    Bindings.Clear;
    Self.DB.BindingsFor(Self.WintermutesAOR, Bindings);

    Check(not Bindings.IsEmpty, '2nd BindingsFor returned no bindings');
    Bindings.First;
    CheckEquals(Gruu, Bindings.CurrentContact.Gruu, 'DB created an all-new GRUU');

  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsForWithGruusSips;
var
  Bindings: TIdSipContacts;
begin
  // If the REGISTER used a SIPS URI in the To header, then all the GRUUs
  // returned must also be SIPS URIs.

  Self.DB.UseGruu := true;
  Self.WintermutesAOR.Supported.Values.Add(ExtensionGruu);
  Self.WintermutesAOR.ToHeader.Address.Scheme := SipsScheme;
  Self.WintermutesAOR.FirstContact.Address.Scheme := SipsScheme;

  Self.DB.AddBindings(Self.WintermutesAOR);
  Self.Wintermute.Value := 'Wintermute <sips:wintermute@talking-head.tessier-ashpool.co.luna>';
  Self.DB.AddBindings(Self.WintermutesAOR);

  Bindings := TIdSipContacts.Create;
  try
    Self.DB.BindingsFor(Self.WintermutesAOR, Bindings);

    Check(not Bindings.IsEmpty, 'BindingsFor returned no bindings');

    Bindings.First;
    while Bindings.HasNext do begin
      Check(Bindings.CurrentContact.Address.IsSipsUri,
            'Not a SIPS URI: ' + Bindings.CurrentContact.FullValue);

      Bindings.Next;
    end;
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestIsAuthorized;
begin
  Self.DB.Authorized := true;
  Check(Self.DB.IsAuthorized(Self.CaseContact,
                             Self.CasesAOR.RequestUri),
        'Authorized');
  Self.DB.Authorized := false;
  Check(not Self.DB.IsAuthorized(Self.CaseContact,
                                 Self.CasesAOR.RequestUri),
        'not Authorized');
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
    Self.CaseContact.Address.Host := 'case@a111.public.booth.org';
    Self.DB.AddBindings(Self.CasesAOR);
    Self.Wintermute.Address.Host := 'silver-head.tessier-ashpool.co.luna';
    Self.DB.AddBindings(Self.WintermutesAOR);

    Bindings := TIdSipContacts.Create;
    try
      Self.DB.BindingsFor(Self.WintermutesAOR, Bindings);
      CheckEquals(2,
                  Bindings.Count,
                  'Sanity check: Wintermute should have two bindings');

      Self.DB.BindingsFor(Self.CasesAOR, Bindings);
      CheckEquals(2,
                  Bindings.Count,
                  'Sanity check: Case should have two bindings');

      Self.DB.RemoveBinding(Self.WintermutesAOR, ToBeDeleted);

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

procedure TestTIdSipMockBindingDatabase.TestUpdateBinding;
var
  Expected:  TDateTime;
  LongTime:  Cardinal;
  Received:  TDateTime;
  ShortTime: Cardinal;
begin
  // Given a binding due to expire at time T, if the registrar receives a
  // REGISTER with a Contact that will expire at time T + t (t > 0), then
  // let the binding be valid until time T + t.
  ShortTime := Self.DB.DefaultExpiryTime;
  LongTime  := ShortTime * 2;

  Self.SetExpiryTime(Self.WintermutesAOR.Contacts, ShortTime);
  Self.DB.AddBindings(Self.WintermutesAOR);

  Self.WintermutesAOR.CSeq.Increment;
  Self.SetExpiryTime(Self.WintermutesAOR.Contacts, LongTime);
  Self.DB.AddBindings(Self.WintermutesAOR);

  Expected := LongTime*OneSecond;
  Received := Self.DB.BindingExpires(Self.WintermutesAOR.AddressOfRecord,
                                     Self.Wintermute.Address.CanonicaliseAsAddress) - Now;
  Check(Abs(Expected - Received) < OneSecond,
        Format('Bindings'' expiry time not updated; expected %s left but was %s',
               [FormatDateTime('nn:ss', Expected), FormatDateTime('nn:ss', Received)]));
end;

procedure TestTIdSipMockBindingDatabase.TestUpdateBindingWithShorterExpires;
var
  Expected:  TDateTime;
  LongTime:  Cardinal;
  Received:  TDateTime;
  ShortTime: Cardinal;
begin
  // Updating the binding should not shorten the registration period, but only
  // extend it.

  ShortTime := Self.DB.DefaultExpiryTime;
  LongTime  := ShortTime * 2;

  Self.SetExpiryTime(Self.WintermutesAOR.Contacts, LongTime);
  Self.DB.AddBindings(Self.WintermutesAOR);

  Self.WintermutesAOR.CSeq.Increment;
  Self.SetExpiryTime(Self.WintermutesAOR.Contacts, ShortTime);
  Self.DB.AddBindings(Self.WintermutesAOR);

  Expected := LongTime*OneSecond;
  Received := Self.DB.BindingExpires(Self.WintermutesAOR.AddressOfRecord,
                                     Self.Wintermute.Address.CanonicaliseAsAddress) - Now;
  Check(Abs(Expected - Received) < OneSecond,
        Format('Bindings'' expiry time not updated; expected %s left but was %s',
               [FormatDateTime('nn:ss', Expected), FormatDateTime('nn:ss', Received)]));
end;

procedure TestTIdSipMockBindingDatabase.TestUpdateBindingWithZeroExpires;
begin
  // You should really use RemoveBindings (to reveal your intentions more
  // clearly), but updating a binding with a zero expires will also remove
  // bindings.

  Self.DB.AddBindings(Self.WintermutesAOR);

  Self.WintermutesAOR.CSeq.Increment;
  Self.SetExpiryTime(Self.WintermutesAOR.Contacts, 0);
  Self.DB.AddBindings(Self.WintermutesAOR);
  
  CheckEquals(0, Self.DB.BindingCount, 'Bindings not removed');
end;

//******************************************************************************
//* TestTIdSipNameMatchingMockBindingDatabase                                  *
//******************************************************************************
//* TestTIdSipNameMatchingMockBindingDatabase Public methods *******************

procedure TestTIdSipNameMatchingMockBindingDatabase.SetUp;
begin
  inherited SetUp;

  Self.DB := TIdSipNameMatchingMockBindingDatabase.Create;
  Self.Bindings := TIdSipContacts.Create;

  Self.CasesAOR := TIdSipRequest.Create;
  Self.CasesAOR.Method         := MethodRegister;
  Self.CasesAOR.ToHeader.Value := 'sip:case@fried.neurons.org';
  Self.CasesAOR.RequestUri.Uri := 'sip:fried.neurons.org';
  Self.CaseContact := Self.CasesAOR.AddHeader(ContactHeaderFull) as TIdSipContactHeader;
  Self.CaseContact.Value := 'Case <sip:terminal1>';

  Self.WintermutesAOR := TIdSipRequest.Create;
  Self.WintermutesAOR.Method         := MethodRegister;
  Self.WintermutesAOR.ToHeader.Value := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.WintermutesAOR.RequestUri.Uri := 'sip:tessier-ashpool.co.luna';
  Self.Wintermute := Self.WintermutesAOR.AddHeader(ContactHeaderFull) as TIdSipContactHeader;
  Self.Wintermute.Value := 'Wintermute <sip:talking-head.tessier-ashpool.co.luna>';
end;

procedure TestTIdSipNameMatchingMockBindingDatabase.TearDown;
begin
  Self.WintermutesAOR.Free;
  Self.CasesAOR.Free;
  Self.Bindings.Free;
  Self.DB.Free;

  inherited TearDown;
end;

//* TestTIdSipNameMatchingMockBindingDatabase Private methods ******************

procedure TestTIdSipNameMatchingMockBindingDatabase.AddBinding(AOR, ContactUri: String);
begin
  Self.WintermutesAOR.ToHeader.Value     := AOR;
  Self.WintermutesAOR.FirstContact.Value := ContactUri;
  Self.DB.AddBindings(Self.WintermutesAOR);
end;

//* TestTIdSipNameMatchingMockBindingDatabase Published methods ****************

procedure TestTIdSipNameMatchingMockBindingDatabase.TestBindingsForNoBindings;
begin
  Self.DB.BindingsFor(Self.WintermutesAOR, Self.Bindings);
  Check(Bindings.IsEmpty, 'No bindings should be returned for an empty database');
end;

procedure TestTIdSipNameMatchingMockBindingDatabase.TestBindingsForSingleBinding;
const
  AOR     = 'sip:wintermute@tessier-ashpool.co.luna';
  Binding = 'sip:talking-head.tessier-ashpool.co.luna';
begin
  Self.AddBinding(AOR, Binding);
  Self.DB.BindingsFor(Self.WintermutesAOR, Self.Bindings);
  Check(not Self.Bindings.IsEmpty, 'No bindings returned');

  Self.Bindings.First;
  CheckEquals(Binding, Self.Bindings.CurrentContact.Value, 'Wrong binding returned');
end;

procedure TestTIdSipNameMatchingMockBindingDatabase.TestBindingsForMatchesOnlyUsername;
const
  ContactUri     = 'sip:wintermute@tessier-ashpool.co.luna';
  MatchingAOR    = 'sip:wintermute@talking-head.tessier-ashpool.co.luna';
  NonMatchingAOR = 'sip:talking-head.tessier-ashpool.co.luna';
begin
  Self.AddBinding(MatchingAOR,    ContactUri);
  Self.AddBinding(NonMatchingAOR, ContactUri);

  Self.WintermutesAOR.ToHeader.Value := MatchingAOR;
  Self.DB.BindingsFor(Self.WintermutesAOR, Self.Bindings);
  Check(not Self.Bindings.IsEmpty, 'No bindings returned');

  Self.Bindings.First;
  CheckEquals(ContactUri, Self.Bindings.CurrentContact.Value, 'Correct binding not returned');

  CheckEquals(1, Self.Bindings.Count, 'Too many bindings returned: didn''t match _solely_ on username');
end;

initialization
  RegisterTest('Registration', Suite);
end.
