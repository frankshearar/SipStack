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
  IdSipMessage, IdSipRegistration, IdSipMockBindingDatabase, TestFramework,
  TestFrameworkSip;

type
  TestTIdSipRegistrations = class(TTestCase)
  private
    Regs: TIdSipRegistrations;
    Uri:  TIdSipUri;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddKnownRegistrar;
    procedure TestCallIDFor;
    procedure TestNextSequenceNoFor;
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
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddBindings;
    procedure TestBindingsFor;
    procedure TestBindingsForClearsListParam;
    procedure TestIsAuthorized;
    procedure TestIsValid;
    procedure TestFailAddBinding;
    procedure TestFailBindingsFor;
    procedure TestFailRemoveBinding;
    procedure TestGruusFor;
    procedure TestRemoveAllBindings;
    procedure TestRemoveBinding;
    procedure TestRemoveBindingWhenNotPresent;
  end;

implementation

uses
  DateUtils, IdSipConsts, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipRegistration unit tests');
  Result.AddTest(TestTIdSipRegistrations.Suite);
  Result.AddTest(TestTIdSipAbstractBindingDatabase.Suite);
  Result.AddTest(TestTIdSipMockBindingDatabase.Suite);
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

//* TestTIdSipRegistrations Published methods **********************************

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

procedure TestTIdSipRegistrations.TestCallIDFor;
const
  CallID = '329087234@casephone.fried-neurons.org';
begin
  // Registrar not known:
  try
    Self.Regs.CallIDFor(Self.Uri);
  except
    on EIdSipRegistrarNotFound do;
  end;

  Self.Regs.AddKnownRegistrar(Self.Uri, CallID, 0);
  CheckEquals(CallID,
              Self.Regs.CallIDFor(Self.Uri),
              'Call-ID');
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

procedure TestTIdSipMockBindingDatabase.TestBindingsForClearsListParam;
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

procedure TestTIdSipMockBindingDatabase.TestGruusFor;
const
  ZeroUrn = '<urn:uuid:00000000-0000-0000-0000-000000000000>';
var
  Bindings: TIdSipContacts;
begin
  Self.CaseContact.SipInstance := ZeroUrn;
  Self.DB.AddBindings(Self.CasesAOR);
  Bindings := TIdSipContacts.Create;
  try
    Bindings.Add(Self.CaseContact);

    Self.DB.GruusFor(Self.CasesAOR.From.AsCanonicalAddress, Bindings);

    Bindings.First;
    Check(Bindings.CurrentContact.HasParam(GruuParam),
          'DB didn''t create and add a GRUU to the binding');
    CheckNotEquals('',
                   Bindings.CurrentContact.Gruu,
                   '"gruu" parameters MUST have a value');
  finally
    Bindings.Free;
  end;
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

initialization
  RegisterTest('Registration', Suite);
end.
