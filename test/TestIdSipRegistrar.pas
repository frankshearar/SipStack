unit TestIdSipRegistrar;

interface

uses
  Classes, IdSipConsts, IdSipHeaders, IdSipMessage,
  IdSipMockTransactionDispatcher, IdSipRegistrar, TestFramework;

type
  TIdSipMockBindingDatabase = class;

  TestTIdSipRegistrar = class(TTestCase)
  private
    DB:        TIdSipMockBindingDatabase;
    Dispatch:  TIdSipMockTransactionDispatcher;
    Registrar: TIdSipRegistrar;
    Request:   TIdSipRequest;

    procedure SimulateRemoteRequest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReceiveInvite;
    procedure TestReceiveRegister;
    procedure TestReceiveExpireTooShort;
    procedure TestReceiveExpireParamTooShort;
  end;

  TIdSipMockBindingDatabase = class(TIdSipAbstractBindingDatabase)
  private
    BindingStore: TStrings;

    procedure DeleteBinding(Index: Integer);
    function  GetBindings(Index: Integer): TIdSipContactHeader;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddBinding(const AddressOfRecord: String;
                         Binding: TIdSipContactHeader); override;
    function  BindingCount: Integer;
    procedure BindingsFor(const AddressOfRecord: String;
                          Bindings: TIdSipHeaders); override;
    procedure RemoveAllBindings(const AddressOfRecord: String); override;
    procedure RemoveBinding(const AddressOfRecord: String;
                            Binding: TIdSipContactHeader); override;
    property Bindings[Index: Integer]: TIdSipContactHeader read GetBindings;
  end;

  TestTIdSipMockBindingDatabase = class(TTestCase)
  private
    CaseContact:    TIdSipContactHeader;
    CasesAOR:       String;
    DB:             TIdSipMockBindingDatabase;
    Wintermute:     TIdSipContactHeader;
    WintermutesAOR: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddBinding;
    procedure TestBindingsFor;
    procedure TestBindingsForClearsList;
    procedure TestRemoveAllBindings;
    procedure TestRemoveBinding;
  end;

implementation

uses
  SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipRegistrar unit tests');
  Result.AddTest(TestTIdSipRegistrar.Suite);
  Result.AddTest(TestTIdSipMockBindingDatabase.Suite);
end;

//******************************************************************************
//* TestTIdSipRegistrar                                                        *
//******************************************************************************
//* TestTIdSipRegistrar Public methods *****************************************

procedure TestTIdSipRegistrar.SetUp;
begin
  inherited SetUp;

  Self.DB := TIdSipMockBindingDatabase.Create;
  Self.Dispatch := TIdSipMockTransactionDispatcher.Create;
  Self.Registrar := TIdSipRegistrar.Create;
  Self.Registrar.BindingDB := Self.DB;
  Self.Registrar.Dispatcher := Self.Dispatch;
  Self.Registrar.MinimumExpiryTime := 3600;

  Self.Request := TIdSipRequest.Create;
  Self.Request.Method := MethodRegister;
  Self.Request.RequestUri.Uri := 'sip:tessier-ashpool.co.luna';
  Self.Request.ToHeader.Address.Uri := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Request.AddHeader(ContactHeaderFull).Value := 'sip:wintermute@talking-head.tessier-ashpool.co.luna';
  Self.Request.CSeq.Method := Self.Request.Method;
  Self.Request.CallID := '1@selftest.foo'
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

procedure TestTIdSipRegistrar.SimulateRemoteRequest;
begin
  Self.Dispatch.Transport.FireOnRequest(Self.Request);
end;

//* TestTIdSipRegistrar Published methods **************************************

procedure TestTIdSipRegistrar.TestReceiveInvite;
begin
  Self.Request.Method := MethodInvite;
  Self.SimulateRemoteRequest;
  CheckEquals(1,
              Self.Dispatch.Transport.SentResponseCount,
              'No response sent');
  CheckEquals(SIPMethodNotAllowed,
              Self.Dispatch.Transport.LastResponse.StatusCode,
              'Response code');
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
begin
  Self.Request.AddHeader(ExpiresHeader).Value := IntToStr(Self.Registrar.MinimumExpiryTime - 1);
  Self.SimulateRemoteRequest;
  CheckEquals(1,
              Self.Dispatch.Transport.SentResponseCount,
              'No response sent');
  CheckEquals(SIPIntervalTooBrief,
              Self.Dispatch.Transport.LastResponse.StatusCode,
              'Expires header value too low');
end;

procedure TestTIdSipRegistrar.TestReceiveExpireParamTooShort;
var
  FirstContact: TIdSipContactHeader;
begin
  FirstContact := Self.Request.FirstHeader(ContactHeaderFull) as TIdSipContactHeader;
  FirstContact.Expires := Self.Registrar.MinimumExpiryTime - 1;
  Self.SimulateRemoteRequest;
  CheckEquals(1,
              Self.Dispatch.Transport.SentResponseCount,
              'No response sent');
  CheckEquals(SIPIntervalTooBrief,
              Self.Dispatch.Transport.LastResponse.StatusCode,
              'Expires param value too low');
end;

//******************************************************************************
//* TIdSipMockBindingDatabase                                                  *
//******************************************************************************
//* TIdSipMockBindingDatabase Public methods ***********************************

constructor TIdSipMockBindingDatabase.Create;
begin
  inherited Create;

  Self.BindingStore := TStringList.Create;
end;

destructor TIdSipMockBindingDatabase.Destroy;
var
  I: Integer;
begin
  for I := 0 to Self.BindingStore.Count - 1 do
    Self.BindingStore.Objects[I].Free;

  Self.BindingStore.Free;

  inherited Destroy;
end;

procedure TIdSipMockBindingDatabase.AddBinding(const AddressOfRecord: String;
                                               Binding: TIdSipContactHeader);
var
  Index:      Integer;
  NewContact: TIdSipContactHeader;
begin
  NewContact := TIdSipContactHeader.Create;
  try
    NewContact.Value := Binding.FullValue;
    Self.BindingStore.AddObject(AddressOfRecord, NewContact);
  except
    Index := Self.BindingStore.IndexOfObject(NewContact);
    if (Index <> -1) then
      Self.DeleteBinding(Index)
    else
      FreeAndNil(NewContact);

    raise;
  end;
end;

function TIdSipMockBindingDatabase.BindingCount: Integer;
begin
  Result := Self.BindingStore.Count;
end;

procedure TIdSipMockBindingDatabase.BindingsFor(const AddressOfRecord: String;
                                                Bindings: TIdSipHeaders);
var
  I: Integer;
begin
  Bindings.Clear;

  for I := 0 to Self.BindingStore.Count - 1 do
    if IsEqual(Self.BindingStore[I], AddressOfRecord) then
      Bindings.Add(Self.Bindings[I]);
end;

procedure TIdSipMockBindingDatabase.RemoveAllBindings(const AddressOfRecord: String);
var
  I: Integer;
begin
  I := 0;
  while (I < Self.BindingStore.Count) do begin
    if IsEqual(Self.BindingStore[I], AddressOfRecord) then begin
      Self.DeleteBinding(I);
    end
    else
      Inc(I);
  end;
end;

procedure TIdSipMockBindingDatabase.RemoveBinding(const AddressOfRecord: String;
                                                  Binding: TIdSipContactHeader);
var
  I: Integer;
begin
  I := 0;
  while (I < Self.BindingCount)
    and not IsEqual(Self.BindingStore[I], AddressOfRecord)
    and not Self.Bindings[I].IsEqualTo(Binding) do
    Inc(I);

  if (I < Self.BindingCount) then
    Self.DeleteBinding(I);
end;

//* TIdSipMockBindingDatabase Private methods **********************************

procedure TIdSipMockBindingDatabase.DeleteBinding(Index: Integer);
begin
  Self.BindingStore.Objects[Index].Free;
  Self.BindingStore.Delete(Index);
end;

function TIdSipMockBindingDatabase.GetBindings(Index: Integer): TIdSipContactHeader;
begin
  Result := Self.BindingStore.Objects[Index] as TIdSipContactHeader;
end;

//******************************************************************************
//* TestTIdSipMockBindingDatabase                                              *
//******************************************************************************
//* TestTIdSipMockBindingDatabase Public methods *******************************

procedure TestTIdSipMockBindingDatabase.SetUp;
begin
  inherited SetUp;

  Self.DB := TIdSipMockBindingDatabase.Create;

  Self.CasesAOR := 'sip:case@fried.neurons.org';
  Self.CaseContact := TIdSipContactHeader.Create;
  Self.CaseContact.Value := 'Case <sips:case@fried.neurons.org>';

  Self.WintermutesAOR := 'sip:wintermute@tessier-ashpool.co.luna';
  Self.Wintermute := TIdSipContactHeader.Create;
  Self.Wintermute.Value := 'Wintermute <' + Self.WintermutesAOR + '>';
end;

procedure TestTIdSipMockBindingDatabase.TearDown;
begin
  Self.Wintermute.Free;
  Self.CaseContact.Free;
  Self.DB.Free;

  inherited TearDown;
end;

//* TestTIdSipMockBindingDatabase Published methods ****************************

procedure TestTIdSipMockBindingDatabase.TestAddBinding;
begin
  Self.DB.AddBinding(Self.Wintermute.Address.Host, Self.Wintermute);
  CheckEquals(1, Self.DB.BindingCount, 'Binding not added');
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsFor;
var
  Bindings: TIdSipHeaders;
begin
  Self.DB.AddBinding(Self.WintermutesAOR, Self.Wintermute);
  Self.Wintermute.Value := 'Wintermute <sip:wintermute@talking-head.tessier-ashpool.co.luna>';
  Self.DB.AddBinding(Self.WintermutesAOR, Self.Wintermute);

  Self.DB.AddBinding(Self.CasesAOR, Self.CaseContact);

  Bindings := TIdSipHeaders.Create;
  try
    Self.DB.BindingsFor(Self.WintermutesAOR, Bindings);
    CheckEquals(2, Bindings.Count, 'Wrong number of bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestBindingsForClearsList;
var
  Bindings: TIdSipHeaders;
begin
  Self.DB.AddBinding(Self.WintermutesAOR, Self.Wintermute);
  Self.Wintermute.Value := 'Wintermute <sip:wintermute@talking-head.tessier-ashpool.co.luna>';
  Self.DB.AddBinding(Self.WintermutesAOR, Self.Wintermute);

  Bindings := TIdSipHeaders.Create;
  try
    Bindings.Add(Self.Wintermute);

    Self.DB.BindingsFor(Self.CasesAOR, Bindings);
    CheckEquals(0, Bindings.Count, 'Wrong number of bindings');
  finally
    Bindings.Free;
  end;
end;

procedure TestTIdSipMockBindingDatabase.TestRemoveAllBindings;
begin
  Self.DB.AddBinding(Self.WintermutesAOR, Self.Wintermute);
  Self.DB.AddBinding(Self.WintermutesAOR, Self.Wintermute);
  Self.DB.AddBinding(Self.CasesAOR, Self.CaseContact);
  Self.DB.RemoveAllBindings(Self.WintermutesAOR);
  CheckEquals(1,
              Self.DB.BindingCount,
              'Number of records removed');
  CheckEquals(Self.CaseContact.Value,
              Self.DB.Bindings[0].Value,
              'Wrong records removed');
end;

procedure TestTIdSipMockBindingDatabase.TestRemoveBinding;
var
  Bindings:    TIdSipHeaders;
  ToBeDeleted: TIdSipContactHeader;
begin
  ToBeDeleted := TIdSipContactHeader.Create;
  try
    ToBeDeleted.Value := Self.Wintermute.FullValue;

    Self.DB.AddBinding(Self.CasesAOR, Self.CaseContact);
    Self.DB.AddBinding(Self.WintermutesAOR, ToBeDeleted);
    Self.CaseContact.Address.Host := 'sip:case@111.public.booth.org';
    Self.DB.AddBinding(Self.CasesAOR, Self.CaseContact);
    Self.Wintermute.Address.Host := 'talking-head.tessier-ashpool.co.luna';
    Self.DB.AddBinding(Self.WintermutesAOR, Self.Wintermute);

    Self.DB.RemoveBinding(Self.WintermutesAOR, ToBeDeleted);

    Bindings := TIdSipHeaders.Create;
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

initialization
  RegisterTest('Registrar', Suite);
end.
