{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipMockBindingDatabase;

interface

uses
 Classes, Contnrs, IdSipMessage, IdSipRegistration, SyncObjs;

type
  TIdSipMatchingFunction = function(UriA, UriB: TIdSipUri): Boolean;

  // I am a proper BindingDatabase. I just store my data in memory, and provide
  // no persistence. Oh, and I allow you to cause operations to fail by setting
  // any of the Fail* properties.
  TIdSipMockBindingDatabase = class(TIdSipAbstractBindingDatabase)
  private
    BindingStore:       TObjectList;
    fAuthorized:        Boolean;
    fFailAddBinding:    Boolean;
    fFailBindingsFor:   Boolean;
    fFailIsValid:       Boolean;
    fFailRemoveBinding: Boolean;
    fFailUpdateBinding: Boolean;
    Lock:               TCriticalSection;

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
    function  CollectBindingsFor(const AddressOfRecord: String;
                                 Bindings: TIdSipContacts;
                                 CollectGruus: Boolean): Boolean; override;
    function  CollectBindingsForUsing(const AddressOfRecord: String;
                                      Bindings: TIdSipContacts;
                                      CollectGruus: Boolean;
                                      MatchingFunction: TIdSipMatchingFunction): Boolean;
    procedure Commit; override;
    procedure Rollback; override;
    procedure StartTransaction; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  BindingCount: Integer;
    function  IsAuthorized(User: TIdSipAddressHeader;
                           AddressOfRecord: TIdSipUri): Boolean; override;
    function  IsValid(Request: TIdSipRequest): Boolean; override;
    function  RemoveBinding(Request: TIdSipRequest;
                            Contact: TIdSipContactHeader): Boolean; override;
    function  UpdateBinding(Request: TIdSipRequest;
                            Contact: TIdSipContactHeader): Boolean; override;

    property Authorized:               Boolean             read fAuthorized write fAuthorized;
    property Bindings[Index: Integer]: TIdRegistrarBinding read GetBindings;
    property FailAddBinding:           Boolean             read fFailAddBinding write fFailAddBinding;
    property FailBindingsFor:          Boolean             read fFailBindingsFor write fFailBindingsFor;
    property FailRemoveBinding:        Boolean             read fFailRemoveBinding write fFailRemoveBinding;
    property FailUpdateBinding:        Boolean             read fFailUpdateBinding write fFailUpdateBinding;
    property FailIsValid:              Boolean             read fFailIsValid write fFailIsValid;
  end;

  // I am an in-memory BindingDatabase that matches only the usernames of URIs.
  // That means that <sip:foo@bar> will match <sip:foo@baz> and <sip:foo@quaax>.
  // This is in contrast to my superclass, who matches on the entire URI.
  TIdSipNameMatchingMockBindingDatabase = class(TIdSipMockBindingDatabase)
  protected
    function CollectBindingsFor(const AddressOfRecord: String;
                                Bindings: TIdSipContacts;
                                CollectGruus: Boolean): Boolean; override;
  end;

function FullUriMatch(UriA, UriB: TIdSipUri): Boolean;
function UsernameMatch(UriA, UriB: TIdSipUri): Boolean;

implementation

uses
  DateUtils, IdRandom, IdSystem, SysUtils;

const
  ItemNotFoundIndex = -1;

//******************************************************************************
//* Unit public functions and procedures                                       *
//******************************************************************************

function FullUriMatch(UriA, UriB: TIdSipUri): Boolean;
begin
  Result := UriA.AsString = UriB.AsString;
end;

function UsernameMatch(UriA, UriB: TIdSipUri): Boolean;
begin
  Result := UriA.Username = UriB.Username;
end;

//******************************************************************************
//* TIdSipMockBindingDatabase                                                  *
//******************************************************************************
//* TIdSipMockBindingDatabase Public methods ***********************************

constructor TIdSipMockBindingDatabase.Create;
begin
  inherited Create;

  Self.BindingStore := TObjectList.Create(true);
  Self.Lock         := TCriticalSection.Create;

  Self.Authorized        := true;
  Self.FailAddBinding    := false;
  Self.FailBindingsFor   := false;
  Self.FailIsValid       := false;
  Self.FailRemoveBinding := false;
end;

destructor TIdSipMockBindingDatabase.Destroy;
begin
  Self.Lock.Acquire;
  try
    Self.BindingStore.Free;
  finally
    Self.Lock.Release;
  end;
  Self.Lock.Free;

  inherited Destroy;
end;

function TIdSipMockBindingDatabase.BindingCount: Integer;
begin
  Self.Lock.Acquire;
  try
    Result := Self.BindingStore.Count;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipMockBindingDatabase.IsAuthorized(User: TIdSipAddressHeader;
                                                AddressOfRecord: TIdSipUri): Boolean;
begin
  Self.Lock.Acquire;
  try
    Result := Self.Authorized;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipMockBindingDatabase.IsValid(Request: TIdSipRequest): Boolean;
begin
  Self.Lock.Acquire;
  try
    Result := not Self.FailIsValid;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipMockBindingDatabase.RemoveBinding(Request: TIdSipRequest;
                                                 Contact: TIdSipContactHeader): Boolean;
begin
  Self.Lock.Acquire;
  try
    Self.DeleteBinding(Self.IndexOfBinding(Request.AddressOfRecord,
                                           Contact));

    Result := not Self.FailRemoveBinding;
  finally
    Self.Lock.Release;
  end;
end;

function TIdSipMockBindingDatabase.UpdateBinding(Request: TIdSipRequest;
                                                 Contact: TIdSipContactHeader): Boolean;
function ExpiryTime(Expires: Cardinal): TDateTime;
begin
  Result := Now + OneSecond * Expires;
end;
var
  Expiry: Cardinal;
  Index:  Integer;
begin
  Self.Lock.Acquire;
  try
    Expiry := Self.CorrectExpiry(Request, Contact);
    Index  := Self.IndexOfBinding(Request.AddressOfRecord, Contact);

    if (Expiry = 0) then begin
      // We should never even enter this clause; the superclass should call
      // RemoveBinding for this binding.
      Self.RemoveBinding(Request, Contact);
    end
    else if (ExpiryTime(Expiry) > Self.Bindings[Index].ValidUntil) then begin
      Self.Binding(Request.AddressOfRecord, Contact.AsAddressOfRecord).ValidUntil := ExpiryTime(Expiry);
    end;

    Result := not Self.FailUpdateBinding;
  finally
    Self.Lock.Release;
  end;
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
                                           Contact.SipInstance,
                                           CallID,
                                           SequenceNo,
                                           Now + OneSecond*ExpiryTime);
  try
    Self.BindingStore.Add(NewBinding);
  except
    Index := Self.BindingStore.IndexOf(NewBinding);
    if (Index <> ItemNotFoundIndex) then
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
  I:       Integer;
  Binding: TIdRegistrarBinding;
begin
  Result := nil;

  I := 0;
  while (I < Self.BindingCount) and not Assigned(Result) do begin
    Binding := Self.Bindings[I];
    if (Binding.AddressOfRecord = AddressOfRecord) and (Binding.Uri = CanonicalUri) then
      Result := Binding
    else
      Inc(I);
  end;
end;

function TIdSipMockBindingDatabase.CollectBindingsFor(const AddressOfRecord: String;
                                                      Bindings: TIdSipContacts;
                                                      CollectGruus: Boolean): Boolean;
begin
  Result := Self.CollectBindingsForUsing(AddressOfRecord, Bindings, CollectGruus, FullUriMatch)
end;

function TIdSipMockBindingDatabase.CollectBindingsForUsing(const AddressOfRecord: String;
                                                           Bindings: TIdSipContacts;
                                                           CollectGruus: Boolean;
                                                           MatchingFunction: TIdSipMatchingFunction): Boolean;
var
  AOR:          TIdSipUri;
  BindingAOR:   TIdSipUri;
  ContactValue: String;
  I:            Integer;
  Gruu:         String;
begin
  AOR := TIdSipUri.Create;
  try
    BindingAOR := TIdSipUri.Create;
    try
      AOR.Uri := AddressOfRecord;
      for I := 0 to Self.BindingStore.Count - 1 do begin
        BindingAOR.Uri := Self.Bindings[I].AddressOfRecord;

        if MatchingFunction(BindingAOR, AOR) then begin
          ContactValue := Self.Bindings[I].Uri
                        + ';' + ExpiresParam + '=';

          if (Now > Self.Bindings[I].ValidUntil) then
            ContactValue := ContactValue + '0'
          else
            ContactValue := ContactValue + IntToStr(SecondsBetween(Self.Bindings[I].ValidUntil, Now));

          if CollectGruus then begin
            Gruu := Self.CreateGruu(Self.Bindings[I].Uri, Self.Bindings[I].InstanceID);
            ContactValue := ContactValue + ';' + GruuParam + '="' + Gruu + '"';
          end;

          Bindings.Add(ContactHeaderFull).Value := ContactValue;
        end;
      end;
    finally
      BindingAOR.Free;
    end;
  finally
    AOR.Free;
  end;

  Result := not Self.FailBindingsFor;
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
    Result := ItemNotFoundIndex;
end;

//******************************************************************************
//* TIdSipNameMatchingMockBindingDatabase
//******************************************************************************
//* TIdSipNameMatchingMockBindingDatabase Protected methods ********************

function TIdSipNameMatchingMockBindingDatabase.CollectBindingsFor(const AddressOfRecord: String;
                                                                  Bindings: TIdSipContacts;
                                                                  CollectGruus: Boolean): Boolean;
begin
  Result := Self.CollectBindingsForUsing(AddressOfRecord, Bindings, CollectGruus, UsernameMatch);
end;

end.
