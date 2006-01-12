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
 Classes, Contnrs, IdSipMessage, IdSipRegistration;

type
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
    function CollectBindingsFor(const AddressOfRecord: String;
                                Bindings: TIdSipContacts;
                                CollectGruus: Boolean): Boolean; override;
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

    property Authorized:               Boolean             read fAuthorized write fAuthorized;
    property Bindings[Index: Integer]: TIdRegistrarBinding read GetBindings;
    property FailAddBinding:           Boolean             read fFailAddBinding write fFailAddBinding;
    property FailBindingsFor:          Boolean             read fFailBindingsFor write fFailBindingsFor;
    property FailRemoveBinding:        Boolean             read fFailRemoveBinding write fFailRemoveBinding;
    property FailIsValid:              Boolean             read fFailIsValid write fFailIsValid;
  end;

implementation

uses
  DateUtils, IdRandom, IdSipConsts, IdSystem, SysUtils;

const
  ItemNotFoundIndex = -1;

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

function TIdSipMockBindingDatabase.IsAuthorized(User: TIdSipAddressHeader;
                                                AddressOfRecord: TIdSipUri): Boolean;
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
  Self.NotifyListenersOfChange;

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

function TIdSipMockBindingDatabase.CollectBindingsFor(const AddressOfRecord: String;
                                                      Bindings: TIdSipContacts;
                                                      CollectGruus: Boolean): Boolean;
var
  ContactValue: String;
  I:            Integer;
  Gruu:         String;
begin
  Bindings.Clear;

  for I := 0 to Self.BindingStore.Count - 1 do
    if (Self.Bindings[I].AddressOfRecord = AddressOfRecord) then begin
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

end.
