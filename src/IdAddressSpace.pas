{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  This unit contains code written by:
    * Frank Shearar
}
unit IdAddressSpace;

interface

uses
  Contnrs;

type
  TIdAddressSpaceType = (asIPv4Subnet, asIPv6Subnet, asDomain, asUnknown);

  // I describe an address space - a subnet (either IPv4 or IPv6) or a domain.
  // For instance, "10.0.0.0/8" and "example.com" are both address spaces. You
  // can specify subnets either with the number of significant bits
  // ("10.0.0.0/8") or with a netmask ("2002:1::/ffff::"). Domains don't have
  // masks - their "space" consists of all domain names that end in the
  // specified suffix (so if the domain is "example.com", then "bob.example.com"
  // is in the address space, while "alice.somewhere.else.org" is not). The
  // special domain "" (the empty string) contains ALL domain names. It's
  // analogous to IPv4's "0.0.0.0/0".
  TIdAddressSpace = class(TObject)
  private
    fDescription: String;

    function SubnetTypeMatchesAddressType(SpaceType: TIdAddressSpaceType;
                                          Subnet: String): Boolean;
  protected
    function  InternalContains(Address: String): Boolean; virtual;
    function  GetDescription: String; virtual;
    procedure SetDescription(Value: String); virtual;
    function  SpaceType: TIdAddressSpaceType; virtual;
  public
    class function AddressToAddressSpaceType(Address: String): TIdAddressSpaceType;
    class function CreateAddressSpace(SpaceType: TIdAddressSpaceType): TIdAddressSpace; overload;
    class function CreateAddressSpace(Description: String): TIdAddressSpace; overload;

    function Contains(Address: String): Boolean;
    function IdentifySpaceType(Address: String): TIdAddressSpaceType;
    function MoreSpecificThan(OtherSpace: TIdAddressSpace): Boolean; virtual;

    property Description: String read GetDescription write SetDescription;
  end;

  TIdAddressSpaceClass = class of TIdAddressSpace;

  TIdIPAddressSpace = class(TIdAddressSpace)
  protected
    Mask:   String;
    Subnet: String;

    function  GetDescription: String; override;
    function  InternalContains(Address: String): Boolean; override;
    procedure SetDescription(Value: String); override;
  end;

  TIdIPv4SubnetAddressSpace = class(TIdIPAddressSpace)
  protected
    function SpaceType: TIdAddressSpaceType; override;
  public
    function MoreSpecificThan(OtherSpace: TIdAddressSpace): Boolean; override;
  end;

  TIdIPv6SubnetAddressSpace = class(TIdIPAddressSpace)
  protected
    function SpaceType: TIdAddressSpaceType; override;
  public
    function MoreSpecificThan(OtherSpace: TIdAddressSpace): Boolean; override;
  end;

  TIdDomainAddressSpace = class(TIdAddressSpace)
  protected
    function InternalContains(Address: String): Boolean; override;
    function SpaceType: TIdAddressSpaceType; override;
  public
    function MoreSpecificThan(OtherSpace: TIdAddressSpace): Boolean; override;
  end;

  // I store parameters on a per-address space basis.
  TIdParameterForAddressSpace = class(TObject)
  private
    Space: TIdAddressSpace;

    function  GetAddressSpace: String;
    procedure SetAddressSpace(Value: String);
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    function ParameterFor(Address: String): Boolean;

    property AddressSpace: String read GetAddressSpace write SetAddressSpace;
  end;

  TIdParameterForAddressSpaceList = class(TObject)
  private
    Descs:             TObjectList;
    fDefaultParameter: TIdParameterForAddressSpace;
  protected
    procedure AddParameter(Parameter: TIdParameterForAddressSpace);
    function  CanonicaliseAddressSpace(AddressSpace: String): String;
    function  CreateDefaultParameter: TIdParameterForAddressSpace; virtual;
    function  FindParameterFor(Address: String): TIdParameterForAddressSpace;
    function  FindParameterForAddressSpace(AddressSpace: String): TIdParameterForAddressSpace;
    function  HasParameterForAddressSpace(AddressSpace: String): Boolean;
    procedure RemoveParameter(AddressSpace: String);

    property DefaultParameter: TIdParameterForAddressSpace read fDefaultParameter;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure ClearAllParameters; virtual;
    function  Count: Integer;
  end;

function AddressSpaceSort(Item1, Item2: Pointer): Integer;
function NumberOfOccurences(C: Char; S: String): Integer;
function Suffix(S: String; Len: Cardinal): String;

implementation

uses
  Classes, IdSimpleParser, IdSystem, RuntimeSafety, SysUtils;

//******************************************************************************
//* Unit public functions & procedures                                         *
//******************************************************************************

function AddressSpaceSort(Item1, Item2: Pointer): Integer;
var
  A, B: TIdParameterForAddressSpace;
begin
  // Order more specific address spaces to less specific.

  A := TIdParameterForAddressSpace(Item1);
  B := TIdParameterForAddressSpace(Item2);

  if A.Space.MoreSpecificThan(B.Space) then
    Result := -1
  else if B.Space.MoreSpecificThan(A.Space) then
    Result := 1
  else
    Result := 0;
end;

function NumberOfOccurences(C: Char; S: String): Integer;
var
  I: Integer;
begin
  // Return the number of times C appears in S.
  Result := 0;
  for I := 1 to Length(S) do
    if (S[I] = C) then Inc(Result);
end;

function Suffix(S: String; Len: Cardinal): String;
var
  StringLength: Cardinal;
begin
  // WHY would someone make Length() return a signed integer? Can someone please
  // tell me what a string with negative length looks like?
  StringLength := Cardinal(Length(S));

  if (Len > StringLength) then
    Result := S
  else begin
    Result := Copy(S, StringLength - Len + 1, Len)
  end;
end;

//******************************************************************************
//* TIdAddressSpace                                                            *
//******************************************************************************
//* TIdAddressSpace Public methods *********************************************

class function TIdAddressSpace.AddressToAddressSpaceType(Address: String): TIdAddressSpaceType;
begin
  case TIdIPAddressParser.IPVersion(Address) of
    Id_IPv4: Result := asIPv4Subnet;
    Id_IPv6: Result := asIPv6Subnet;
  else
    if TIdSimpleParser.IsFQDN(Address) or (Address = '') then
      Result := asDomain
    else
      Result := asUnknown;
  end;
end;

class function TIdAddressSpace.CreateAddressSpace(SpaceType: TIdAddressSpaceType): TIdAddressSpace;
var
  ResultType: TIdAddressSpaceClass;
begin
  case SpaceType of
    asIPv4Subnet: ResultType := TIdIPv4SubnetAddressSpace;
    asIPv6Subnet: ResultType := TIdIPv6SubnetAddressSpace;
    asDomain:     ResultType := TIdDomainAddressSpace;
  else
    ResultType := TIdAddressSpace;
  end;

  Result := ResultType.Create;
end;

class function TIdAddressSpace.CreateAddressSpace(Description: String): TIdAddressSpace;
var
  A: TIdAddressSpace;
begin
  // TODO: Creating an object just to get to IdentifySpaceType is lame.
  
  A := TIdAddressSpace.Create;
  try
    Result := Self.CreateAddressSpace(A.IdentifySpaceType(Description));
    Result.Description := Description;
  finally
    A.Free;
  end;
end;

function TIdAddressSpace.Contains(Address: String): Boolean;
begin
  if (Self.SpaceType <> Self.AddressToAddressSpaceType(Address)) then begin
    Result := false;
    Exit;
  end;

  Result := Self.InternalContains(Address);
end;

function TIdAddressSpace.IdentifySpaceType(Address: String): TIdAddressSpaceType;
var
  Subnet: String;
begin
  if (Pos('/', Address) > 0) then begin
    // Process subnet
    Subnet := Address;
    Address := Fetch(Subnet, '/');

    Result := Self.AddressToAddressSpaceType(Address);

    if not Self.SubnetTypeMatchesAddressType(Result, Subnet) then
      Result := asUnknown;
  end
  else begin
    // Process address or domain
    Result := Self.AddressToAddressSpaceType(Address);
  end;
end;

function TIdAddressSpace.MoreSpecificThan(OtherSpace: TIdAddressSpace): Boolean;
begin
  // A "more specific" address space is one that defines a smaller address space.
  // This function returns true if and only if
  // * Self and OtherSpace are of the same SpaceType and
  // ** if Self is a domain address and OtherSpace is a suffix of Self or
  // ** if Self is an IPv4/IPv6 address that is contained inside OtherSpace.
  Result := false;
{
  Result := Self.SpaceType = OtherSpace.SpaceType;

  if Result then begin
    case Self.SpaceType of
      asDomain: Result := Self.Contains(OtherSpace);
      asIPv4Subnet: Result := false;
      asIPv6Subnet: Result := false;
    end;
  end;
}
end;

//* TIdAddressSpace Protected methods ******************************************

function TIdAddressSpace.InternalContains(Address: String): Boolean;
begin
  Result := false;
end;

function TIdAddressSpace.GetDescription: String;
begin
  Result := Self.fDescription;
end;

procedure TIdAddressSpace.SetDescription(Value: String);
begin
  Self.fDescription := Value;
end;

function TIdAddressSpace.SpaceType: TIdAddressSpaceType;
begin
  Result := asUnknown;
end;

//* TIdAddressSpace Private methods ********************************************

function TIdAddressSpace.SubnetTypeMatchesAddressType(SpaceType: TIdAddressSpaceType;
                                                      Subnet: String): Boolean;
var
  SubnetVer: TIdIPVersion;
begin
  // Given a subnet description (either something like "255.255.255.0" or
  // "ffff::" or a number), return true if that subnet description is compatible
  // with SpaceType.

  Result := TIdSimpleParser.IsNumber(Subnet);

  if not Result then begin
    SubnetVer := TIdIPAddressParser.IPVersion(Subnet);

    Result := ((SpaceType = asIPv4Subnet) and (SubnetVer = Id_IPv4))
           or ((SpaceType = asIPv6Subnet) and (SubnetVer = Id_IPv6))
  end;
end;

//******************************************************************************
//* TIdIPAddressSpace                                                          *
//******************************************************************************
//* TIdIPAddressSpace Protected methods ****************************************

function TIdIPAddressSpace.GetDescription: String;
begin
  Result := Self.Subnet + '/' + Self.Mask;
end;

function TIdIPAddressSpace.InternalContains(Address: String): Boolean;
begin
  Result := OnSameNetwork(Self.Subnet, Address, Self.Mask);
end;

procedure TIdIPAddressSpace.SetDescription(Value: String);
var
  M: String;
begin
  inherited SetDescription(Value);

  M           := Value;
  Self.Subnet := Fetch(M, '/');

  if TIdSimpleParser.IsNumber(M) then
    Self.Mask := TIdIPAddressParser.MaskToAddress(StrToInt(M), TIdIPAddressParser.IPVersion(Self.Subnet))
  else
    Self.Mask := M;
end;

//******************************************************************************
//* TIdIPv4SubnetAddressSpace                                                  *
//******************************************************************************
//* TIdIPv4SubnetAddressSpace Public methods ***********************************

function TIdIPv4SubnetAddressSpace.MoreSpecificThan(OtherSpace: TIdAddressSpace): Boolean;
var
  OtherMask: String;
begin
  Result := Self.SpaceType = OtherSpace.SpaceType;

  if Result then begin
    OtherMask := OtherSpace.Description;
    Fetch(OtherMask, '/');

    if TIdSimpleParser.IsNumber(OtherMask) then
      OtherMask := TIdIPAddressParser.MaskToAddress(StrToInt(OtherMask), TIdIPAddressParser.IPVersion(Self.Subnet));

    Result := TIdIPAddressParser.InetAddr(Self.Mask) > TIdIPAddressParser.InetAddr(OtherMask);
  end;
end;

//* TIdIPv4SubnetAddressSpace Protected methods ********************************

function TIdIPv4SubnetAddressSpace.SpaceType: TIdAddressSpaceType;
begin
  Result := asIPv4Subnet;
end;

//******************************************************************************
//* TIdIPv6SubnetAddressSpace                                                  *
//******************************************************************************
//* TIdIPv6SubnetAddressSpace Public methods ***********************************

function TIdIPv6SubnetAddressSpace.MoreSpecificThan(OtherSpace: TIdAddressSpace): Boolean;
var
  I:             Integer;
  Larger:        Boolean;
  MaskAddr:      TIdIPv6AddressRec;
  OtherMask:     String;
  OtherMaskAddr: TIdIPv6AddressRec;
begin
  if (Self.Mask = '') then begin
    // The description of this address space is malformed, and nothing fruitful
    // can be done with it.
    Result := false;
    Exit;
  end;

  Result := Self.SpaceType = OtherSpace.SpaceType;

  if Result then begin
    OtherMask := OtherSpace.Description;
    Fetch(OtherMask, '/');

    if TIdSimpleParser.IsNumber(OtherMask) then
      OtherMask := TIdIPAddressParser.MaskToAddress(StrToInt(OtherMask), TIdIPAddressParser.IPVersion(Self.Subnet));

    TIdIPAddressParser.ParseIPv6Address(Self.Mask, MaskAddr);
    TIdIPAddressParser.ParseIPv6Address(OtherMask, OtherMaskAddr);

    Larger := false;
    I := 0;
    while not Larger and (I <= High(MaskAddr)) do begin
      Larger := MaskAddr[I] > OtherMaskAddr[I];
      Inc(I);
    end;
    Result := Larger;
  end;
end;

//* TIdIPv6SubnetAddressSpace Protected methods ********************************

function TIdIPv6SubnetAddressSpace.SpaceType: TIdAddressSpaceType;
begin
  Result := asIPv6Subnet;
end;

//******************************************************************************
//* TIdDomainAddressSpace                                                      *
//******************************************************************************
//* TIdDomainAddressSpace Public methods ***************************************

function TIdDomainAddressSpace.MoreSpecificThan(OtherSpace: TIdAddressSpace): Boolean;
var
  NumberOfLabels:      Integer;
  OtherNumberOfLabels: Integer;
begin
  Result := Self.SpaceType = OtherSpace.SpaceType;

  if Result then begin
    NumberOfLabels      := NumberOfOccurences('.', Self.Description);
    OtherNumberOfLabels := NumberOfOccurences('.', OtherSpace.Description);

    if (NumberOfLabels = OtherNumberOfLabels) then
      Result := (Self.Description <> '') and (OtherSpace.Description = '')
    else
      Result := NumberOfLabels > OtherNumberOfLabels;
  end;
end;

//* TIdDomainAddressSpace Protected methods ************************************

function TIdDomainAddressSpace.InternalContains(Address: String): Boolean;
begin
  Result := Suffix(Address, Length(Self.Description)) = Self.Description;
end;

function TIdDomainAddressSpace.SpaceType: TIdAddressSpaceType;
begin
  Result := asDomain;
end;

//******************************************************************************
//* TIdParameterForAddressSpace                                                *
//******************************************************************************
//* TIdParameterForAddressSpace Public methods *********************************

constructor TIdParameterForAddressSpace.Create;
begin
  inherited Create;

  Self.Space := TIdAddressSpace.Create;
end;

destructor TIdParameterForAddressSpace.Destroy;
begin
  Self.Space.Free;

  inherited Destroy;
end;

function TIdParameterForAddressSpace.ParameterFor(Address: String): Boolean;
begin
  // Is Address a member of the set of addresses in Self.Space?

  Result := Self.Space.Contains(Address);
end;

//* TIdParameterForAddressSpace Private methods ********************************

function TIdParameterForAddressSpace.GetAddressSpace: String;
begin
  Result := Self.Space.Description;
end;

procedure TIdParameterForAddressSpace.SetAddressSpace(Value: String);
begin
  if (Self.AddressSpace = Value) then Exit;

  Self.Space.Free;
  Self.Space := TIdAddressSpace.CreateAddressSpace(Value);
end;

//******************************************************************************
//* TIdParameterForAddressSpaceList                                            *
//******************************************************************************
//* TIdParameterForAddressSpaceList Public methods *****************************

constructor TIdParameterForAddressSpaceList.Create;
begin
  inherited Create;

  Self.Descs := TObjectList.Create(true);
  Self.fDefaultParameter := Self.CreateDefaultParameter;
end;

destructor TIdParameterForAddressSpaceList.Destroy;
begin
  Self.fDefaultParameter.Free;
  Self.Descs.Free;

  inherited Destroy;
end;

procedure TIdParameterForAddressSpaceList.ClearAllParameters;
begin
  Self.Descs.Clear;
end;

function TIdParameterForAddressSpaceList.Count: Integer;
begin
  Result := Self.Descs.Count;
end;

//* TIdParameterForAddressSpaceList Protected methods **************************

procedure TIdParameterForAddressSpaceList.AddParameter(Parameter: TIdParameterForAddressSpace);
begin
  Self.Descs.Add(Parameter);
  Self.Descs.Sort(AddressSpaceSort);
end;

function TIdParameterForAddressSpaceList.CanonicaliseAddressSpace(AddressSpace: String): String;
var
  S: TIdAddressSpace;
begin
  S := TIdAddressSpace.CreateAddressSpace(AddressSpace);
  try
    Result := S.Description;
  finally
    S.Free;
  end;
end;

function TIdParameterForAddressSpaceList.CreateDefaultParameter: TIdParameterForAddressSpace;
begin
  Result := nil;
  RaiseAbstractError(Self.ClassName, 'CreateDefaultParameter');
end;

function TIdParameterForAddressSpaceList.FindParameterFor(Address: String): TIdParameterForAddressSpace;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Descs.Count - 1 do begin
    if TIdParameterForAddressSpace(Self.Descs[I]).ParameterFor(Address) then begin
      Result := TIdParameterForAddressSpace(Self.Descs[I]);
      Break;
    end;
  end;
end;

function TIdParameterForAddressSpaceList.FindParameterForAddressSpace(AddressSpace: String): TIdParameterForAddressSpace;
var
  CanonicalAddressSpace: String;
  I:                     Integer;
begin
  CanonicalAddressSpace := Self.CanonicaliseAddressSpace(AddressSpace);

  Result := nil;
  for I := 0 to Self.Descs.Count - 1 do begin
    if (TIdParameterForAddressSpace(Self.Descs[I]).AddressSpace = CanonicalAddressSpace) then begin
      Result := TIdParameterForAddressSpace(Self.Descs[I]);
      Break;
    end;
  end;
end;

function TIdParameterForAddressSpaceList.HasParameterForAddressSpace(AddressSpace: String): Boolean;
begin
  Result := Assigned(Self.FindParameterForAddressSpace(AddressSpace));
end;

procedure TIdParameterForAddressSpaceList.RemoveParameter(AddressSpace: String);
var
  I: Integer;
begin
  // Remove the parameter associated with AddressSpace.

  for I := 0 to Self.Descs.Count - 1 do begin
    if (TIdParameterForAddressSpace(Self.Descs[I]).AddressSpace = AddressSpace) then begin
      Self.Descs.Delete(I);
      Break;
    end;
  end;
end;

end.
