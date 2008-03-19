{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit StringDictionary;

interface

uses
  Classes, Contnrs;

type
  TKeyValuePair = class(TObject)
  private
    fKey:   String;
    fValue: String;
  public
    constructor Create(Key, Value: String);

    property Key:   String read fKey write fKey;
    property Value: String read fValue write fValue;
  end;

  TStringDictionary = class(TObject)
  private
    Associations: TObjectList;

    function  AssociationAt(Index: Integer): TKeyValuePair;
    function  InternalFind(Key: String): TKeyValuePair;
    procedure Sort;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Key, Value: String);
    procedure AddKeys(Keys: TStringDictionary);
    procedure Clear;
    procedure CollectKeys(Result: TStrings);
    function  Count: Integer;
    function  Find(Key: String): String;
    function  HasKey(Key: String): Boolean;
    function  IsEmpty: Boolean;
    procedure Remove(Key: String);
  end;

function TKeyValuePairSort(Item1, Item2: Pointer): Integer;

implementation

uses
  SysUtils;

function TKeyValuePairSort(Item1, Item2: Pointer): Integer;
var
  KeyA, KeyB: TKeyValuePair;
begin
  KeyA := TKeyValuePair(Item1);
  KeyB := TKeyValuePair(Item2);

  Result := CompareStr(Lowercase(KeyA.Key), Lowercase(KeyB.Key));

  if (Result = 0) then
    Result := CompareStr(KeyA.Value, KeyB.Value)
end;

//******************************************************************************
//* TKeyValuePair                                                              *
//******************************************************************************
//* TKeyValuePair Public methods ***********************************************

constructor TKeyValuePair.Create(Key, Value: String);
begin
  inherited Create;

  Self.fKey   := Key;
  Self.fValue := Value;
end;

//******************************************************************************
//* TStringDictionary                                                          *
//******************************************************************************
//* TStringDictionary Public methods *******************************************

constructor TStringDictionary.Create;
begin
  inherited Create;

  Self.Associations := TObjectList.Create(true);
end;

destructor TStringDictionary.Destroy;
begin
  Self.Associations.Free;

  inherited Destroy;
end;

procedure TStringDictionary.Add(Key, Value: String);
begin
  if not Self.HasKey(Key) then begin
    Self.Associations.Add(TKeyValuePair.Create(Key, Value));
    Self.Sort;
  end;
end;

procedure TStringDictionary.AddKeys(Keys: TStringDictionary);
var
  I:        Integer;
  KeyNames: TStrings;
begin
  KeyNames := TStringList.Create;
  try
    Keys.CollectKeys(KeyNames);

    for I := 0 to Keys.Count - 1 do
      Self.Add(KeyNames[I], Keys.Find(KeyNames[I]));
  finally
    KeyNames.Free;
  end;
end;

procedure TStringDictionary.Clear;
begin
  Self.Associations.Clear;
end;

procedure TStringDictionary.CollectKeys(Result: TStrings);
var
  I: Integer;
begin
  // Collect all key names. I do not guarantee order.

  for I := 0 to Self.Associations.Count - 1 do
    Result.Add(Self.AssociationAt(I).Key);
end;

function TStringDictionary.Count: Integer;
begin
  Result := Self.Associations.Count;
end;

function TStringDictionary.Find(Key: String): String;
var
  Assoc: TKeyValuePair;
begin
  Assoc := Self.InternalFind(Key);

  if Assigned(Assoc) then
    Result := Assoc.Value
  else
    Result := '';
end;

function TStringDictionary.HasKey(Key: String): Boolean;
begin
  Result := Self.InternalFind(Key) <> nil;
end;

function TStringDictionary.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

procedure TStringDictionary.Remove(Key: String);
begin
  Self.Associations.Remove(Self.InternalFind(Key));
end;



//* TStringDictionary Private methods ******************************************

function TStringDictionary.AssociationAt(Index: Integer): TKeyValuePair;
begin
  Result := TKeyValuePair(Self.Associations[Index]);
end;

function TStringDictionary.InternalFind(Key: String): TKeyValuePair;
var
  C:          Integer;
  Middle:     Integer;
  StartIndex: Integer;
  EndIndex:   Integer;
  MiddlePair: TKeyValuePair;
begin
  Result := nil;

  Key := Lowercase(Key);
  StartIndex := 0;
  EndIndex   := Self.Count - 1;

  if (StartIndex > EndIndex) then Exit;

  C := 1;
  MiddlePair := nil;
  while (StartIndex <= EndIndex) do begin
    Middle := (EndIndex + StartIndex) div 2;
    MiddlePair := Self.AssociationAt(Middle);

    C := CompareStr(Lowercase(Key), Lowercase(MiddlePair.Key));

    if (C < 0) then
      EndIndex := Middle - 1
    else if (C > 0) then
      StartIndex := Middle + 1
    else
      Break;
  end;

  if (C = 0) and Assigned(MiddlePair) then
    Result := MiddlePair;
end;

procedure TStringDictionary.Sort;
begin
  Self.Associations.Sort(TKeyValuePairSort);
end;

end.
