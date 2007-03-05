unit StringDictionary;

interface

uses
  Contnrs;

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

    function InternalFind(Key: String): TKeyValuePair;
    procedure Sort; 
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Key, Value: String);
    procedure Clear;
    function  Count: Integer;
    function  Find(Key: String): String;
    function  HasKey(Key: String): Boolean;
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

procedure TStringDictionary.Clear;
begin
  Self.Associations.Clear;
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

procedure TStringDictionary.Remove(Key: String);
begin
  Self.Associations.Remove(Self.InternalFind(Key));
end;



//* TStringDictionary Private methods ******************************************

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
    MiddlePair := TKeyValuePair(Self.Associations[Middle]);

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
