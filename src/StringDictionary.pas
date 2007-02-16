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

implementation

uses
  SysUtils;

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
  if not Self.HasKey(Key) then
    Self.Associations.Add(TKeyValuePair.Create(Key, Value));
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
  I: Integer;
begin
  Result := nil;

  for I := 0 to Self.Count - 1 do begin
    if Lowercase(TKeyValuePair(Self.Associations[I]).Key) = Lowercase(Key) then begin
      Result := TKeyValuePair(Self.Associations[I]);
      Break;
    end;
  end;
end;

end.
