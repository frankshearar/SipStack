unit TestStringDictionary;

interface

uses
  StringDictionary, TestFramework;

type
  TestTStringDictionary = class(TTestCase)
  private
    D: TStringDictionary;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestClear;
    procedure TestFind;
    procedure TestHasKey;
    procedure TestRemove;
  end;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('StringDictionary unit tests');
  Result.AddTest(TestTStringDictionary.Suite);
end;

//******************************************************************************
//* TestTStringDictionary                                                      *
//******************************************************************************
//* TestTStringDictionary Public methods ***************************************

procedure TestTStringDictionary.SetUp;
begin
  inherited SetUp;

  Self.D := TStringDictionary.Create;
end;

procedure TestTStringDictionary.TearDown;
begin
  Self.D.Free;

  inherited TearDown;
end;

//* TestTStringDictionary Published methods ************************************

procedure TestTStringDictionary.TestAddAndCount;
begin
  CheckEquals(0, Self.D.Count, 'Initially empty');

  Self.D.Add('foo', 'bar');
  CheckEquals(1, Self.D.Count, 'After one add');

  Self.D.Add('foo', 'baz');
  CheckEquals(1, Self.D.Count, 'Key already present');

  Self.D.Add('quaax', 'bar');
  CheckEquals(2, Self.D.Count, 'Two Adds (of unique keys)');
end;

procedure TestTStringDictionary.TestClear;
begin
  Self.D.Add('foo', '1');
  Self.D.Add('bar', '2');
  Self.D.Add('baz', '3');
  Self.D.Add('quaax', '4');

  Self.D.Clear;

  CheckEquals(0, Self.D.Count, 'Dictionary not cleared');
end;

procedure TestTStringDictionary.TestFind;
begin
  Self.D.Add('foo', '1');
  Self.D.Add('bar', '2');
  Self.D.Add('baz', '3');
  Self.D.Add('quaax', '4');

  CheckEquals('1', Self.D.Find('foo'),   '"foo"''s value');
  CheckEquals('2', Self.D.Find('bar'),   '"bar"''s value');
  CheckEquals('3', Self.D.Find('baz'),   '"baz"''s value');
  CheckEquals('4', Self.D.Find('quaax'), '"quaax"''s value');

  CheckEquals('', Self.D.Find('not in the dictionary'), 'Unknown key must return the empty string');
end;

procedure TestTStringDictionary.TestHasKey;
begin
  Check(not Self.D.HasKey('foo'), '"foo" present in an empty list?');

  Self.D.Add('foo', 'bar');
  Check(Self.D.HasKey('foo'), '"foo" not added');

  Self.D.Remove('foo');
  Check(not Self.D.HasKey('foo'), '"foo" not removed');
end;

procedure TestTStringDictionary.TestRemove;
begin
  Self.D.Add('foo', 'bar');
  Self.D.Remove('foo');

  Check(not Self.D.HasKey('foo'), '"foo" not removed');
end;

initialization
  RegisterTest('Dictionary tests', Suite);
end.
