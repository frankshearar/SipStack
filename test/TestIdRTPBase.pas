unit TestIdRTPBase;

interface

uses
  IdRTPBase, TestFramework;

type
  TestFunctions = class(TTestCase)
  published
    procedure TestEncodeAsStringCardinal;
    procedure TestEncodeAsStringWord;
    procedure TestHtoNL;
    procedure TestHtoNS;
    procedure TestNtoHL;
    procedure TestNtoHS;
    procedure TestReadCardinal;
    procedure TestReadRemainderOfStream;
    procedure TestReadRemainderOfStreamLong;
    procedure TestReadWord;
    procedure TestWriteCardinal;
    procedure TestWriteWord;
  end;

  TestTIdRTPEncoding = class(TTestCase)
  private
    Encoding: TIdRTPEncoding;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCreate;
    procedure TestCreateFromEncoding;
    procedure TestClone;
    procedure TestIsNull;
  end;

  TestTIdRTPNullEncoding = class(TTestCase)
  private
    Encoding: TIdRTPNullEncoding;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCreate;
    procedure TestCreateFromEncoding;
    procedure TestClone;
    procedure TestIsNull;
  end;

  TestTIdT140Payload = class(TTestCase)
  private
    Packet: TIdT140Payload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReadFrom;
    procedure TestPrintOn;
  end;

implementation

uses
  Classes, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTP unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdRTPEncoding.Suite);
  Result.AddTest(TestTIdRTPNullEncoding.Suite);
  Result.AddTest(TestTIdT140Payload.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

function ShowEncoded(S: String): String;
var
  I: Integer;
begin
  Result := '';

  for I := 1 to Length(S) do
    Result := Result + '#$' + IntToHex(Ord(S[I]), 2);
end;

procedure TestFunctions.TestEncodeAsStringCardinal;
begin
  CheckEquals('#$00#$00#$00#$00',
              ShowEncoded(EncodeAsString(Cardinal(0))),
              '0');

  CheckEquals('#$01#$02#$03#$04',
              ShowEncoded(EncodeAsString($01020304)),
              '$01020304');
end;

procedure TestFunctions.TestEncodeAsStringWord;
begin
  CheckEquals('#$00#$00',
              ShowEncoded(EncodeAsString(Word(0))),
              '0');

  CheckEquals('#$01#$02',
              ShowEncoded(EncodeAsString($0102)),
              '$0102');
end;

procedure TestFunctions.TestHtoNL;
begin
  CheckEquals(0,         HtoNL(0),         '0');
  CheckEquals($01020304, HtoNL($04030201), '$04030201');
end;

procedure TestFunctions.TestHtoNS;
begin
  CheckEquals(0,     HtoNS(0),     '0');
  CheckEquals($0102, HtoNS($0201), '$0201');
end;

procedure TestFunctions.TestNtoHL;
var
  I: Integer;
  N: Cardinal;
begin
  CheckEquals(0,         NtoHL(0),         '0');
  CheckEquals($01020304, NtoHL($04030201), '$04030201');

  for I := 1 to 1000 do begin
    N := Abs(Random(High(Integer))); // Random returns an Integer
    CheckEquals(N, HtoNL(NtoHL(N)), IntToStr(N));
  end;
end;

procedure TestFunctions.TestNtoHS;
var
  I: Integer;
  N: Word;
begin
  CheckEquals(0,     NtoHS(0),      '0');
  CheckEquals($0304, NtoHS($0403), '$0403');

  for I := 1 to 1000 do begin
    N := Abs(Random(High(Word))); // Random returns an Integer
    CheckEquals(N, HtoNS(NtoHS(N)), IntToStr(N));
  end;
end;

procedure TestFunctions.TestReadCardinal;
var
  S: TStringStream;
  C: Cardinal;
begin
  S := TStringStream.Create(#$11#$22#$33#$44);
  try
    C := ReadCardinal(S);
    CheckEquals($11223344, C, 'Cardinal incorrectly read - check byte order');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestReadRemainderOfStream;
var
  C: Char;
  S: TStringStream;
begin
  S := TStringStream.Create('1234567890');
  try
    S.Read(C, 1);
    S.Read(C, 1);
    S.Read(C, 1);

    CheckEquals('4567890', ReadRemainderOfStream(S), 'ReadRemainderOfStream');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestReadRemainderOfStreamLong;
var
  C:   Char;
  S:   TStringStream;
  Src: String;
begin
  while (Length(Src) < 1000) do
    Src := Src + '0123456789';

  S := TStringStream.Create(Src);
  try
    S.Read(C, 1);
    S.Read(C, 1);
    S.Read(C, 1);

    CheckEquals(Length(Src) - 3,
                Length(ReadRemainderOfStream(S)),
                'ReadRemainderOfStream, length of remainder');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestReadWord;
var
  S: TStringStream;
  W: Word;
begin
  S := TStringStream.Create(#$11#$22);
  try
    W := ReadWord(S);
    CheckEquals($1122, W, 'Word incorrectly read - check byte order');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestWriteCardinal;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    WriteCardinal(S, $11223344);

    CheckEquals(#$11#$22#$33#$44,
                S.DataString,
                'Cardinal incorrectly written - check byte order');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestWriteWord;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    WriteWord(S, $1122);
    CheckEquals(#$11#$22,
                S.DataString,
                'Word incorrectly written - check byte order');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdRTPEncoding                                                         *
//******************************************************************************
//* TestTIdRTPEncoding Public methods ******************************************

procedure TestTIdRTPEncoding.SetUp;
begin
  inherited SetUp;

  Self.Encoding := TIdRTPEncoding.Create('t140', 1000, '1');
end;

procedure TestTIdRTPEncoding.TearDown;
begin
  Self.Encoding.Free;

  inherited TearDown;
end;

//* TestTIdRTPEncoding Published methods ***************************************

procedure TestTIdRTPEncoding.TestAsString;
var
  Enc: TIdRTPEncoding;
begin
  CheckEquals('t140/1000/1',
              Self.Encoding.AsString,
              'AsString with Parameters');

  Enc := TIdRTPEncoding.Create('red', 8000);
  try
    CheckEquals('red/8000', Enc.AsString, 'AsString without Parameters');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestCreate;
begin
  CheckEquals(1000,   Self.Encoding.ClockRate,  'ClockRate');
  CheckEquals('t140', Self.Encoding.Name,       'Name');
  CheckEquals('1',    Self.Encoding.Parameters, 'Parameters');
end;

procedure TestTIdRTPEncoding.TestCreateFromEncoding;
var
  Enc: TIdRTPEncoding;
begin
  Enc := TIdRTPEncoding.Create(Self.Encoding);
  try
    Check(Enc.IsEqualTo(Self.Encoding), 'Properties not copied correctly');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestClone;
var
  Enc: TIdRTPEncoding;
begin
  Enc := Self.Encoding.Clone;
  try
    CheckEquals(TIdRTPEncoding.ClassName,
                Enc.ClassName,
                'Incorrect type used for cloning');
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPEncoding.TestIsNull;
begin
  Check(not Self.Encoding.IsNull, 'non-Null Encoding marked as null');
end;

//******************************************************************************
//* TestTIdRTPNullEncoding                                                     *
//******************************************************************************
//* TestTIdRTPNullEncoding Public methods **************************************

procedure TestTIdRTPNullEncoding.SetUp;
begin
  inherited SetUp;

  Self.Encoding := TIdRTPNullEncoding.Create('t140', 1000, '1');
end;

procedure TestTIdRTPNullEncoding.TearDown;
begin
  Self.Encoding.Free;

  inherited TearDown;
end;

//* TestTIdRTPNullEncoding Published methods ***********************************

procedure TestTIdRTPNullEncoding.TestAsString;
begin
  CheckEquals('', Self.Encoding.AsString, 'AsString for Null Encoding');
end;

procedure TestTIdRTPNullEncoding.TestCreate;
begin
  CheckEquals(0,  Self.Encoding.ClockRate,  'ClockRate');
  CheckEquals('', Self.Encoding.Name,       'Name');
  CheckEquals('', Self.Encoding.Parameters, 'Parameters');
end;

procedure TestTIdRTPNullEncoding.TestCreateFromEncoding;
var
  Enc:  TIdRTPEncoding;
  Null: TIdRTPNullEncoding;
begin
  Enc := TIdRTPEncoding.Create('t140', 1000, '1');
  try
    Null := TIdRTPNullEncoding.Create(Enc);
    try
      Check(Null.IsEqualTo(Self.Encoding), 'Properties not copied correctly');
    finally
      Null.Free;
    end;
  finally
    Enc.Free;
  end;
end;

procedure TestTIdRTPNullEncoding.TestClone;
var
  Null: TIdRTPEncoding;
begin
  Null := Self.Encoding.Clone;
  try
    CheckEquals(TIdRTPNullEncoding.ClassName,
                Null.ClassName,
                'Incorrect type used for cloning');
  finally
    Null.Free;
  end;
end;

procedure TestTIdRTPNullEncoding.TestIsNull;
begin
  Check(Self.Encoding.IsNull, 'Null Encoding not marked as null');
end;

//******************************************************************************
//* TestTIdT140Payload                                                         *
//******************************************************************************
//* TestTIdT140Payload Public methods ******************************************

procedure TestTIdT140Payload.SetUp;
begin
  inherited SetUp;

  Self.Packet := TIdT140Payload.Create;
end;

procedure TestTIdT140Payload.TearDown;
begin
  Self.Packet.Free;

  inherited TearDown;
end;

//* TestTIdT140Payload Published methods ***************************************

procedure TestTIdT140Payload.TestReadFrom;
var
  S: TStringStream;
begin
  S := TStringStream.Create(#$BE#$EF'fooing the bar');
  try
    Self.Packet.ReadFrom(S);

    CheckEquals($BEEF,            Self.Packet.BlockCount, 'BlockCount');
    CheckEquals('fooing the bar', Self.Packet.Block,      'Block');
  finally
    S.Free;
  end;
end;

procedure TestTIdT140Payload.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Packet.BlockCount := $BEEF;
    Self.Packet.Block      := 'fooing the bar';

    Self.Packet.PrintOn(S);

    CheckEquals(#$BE#$EF'fooing the bar',
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

initialization
  RegisterTest('RTP base classes', Suite);
end.
