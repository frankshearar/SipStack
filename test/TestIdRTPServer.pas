unit TestIdRTPServer;

interface

uses
  Classes, IdRTPServer, IdSocketHandle, IdUDPClient, TestFramework,
  TestFrameworkEx;

type
  TTestRTP = class(TThreadingTestCase)
  public
    procedure CheckHasEqualHeaders(const Expected, Received: TIdRTPPacket);
  end;

  TestFunctions = class(TTestCase)
  published
    procedure TestEncodeAsStringCardinal;
    procedure TestEncodeAsStringWord;
    procedure TestHtoNL;
    procedure TestHtoNS;
    procedure TestNtoHL;
    procedure TestNtoHS;
    procedure TestReadCardinal;
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
    procedure TestCreate;
    procedure TestCreateFromEncoding;
    procedure TestAsString;
    procedure TestIsNull;
  end;

  TestTIdRTPNullEncoding = class(TTestCase)
  private
    Encoding: TIdRTPNullEncoding;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestCreateFromEncoding;
    procedure TestAsString;
    procedure TestIsNull;
  end;

  TestTIdRTPProfile = class(TTestCase)
  private
    ArbPT:                   TIdRTPPayloadType;
    Profile:                 TIdRTPProfile;
    T140Encoding:            TIdRTPEncoding;
    InterleavedT140Encoding: TIdRTPEncoding;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddEncoding;
    procedure TestCount;
    procedure TestHasEncoding;
    procedure TestHasPayloadType;
    procedure TestEncodingFor;
    procedure TestPayloadTypeFor;
  end;

  TestTIdAudioVisualProfile = class(TTestCase)
  private
    Profile: TIdAudioVisualProfile;

    procedure CheckRange(StartType, EndType: TIdRTPPayloadType);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefinedPayloads;
    procedure TestDynamicPayloadTypes;
    procedure TestReservedAndUnassignedPayloadTypes;
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

  TestTIdRTPPacket = class(TTestRTP)
  private
    AVP:    TIdAudioVisualProfile;
    Packet: TIdRTPPacket;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrintOn;
    procedure TestReadFromContributingSourceIdentifiers;
    procedure TestReadFromCsrcCount;
    procedure TestReadFromHasExtension;
    procedure TestReadFromHasPadding;
    procedure TestReadFromIsMarker;
    procedure TestReadFromPayloadType;
    procedure TestReadFromSequenceNo;
    procedure TestReadFromSynchronizationSourceIdentifier;
    procedure TestReadFromTimestamp;
    procedure TestReadFromVersion;
  end;

  TestTIdRTPServer = class(TTestRTP)
  private
    Client: TIdUDPClient;
    Packet: TIdRTPPacket;
    Server: TIdRTPServer;
    procedure CheckReceivePacket(Sender: TObject;
                                 AData: TStream;
                                 ABinding: TIdSocketHandle);
    procedure CheckOnRTPRead(Sender: TObject;
                             APacket: TIdRTPPacket;
                             ABinding: TIdSocketHandle);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOnRTPRead;
    procedure TestReceivePacket;
  end;

const
  DefaultTimeout = 5000; // ms

implementation

uses
  IdSipConsts, SyncObjs, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTPServer unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdRTPEncoding.Suite);
  Result.AddTest(TestTIdRTPNullEncoding.Suite);
  Result.AddTest(TestTIdRTPProfile.Suite);
  Result.AddTest(TestTIdAudioVisualProfile.Suite);
  Result.AddTest(TestTIdT140Payload.Suite);
  Result.AddTest(TestTIdRTPPacket.Suite);
  Result.AddTest(TestTIdRTPServer.Suite);
end;

//******************************************************************************
//* TTestRTP                                                                   *
//******************************************************************************
//* TTestRTP Public methods ****************************************************

procedure TTestRTP.CheckHasEqualHeaders(const Expected, Received: TIdRTPPacket);
var
  I: Integer;
begin
  CheckEquals(Expected.Version,     Received.Version,     'Version');
  CheckEquals(Expected.HasPadding,  Received.HasPadding,  'HasPadding');
  CheckEquals(Expected.CsrcCount,   Received.CsrcCount,   'CSRC count');
  CheckEquals(Expected.IsMarker,    Received.IsMarker,    'IsMarker');
  CheckEquals(Expected.PayloadType, Received.PayloadType, 'PayloadType');
  CheckEquals(Expected.SequenceNo,  Received.SequenceNo,  'SequenceNo');
  CheckEquals(Expected.Timestamp,   Received.Timestamp,   'Timestamp');
  CheckEquals(Expected.SyncSrcID,   Received.SyncSrcID,   'SSRC ID');

  for I := 0 to Expected.CsrcCount - 1 do
    CheckEquals(Integer(Expected.CsrcIDs[I]),
                Integer(Received.CsrcIDs[I]),
                IntToStr(I) + 'th CSRC ID');
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

procedure TestTIdRTPNullEncoding.TestAsString;
begin
  CheckEquals('', Self.Encoding.AsString, 'AsString for Null Encoding');
end;

procedure TestTIdRTPNullEncoding.TestIsNull;
begin
  Check(Self.Encoding.IsNull, 'Null Encoding not marked as null');
end;

//******************************************************************************
//* TestTIdRTPProfile                                                          *
//******************************************************************************
//* TestTIdRTPProfile Public methods *******************************************

procedure TestTIdRTPProfile.SetUp;
begin
  inherited SetUp;

  Self.ArbPT := 98;

  Self.InterleavedT140Encoding := TIdRTPEncoding.Create('red', 8000);
  Self.Profile                 := TIdRTPProfile.Create;
  Self.T140Encoding            := TIdRTPEncoding.Create('t140', 1000);
end;

procedure TestTIdRTPProfile.TearDown;
begin
  Self.T140Encoding.Free;
  Self.Profile.Free;
  Self.InterleavedT140Encoding.Free;

  inherited TearDown;
end;

//* TestTIdRTPProfile Published methods ****************************************

procedure TestTIdRTPProfile.TestAddEncoding;
begin
  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  CheckEquals(1, Self.Profile.Count, 'New MIME type not added');

  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT - 1);
  CheckEquals(1,
              Self.Profile.Count,
              'No duplication of MIME type allowed');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT);
  CheckEquals(1,
              Self.Profile.Count,
              'No duplication of Payload Type allowed');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT - 1);
  CheckEquals(2, Self.Profile.Count, 'New, different, MIME type not added');
end;

procedure TestTIdRTPProfile.TestCount;
begin
  CheckEquals(0, Self.Profile.Count, 'Count on new profile');

  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  CheckEquals(1, Self.Profile.Count, 'Count after AddEncoding');
end;

procedure TestTIdRTPProfile.TestHasEncoding;
begin
  Check(not Self.Profile.HasEncoding(Self.T140Encoding), 'New profile');

  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  Check(Self.Profile.HasEncoding(Self.T140Encoding), 'After Add');
end;

procedure TestTIdRTPProfile.TestHasPayloadType;
begin
  Check(not Self.Profile.HasPayloadType(Self.ArbPT), 'New profile');

  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  Check(Self.Profile.HasPayloadType(Self.ArbPT), 'After Add');
end;

procedure TestTIdRTPProfile.TestEncodingFor;
begin
  CheckEquals('',
              Self.Profile.EncodingFor(Self.ArbPT).AsString,
              '"MIME type" for unknown Payload Type');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT - 1);
  Self.Profile.AddEncoding(Self.T140Encoding,            Self.ArbPT);

  CheckEquals(Self.InterleavedT140Encoding.AsString,
              Self.Profile.EncodingFor(Self.ArbPT - 1).AsString,
              'MIME type for ' + InterleavedT140MimeType);

  CheckEquals(Self.T140Encoding.AsString,
              Self.Profile.EncodingFor(Self.ArbPT).AsString,
              'MIME type for ' + T140MimeType);
end;

procedure TestTIdRTPProfile.TestPayloadTypeFor;
begin
  CheckEquals(High(TIdRTPPayloadType),
              Self.Profile.PayloadTypeFor(Self.T140Encoding),
              '"Payload Type" for unknown MIME type');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT - 1);
  Self.Profile.AddEncoding(Self.T140Encoding,            Self.ArbPT);

  CheckEquals(Self.ArbPT - 1,
              Self.Profile.PayloadTypeFor(Self.InterleavedT140Encoding),
              'Payload Type for ' + InterleavedT140MimeType);

  CheckEquals(Self.ArbPT,
              Self.Profile.PayloadTypeFor(Self.T140Encoding),
              'Payload Type for ' + T140MimeType);
end;

//******************************************************************************
//* TestTIdAudioVisualProfile                                                  *
//******************************************************************************
//* TestTIdAudioVisualProfile Public methods ***********************************

procedure TestTIdAudioVisualProfile.SetUp;
begin
  inherited SetUp;

  Self.Profile := TIdAudioVisualProfile.Create;
end;

procedure TestTIdAudioVisualProfile.TearDown;
begin
  Self.Profile.Free;

  inherited TearDown;
end;

//* TIdAudioVisualProfile Private methods **************************************

procedure TestTIdAudioVisualProfile.CheckRange(StartType, EndType: TIdRTPPayloadType);
var
  I:            TIdRTPPayloadType;
  PayloadCount: Integer;
  TestEncoding: TIdRTPEncoding;
begin
  PayloadCount := Self.Profile.Count;

  TestEncoding := TIdRTPEncoding.Create('arb values', 0);
  try
    for I := StartType to EndType do begin
      Self.Profile.AddEncoding(TestEncoding, I);
      Check(PayloadCount = Self.Profile.Count,
            'New encoding was added for a reserved/unassigned Payload Type '
          + IntToStr(I));
    end;
  finally
    TestEncoding.Free
  end;
end;

//* TIdAudioVisualProfile Published methods ************************************

procedure TestTIdAudioVisualProfile.TestDefinedPayloads;
begin
  CheckEquals('PCMU/8000',    Self.Profile.EncodingFor(0).AsString,   '0');
  CheckEquals('GSM/8000',     Self.Profile.EncodingFor(3).AsString,   '3');
  CheckEquals('G723/8000',    Self.Profile.EncodingFor(4).AsString,   '4');
  CheckEquals('DVI4/8000',    Self.Profile.EncodingFor(5).AsString,   '5');
  CheckEquals('DVI4/16000',   Self.Profile.EncodingFor(6).AsString,   '6');
  CheckEquals('LPC/8000',     Self.Profile.EncodingFor(7).AsString,   '7');
  CheckEquals('PCMA/8000',    Self.Profile.EncodingFor(8).AsString,   '8');
  CheckEquals('G722/8000',    Self.Profile.EncodingFor(9).AsString,   '9');
  CheckEquals('L16/44100/2',  Self.Profile.EncodingFor(10).AsString, '10');
  CheckEquals('L16/44100/1',  Self.Profile.EncodingFor(11).AsString, '11');
  CheckEquals('QCELP/8000',   Self.Profile.EncodingFor(12).AsString, '12');
  CheckEquals('CN/8000',      Self.Profile.EncodingFor(13).AsString, '13');
  CheckEquals('MPA/90000',    Self.Profile.EncodingFor(14).AsString, '14');
  CheckEquals('G728/8000',    Self.Profile.EncodingFor(15).AsString, '15');
  CheckEquals('DVI4/11025',   Self.Profile.EncodingFor(16).AsString, '16');
  CheckEquals('DVI4/22050',   Self.Profile.EncodingFor(17).AsString, '17');
  CheckEquals('G729/8000',    Self.Profile.EncodingFor(18).AsString, '18');

  CheckEquals('CelB/90000',   Self.Profile.EncodingFor(25).AsString, '25');
  CheckEquals('JPEG/90000',   Self.Profile.EncodingFor(26).AsString, '26');

  CheckEquals('nv/90000',     Self.Profile.EncodingFor(28).AsString, '28');

  CheckEquals('H261/90000',   Self.Profile.EncodingFor(31).AsString, '31');
  CheckEquals('MPV/90000',    Self.Profile.EncodingFor(32).AsString, '32');
  CheckEquals('MP2T/90000',   Self.Profile.EncodingFor(33).AsString, '33');
  CheckEquals('H263/90000',   Self.Profile.EncodingFor(34).AsString, '34');
end;

procedure TestTIdAudioVisualProfile.TestDynamicPayloadTypes;
var
  I:            TIdRTPPayloadType;
  PayloadCount: Integer;
  TestEncoding: TIdRTPEncoding;
begin
  for I := 96 to High(TIdRTPPayloadType) do begin
    TestEncoding := TIdRTPEncoding.Create('arb values', I);
    try
      PayloadCount := Self.Profile.Count;

      Self.Profile.AddEncoding(TestEncoding, I);
      Check(PayloadCount + 1 = Self.Profile.Count,
            'New encoding wasn''t added for a dynamic Payload Type '
          + IntToStr(I));
    finally
      TestEncoding.Free
    end;
  end;
end;

procedure TestTIdAudioVisualProfile.TestReservedAndUnassignedPayloadTypes;
begin
  CheckRange(1,  2);
  CheckRange(19, 24);
  CheckRange(29, 30);
  CheckRange(35, 95);
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

//******************************************************************************
//* TestTIdRTPPacket                                                           *
//******************************************************************************
//* TestTIdRTPPacket Public methods ********************************************

procedure TestTIdRTPPacket.SetUp;
begin
  inherited SetUp;

  Self.AVP := TIdAudioVisualProfile.Create;
  Self.Packet := TIdRTPPacket.Create(Self.AVP);
end;

procedure TestTIdRTPPacket.TearDown;
begin
  Self.Packet.Free;
  Self.AVP.Free;

  inherited TearDown;
end;

//* TestTIdRTPPacket Published methods *****************************************

procedure TestTIdRTPPacket.TestPrintOn;
var
  P:   TIdRTPPacket;
  S:   TStringStream;
  Srv: TIdRTPServer;
begin
  Self.Packet.Version      := 2;
  Self.Packet.HasPadding   := true;
  Self.Packet.HasExtension := false;
  Self.Packet.CsrcCount    := 2;
  Self.Packet.CsrcIDs[0]   := $CAFEBABE;
  Self.Packet.CsrcIDs[1]   := $DEADBEEF;
  Self.Packet.IsMarker     := true;
  Self.Packet.PayloadType  := $0D;
  Self.Packet.SequenceNo   := $0102;
  Self.Packet.Timestamp    := $0A0B0C0D;
  Self.Packet.SyncSrcID    := $10203040;

  S := TStringStream.Create('');
  try
    Srv := TIdRTPServer.Create(nil);
    try
      Self.Packet.PrintOn(S);
      S.Seek(0, soFromBeginning);

      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        Self.CheckHasEqualHeaders(Self.Packet, P);
      finally
        P.Free;
      end;
    finally
      Srv.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromContributingSourceIdentifiers;
var
  I:      Integer;
  IDs:    TIdCardinalArray;
  J:      Integer;
  Packet: String;
  P:      TIdRTPPacket;
  S:      TStream;
begin
  for I := Low(TIdRTPCsrcCount) to High(TIdRTPCsrcCount) do begin
    SetLength(IDs, I);
    Packet := Chr(I) + #$00#$00#$00
                 + #$00#$00#$00#$00  // timestamp
                 + #$00#$00#$00#$00; // SSRC ID


    for J := 0 to I -1 do begin
      IDs[J] := Random(High(Integer));
      Packet := Packet + EncodeAsString(IDs[J]);
    end;

    S := TStringStream.Create(Packet);
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(I, P.CsrcCount, 'CsrcCount');

        for J := 0 to P.CsrcCount - 1 do
          CheckEquals(IDs[J],
                      P.CsrcIDs[J],
                      'CSRC ID #' + IntToStr(J));
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromCsrcCount;
var
  I: Integer;
  P: TIdRTPPacket;
  S: TStream;
begin
  for I := Low(TIdRTPCsrcCount) to High(TIdRTPCsrcCount) do begin
    S := TStringStream.Create(Chr(I));
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(I, P.CsrcCount, 'CsrcCount ' + IntToStr(I));
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromHasExtension;
var
  P: TIdRTPPacket;
  S: TStream;
begin
  S := TStringStream.Create(#$10);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(P.HasExtension, 'HasExtension set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(not P.HasExtension, 'HasExtension not set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromHasPadding;
var
  P: TIdRTPPacket;
  S: TStream;
begin
  S := TStringStream.Create(#$20);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(P.HasPadding, 'HasPadding set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(not P.HasPadding, 'HasPadding not set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromIsMarker;
var
  P: TIdRTPPacket;
  S: TStream;
begin
  S := TStringStream.Create(#$00#$80);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(P.IsMarker, 'IsMarker set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;

  S := TStringStream.Create(#$00#$00);
  try
    P := TIdRTPPacket.Create(Self.AVP);
    try
      P.ReadFrom(S);
      Check(not P.IsMarker, 'IsMarker not set');
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromPayloadType;
var
  I: Integer;
  P: TIdRTPPacket;
  S: TStream;
begin
  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
    S := TStringStream.Create(#$00 + Chr(I));
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(I, P.PayloadType, 'PayloadType ' + IntToStr(I));
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromSequenceNo;
var
  I: Word;
  P: TIdRTPPacket;
  S: TStringStream;
begin
  for I := Low(TIdRTPSequenceNo) to High(TIdRTPSequenceNo) do begin
    S := TStringStream.Create(#$00#$00 + EncodeAsString(I));
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(I,
                    P.SequenceNo,
                    'SequenceNo ' + IntToStr(I));
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromSynchronizationSourceIdentifier;
var
  I:    Integer;
  P:    TIdRTPPacket;
  S:    TStream;
  SSRC: Cardinal;
begin
  for I := 1 to 1000 do begin
    SSRC := Abs(Random(High(Integer)));
    S := TStringStream.Create(#$00#$00#$00#$00
                            + #$00#$00#$00#$00
                            + EncodeAsString(SSRC));
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(SSRC,
                    P.SyncSrcID,
                    'Synchronization SouRCe identifier, ' + IntToStr(I) + 'th test');
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromTimestamp;
var
  I:         Integer;
  P:         TIdRTPPacket;
  S:         TStream;
  Timestamp: Cardinal;
begin
  for I := 1 to 1000 do begin
    Timestamp := Abs(Random(High(Integer)));
    S := TStringStream.Create(#$00#$00#$00#$00
                            + EncodeAsString(Timestamp));
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(Timestamp,
                    P.Timestamp,
                    'Timestamp, ' + IntToStr(I) + 'th test');
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TestTIdRTPPacket.TestReadFromVersion;
var
  I: Integer;
  P: TIdRTPPacket;
  S: TStream;
begin
  for I := Low(TIdRTPVersion) to High(TIdRTPVersion) do begin
    S := TStringStream.Create(Chr(I shl 6));
    try
      P := TIdRTPPacket.Create(Self.AVP);
      try
        P.ReadFrom(S);
        CheckEquals(I, P.Version, 'Version ' + IntToStr(I));
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

//******************************************************************************
//* TestTIdRTPServer                                                           *
//******************************************************************************
//* TestTIdRTPServer Public methods ********************************************

procedure TestTIdRTPServer.SetUp;
begin
  inherited SetUp;

  Self.Server := TIdRTPServer.Create(nil);
  Self.Server.DefaultPort := 5004;

  Self.Packet := TIdRTPPacket.Create(Self.Server.Profile);
  Self.Packet.Version      := 2;
  Self.Packet.HasPadding   := true;
  Self.Packet.HasExtension := false;
  Self.Packet.CsrcCount    := 2;
  Self.Packet.CsrcIDs[0]   := $CAFEBABE;
  Self.Packet.CsrcIDs[1]   := $DEADBEEF;
  Self.Packet.IsMarker     := true;
  Self.Packet.PayloadType  := $0D;
  Self.Packet.SequenceNo   := $0102;
  Self.Packet.Timestamp    := $0A0B0C0D;
  Self.Packet.SyncSrcID    := $10203040;

  Self.Client := TIdUDPClient.Create(nil);
  Self.Client.Host := '127.0.0.1';
  Self.Client.Port := Self.Server.DefaultPort;
end;

procedure TestTIdRTPServer.TearDown;
begin
  Self.Client.Free;
  Self.Packet.Free;
  Self.Server.Free;

  inherited TearDown;
end;

//* TestTIdRTPServer Private methods *******************************************

procedure TestTIdRTPServer.CheckReceivePacket(Sender: TObject;
                                              AData: TStream;
                                              ABinding: TIdSocketHandle);
var
  P: TIdRTPPacket;
begin
  try
    P := TIdRTPPacket.Create(Self.Server.Profile);
    try
      P.ReadFrom(AData);
      Self.CheckHasEqualHeaders(Self.Packet, P);
    finally
      P.Free;
    end;

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

procedure TestTIdRTPServer.CheckOnRTPRead(Sender: TObject;
                                          APacket: TIdRTPPacket;
                                          ABinding: TIdSocketHandle);
begin
  try
    Self.CheckHasEqualHeaders(Self.Packet, APacket);

    Self.ThreadEvent.SetEvent;
  except
    on E: Exception do begin
      Self.ExceptionType    := ExceptClass(E.ClassType);
      Self.ExceptionMessage := E.Message;
    end;
  end;
end;

//* TestTIdRTPServer Published methods *****************************************

procedure TestTIdRTPServer.TestOnRTPRead;
var
  S: TStringStream;
begin
  Self.Server.OnRTPRead := Self.CheckOnRTPRead;
  Self.Server.Active := true;
  try
    S := TStringStream.Create('');
    try
      Self.Packet.PrintOn(S);

      Self.Client.Send(S.DataString);

      if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
        raise Self.ExceptionType.Create(Self.ExceptionMessage);
    finally
      S.Free;
    end;
  finally
    Self.Server.Active := false;
  end;
end;

procedure TestTIdRTPServer.TestReceivePacket;
var
  S: TStringStream;
begin
  Self.Server.OnUDPRead := Self.CheckReceivePacket;
  Self.Server.Active := true;
  try
    S := TStringStream.Create('');
    try
      Self.Packet.PrintOn(S);

      Self.Client.Send(S.DataString);

      if (wrSignaled <> Self.ThreadEvent.WaitFor(DefaultTimeout)) then
        raise Self.ExceptionType.Create(Self.ExceptionMessage);
    finally
      S.Free;
    end;
  finally
    Self.Server.Active := false;
  end;
end;

initialization
  RegisterTest('IdRTPServer', Suite);
end.
