unit TestIdRTP;

interface

uses
  IdRTPBase, IdRTP, TestFramework, TestFrameworkSip;

type
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
    procedure TestClear;
    procedure TestCount;
    procedure TestEncodingFor;
    procedure TestFirstFreePayloadType;
    procedure TestHasEncoding;
    procedure TestHasPayloadType;
    procedure TestInitializeOnEmptySdpPayload;
    procedure TestInitializeOnSingleMediaSdp;
    procedure TestIsFull;
    procedure TestPayloadTypeFor;
    procedure TestReservedEncodingMustntOverwriteOthers;
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
    procedure TestTransportDesc;
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

implementation

uses
  Classes, IdRtpServer, IdSdpParser, SysUtils;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdRTP unit tests');
  Result.AddTest(TestTIdRTPProfile.Suite);
  Result.AddTest(TestTIdAudioVisualProfile.Suite);
  Result.AddTest(TestTIdRTPPacket.Suite);
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

procedure TestTIdRTPProfile.TestClear;
begin
  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT + 1);

  Self.Profile.Clear;

  CheckEquals(0, Self.Profile.Count, 'Profile wasn''t cleared');
end;

procedure TestTIdRTPProfile.TestCount;
begin
  CheckEquals(0, Self.Profile.Count, 'Count on new profile');

  Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
  CheckEquals(1, Self.Profile.Count, 'Count after AddEncoding');
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

procedure TestTIdRTPProfile.TestFirstFreePayloadType;
begin
  CheckEquals(0, Self.Profile.FirstFreePayloadType, 'New (empty) profile');

  Self.Profile.AddEncoding(Self.T140Encoding, 1);
  CheckEquals(0,
              Self.Profile.FirstFreePayloadType,
              'Empty spaces before first assigned payload type');

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, 0);
  CheckEquals(2,
              Self.Profile.FirstFreePayloadType,
              'First couple of payload types assigned');
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

procedure TestTIdRTPProfile.TestInitializeOnEmptySdpPayload;
var
  PT:  TIdRTPPayloadType;
  SDP: TIdSdpPayload;
begin
  SDP := TIdSdpPayload.Create;
  try
    Self.Profile.InitializeOn(SDP);

    for PT := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do
      CheckEquals(TIdRTPNullEncoding.ClassName,
                  Self.Profile.EncodingFor(PT).ClassName,
                  'Encoding for payload type ' + IntToStr(PT));
  finally
    SDP.Free;
  end;
end;

procedure TestTIdRTPProfile.TestInitializeOnSingleMediaSdp;
var
  P:   TIdSdpParser;
  PT:  TIdRTPPayloadType;
  S:   TStringStream;
  SDP: TIdSdpPayload;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'm=text 11000 RTP/AVP 98'#13#10
                          + 'a=rtpmap:98 t140/1000');
  try
    P := TIdSdpParser.Create;
    try
      P.Source := S;

      SDP := TIdSdpPayload.Create;
      try
        P.Parse(SDP);

        Self.Profile.InitializeOn(SDP);

        for PT := Low(TIdRTPPayloadType) to 97 do
          CheckEquals(TIdRTPNullEncoding.ClassName,
                      Self.Profile.EncodingFor(PT).ClassName,
                      'Encoding for payload type ' + IntToStr(PT));

        CheckNotEquals(TIdRTPNullEncoding.ClassName,
                       Self.Profile.EncodingFor(98).ClassName,
                      'Encoding for payload type 98');

        for PT := 99 to High(TIdRTPPayloadType) do
          CheckEquals(TIdRTPNullEncoding.ClassName,
                      Self.Profile.EncodingFor(PT).ClassName,
                      'Encoding for payload type ' + IntToStr(PT));
      finally
        SDP.Free;
      end;
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdRTPProfile.TestIsFull;
var
  I:   TIdRTPPayloadType;
  Enc: TIdRTPEncoding;
begin
  Check(not Self.Profile.IsFull, 'Empty profile');

  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
    Enc := TIdRTPEncoding.Create('foo', I);
    try
      Self.Profile.AddEncoding(Enc, I);
    finally
      Enc.Free;
    end;
  end;
  Check(Self.Profile.IsFull, 'Full profile');
end;

procedure TestTIdRTPProfile.TestPayloadTypeFor;
begin
  try
    Self.Profile.PayloadTypeFor(Self.T140Encoding);
    Fail('"Payload Type" for unknown MIME type');
  except
    on ENoPayloadTypeFound do;
  end;

  Self.Profile.AddEncoding(Self.InterleavedT140Encoding, Self.ArbPT - 1);
  Self.Profile.AddEncoding(Self.T140Encoding,            Self.ArbPT);

  CheckEquals(Self.ArbPT - 1,
              Self.Profile.PayloadTypeFor(Self.InterleavedT140Encoding),
              'Payload Type for ' + InterleavedT140MimeType);

  CheckEquals(Self.ArbPT,
              Self.Profile.PayloadTypeFor(Self.T140Encoding),
              'Payload Type for ' + T140MimeType);
end;

procedure TestTIdRTPProfile.TestReservedEncodingMustntOverwriteOthers;
var
  Res: TIdRTPReservedEncoding;
begin
  Res := TIdRTPReservedEncoding.Create('', 0);
  try
    Self.Profile.AddEncoding(Self.T140Encoding, Self.ArbPT);
    Self.Profile.AddEncoding(Res, Self.ArbPT);
    Self.Profile.PayloadTypeFor(Self.T140Encoding);
  finally
    Res.Free;
  end;
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
  Self.CheckRange(1,  2);
  Self.CheckRange(19, 24);
  Self.CheckRange(27, 27);
  Self.CheckRange(29, 30);
  Self.CheckRange(35, 95);
end;

procedure TestTIdAudioVisualProfile.TestTransportDesc;
begin
  CheckEquals(AudioVisualProfile,
              Self.Profile.TransportDesc,
              'TransportDesc');
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

initialization
  RegisterTest('RTP Profiles and Packets', Suite);
end.
