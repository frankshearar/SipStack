unit IdRTP;

interface

uses
  Classes, IdRTPBase, IdSdpParser;

type
  // I am a 1-1 association map between encodings and RTP Payload Types. RTP
  // packets use me to determine how their payload should be interpreted.
  TIdRTPProfile = class(TObject)
  private
    Encodings:    TIdPayloadArray;
    NullEncoding: TIdRTPEncoding;
    Reserved:     TIdRTPEncoding;

    function  EncodingAt(const PayloadType: TIdRTPPayloadType): TIdRTPEncoding;
    function  IndexOfEncoding(const Encoding: TIdRTPEncoding): Integer;
    procedure Initialize;
    procedure RemoveEncoding(const PayloadType: TIdRTPPayloadType);
  protected
    procedure AddEncodingAsReference(Encoding: TIdRTPEncoding;
                                     const PayloadType: TIdRTPPayloadType);

    procedure ReservePayloadType(const PayloadType: TIdRTPPayloadType);
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddEncoding(const Encoding: TIdRTPEncoding;
                          const PayloadType: TIdRTPPayloadType);
    procedure Clear;
    function  Count: Integer;
    function  FirstFreePayloadType: TIdRTPPayloadType;
    procedure InitializeOn(const SDP: TIdSdpPayload);
    function  IsFull: Boolean;
    function  HasEncoding(const Encoding: TIdRTPEncoding): Boolean;
    function  HasPayloadType(const PayloadType: TIdRTPPayloadType): Boolean;
    function  EncodingFor(const PayloadType: TIdRTPPayloadType): TIdRTPEncoding;
    function  PayloadTypeFor(const Encoding: TIdRTPEncoding): TIdRTPPayloadType;
    function  TransportDesc: String; virtual;
  end;

  // I am the profile defined in RFC 3551
  TIdAudioVisualProfile = class(TIdRTPProfile)
  private
    procedure ReserveRange(const LowPT, HighPT: TIdRTPPayloadType);
  public
    constructor Create; override;

    function  TransportDesc: String; override;
  end;

  // I am a packet of the Real-time Transport Protocol.
  //
  // I currently do not support the Header Extension as defined in RFC 3550,
  // section 5.3.1
  TIdRTPPacket = class(TObject)
  private
    fCsrcCount:    TIdRTPCsrcCount;
    fCsrcIDs:      TIdCardinalArray;
    fHasExtension: Boolean;
    fHasPadding:   Boolean;
    fIsMarker:     Boolean;
    fPayload:      TIdRTPPayload;
    fPayloadType:  TIdRTPPayloadType;
    fSequenceNo:   TIdRTPSequenceNo;
    fSyncSrcID:    Cardinal;
    fTimestamp:    TIdNTPTimestamp;
    fVersion:      TIdRTPVersion;
    Profile:       TIdRTPProfile;

    procedure CreatePayload(const Encoding: TIdRTPEncoding);
    function  GetCsrcCount: TIdRTPCsrcCount;
    function  GetCsrcID(Index: TIdRTPCsrcCount): Cardinal;
    procedure SetCsrcCount(const Value: TIdRTPCsrcCount);
    procedure SetCsrcID(Index: TIdRTPCsrcCount; const Value: Cardinal);
  public
    constructor Create(const Profile: TIdRTPProfile);
    destructor  Destroy; override;

    function  DefaultVersion: TIdRTPVersion;
    procedure ReadFrom(Src: TStream);
    procedure PrintOn(Dest: TStream);
    procedure ReadPayload(Src: TStream); overload;
    procedure ReadPayload(Src: String); overload;

    property CsrcCount:                       TIdRTPCsrcCount   read GetCsrcCount write SetCsrcCount;
    property CsrcIDs[Index: TIdRTPCsrcCount]: Cardinal          read GetCsrcID write SetCsrcID;
    property HasExtension:                    Boolean           read fHasExtension write fHasExtension;
    property HasPadding:                      Boolean           read fHasPadding write fHasPadding;
    property IsMarker:                        Boolean           read fIsMarker write fIsMarker;
    property Payload:                         TIdRTPPayload     read fPayload;
    property PayloadType:                     TIdRTPPayloadType read fPayloadType write fPayloadType;
    property SequenceNo:                      TIdRTPSequenceNo  read fSequenceNo write fSequenceNo;
    property SyncSrcID:                       Cardinal          read fSyncSrcID write fSyncSrcID;
    property Timestamp:                       TIdNTPTimestamp   read fTimestamp write fTimestamp;
    property Version:                         TIdRTPVersion     read fVersion write fVersion;
  end;

implementation

//******************************************************************************
//* TIdRTPProfile                                                              *
//******************************************************************************
//* TIdRTPProfile Public methods ***********************************************

constructor TIdRTPProfile.Create;
begin
  inherited Create;

  Self.NullEncoding := TIdRTPNullEncoding.Create('', 0, '');
  Self.Reserved     := TIdRTPReservedEncoding.Create('', 0, '');
  Self.Initialize;
end;

destructor TIdRTPProfile.Destroy;
begin
  Self.Clear;

  Self.Reserved.Free;
  Self.NullEncoding.Free;

  inherited Destroy;
end;

procedure TIdRTPProfile.AddEncoding(const Encoding: TIdRTPEncoding;
                                    const PayloadType: TIdRTPPayloadType);
begin
  Self.AddEncodingAsReference(Encoding.Clone, PayloadType);
end;

procedure TIdRTPProfile.Clear;
var
  I: TIdRTPPayloadType;
begin
  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do
    if not Self.EncodingAt(I).IsNull and not Self.EncodingAt(I).IsReserved then
      Self.RemoveEncoding(I);
end;

function TIdRTPProfile.Count: Integer;
var
  I: TIdRTPPayloadType;
begin
  Result := 0;
  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do
    if not Self.EncodingAt(I).IsNull then Inc(Result);
end;

function TIdRTPProfile.FirstFreePayloadType: TIdRTPPayloadType;
var
  I: Cardinal;
begin
 I := 0;
  while (I <= High(TIdRTPPayloadType)) and not Self.EncodingAt(I).IsNull do
    Inc(I);

  Result := TIdRTPPayloadType(I);
end;

procedure TIdRTPProfile.InitializeOn(const SDP: TIdSdpPayload);
var
  I:       Integer;
  RTPMaps: TIdSdpAttributes;
begin
  RTPMaps := TIdSdpAttributes.Create;
  try
    SDP.GetRtpMapAttributes(RTPMaps);

    for I := 0 to RTPMaps.Count - 1 do begin
    end;
  finally
    RTPMaps.Free;
  end;
end;

function TIdRTPProfile.IsFull: Boolean;
var
  I: TIdRTPPayloadType;
begin
  Result := true;

  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
    Result := Result and not Self.EncodingAt(I).IsNull;
    if not Result then Break;
  end;
end;

function TIdRTPProfile.HasEncoding(const Encoding: TIdRTPEncoding): Boolean;
begin
  Result := not Encoding.IsNull and (Self.IndexOfEncoding(Encoding) <> -1);
end;

function TIdRTPProfile.HasPayloadType(const PayloadType: TIdRTPPayloadType): Boolean;
begin
  Result := not Self.EncodingAt(PayloadType).IsNull;
end;

function TIdRTPProfile.EncodingFor(const PayloadType: TIdRTPPayloadType): TIdRTPEncoding;
begin
  Result := Self.EncodingAt(PayloadType)
end;

function TIdRTPProfile.PayloadTypeFor(const Encoding: TIdRTPEncoding): TIdRTPPayloadType;
var
  Index: Integer;
begin
  Index := Self.IndexOfEncoding(Encoding);

  if (Index = -1) then
    raise ENoPayloadTypeFound.Create(Encoding.AsString)
  else
    Result := TIdRTPPayloadType(Index);
end;

function TIdRTPProfile.TransportDesc: String;
begin
  Result := '';
end;

//* TIdRTPProfile Protected methods ********************************************

procedure TIdRTPProfile.AddEncodingAsReference(Encoding: TIdRTPEncoding;
                                               const PayloadType: TIdRTPPayloadType);
begin
  if not Self.HasPayloadType(PayloadType) and not Self.HasEncoding(Encoding) then
    Self.Encodings[PayloadType] := Encoding;
end;

procedure TIdRTPProfile.ReservePayloadType(const PayloadType: TIdRTPPayloadType);
begin
  Self.Encodings[PayloadType] := Self.Reserved;
end;

//* TIdRTPProfile Private methods **********************************************

function TIdRTPProfile.EncodingAt(const PayloadType: TIdRTPPayloadType): TIdRTPEncoding;
begin
  Result := TIdRTPEncoding(Self.Encodings[PayloadType]);
end;

function TIdRTPProfile.IndexOfEncoding(const Encoding: TIdRTPEncoding): Integer;
begin
  Result := 0;

  while (Result <= High(TIdRTPPayloadType))
    and not Self.EncodingAt(Result).IsEqualTo(Encoding) do
      Inc(Result);

  if (Result > High(TIdRTPPayloadType)) then
    Result := -1;
end;

procedure TIdRTPProfile.Initialize;
var
  I: TIdRTPPayloadType;
begin
  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do
    Self.Encodings[I] := Self.NullEncoding;
end;

procedure TIdRTPProfile.RemoveEncoding(const PayloadType: TIdRTPPayloadType);
begin
  Self.Encodings[PayloadType].Free;
  Self.Encodings[PayloadType] := Self.NullEncoding;
end;

//******************************************************************************
//* TIdAudioVisualProfile                                                      *
//******************************************************************************
//* TIdAudioVisualProfile Public methods ***************************************

constructor TIdAudioVisualProfile.Create;
begin
  inherited Create;

  Self.AddEncodingAsReference(TIdRTPEncoding.Create('PCMU',  8000),        0);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('GSM',   8000),        3);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('G723',  8000),        4);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('DVI4',  8000),        5);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('DVI4',  16000),       6);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('LPC',   8000),        7);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('PCMA',  8000),        8);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('G722',  8000),        9);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('L16',   44100, '2'), 10);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('L16',   44100, '1'), 11);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('QCELP', 8000),       12);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('CN',    8000),       13);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('MPA',   90000),      14);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('G728',  8000),       15);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('DVI4',  11025),      16);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('DVI4',  22050),      17);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('G729',  8000),       18);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('CelB',  90000),      25);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('JPEG',  90000),      26);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('nv',    90000),      28);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('H261',  90000),      31);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('MPV',   90000),      32);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('MP2T',  90000),      33);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('H263',  90000),      34);

  Self.ReserveRange(1,  2);
  Self.ReserveRange(19, 24);

  Self.ReservePayloadType(27);

  Self.ReserveRange(29, 30);
  Self.ReserveRange(35, 95);
end;

function TIdAudioVisualProfile.TransportDesc: String;
begin
  Result := AudioVisualProfile;
end;

//* TIdAudioVisualProfile Private methods **************************************

procedure TIdAudioVisualProfile.ReserveRange(const LowPT, HighPT: TIdRTPPayloadType);
var
  I: TIdRTPPayloadType;
begin
  for I := LowPT to HighPT do
    Self.ReservePayloadType(I);
end;

//******************************************************************************
//* TIdRTPPacket                                                               *
//******************************************************************************
//* TIdRTPPacket Public methods ************************************************

constructor TIdRTPPacket.Create(const Profile: TIdRTPProfile);
begin
  inherited Create;

  fPayload := TIdRTPPayload.NullPayload;

  Self.Profile := Profile;
  Self.Version := Self.DefaultVersion;
end;

destructor TIdRTPPacket.Destroy;
begin
  if not Self.Payload.IsNull then
    Self.Payload.Free;

  inherited Destroy;
end;

function TIdRTPPacket.DefaultVersion: TIdRTPVersion;
begin
  Result := 2;
end;

procedure TIdRTPPacket.ReadFrom(Src: TStream);
var
  B: Byte;
  I: TIdRTPCsrcCount;
begin
  Src.Read(B, SizeOf(B));
  Self.Version      := (B and $C0) shr 6;
  Self.HasPadding   := (B and $20) <> 0;
  Self.HasExtension := (B and $10) <> 0;
  Self.CsrcCount    :=  B and $0F;

  Src.Read(B, SizeOf(B));
  Self.IsMarker    := (B and $80) <> 0;
  Self.PayloadType :=  B and $7F;

  Self.SequenceNo := ReadWord(Src);
  Self.Timestamp  := ReadCardinal(Src);
  Self.SyncSrcID  := ReadCardinal(Src);

  for I := 1 to Self.CsrcCount do
    Self.CsrcIDs[I - 1] := ReadCardinal(Src);
end;

procedure TIdRTPPacket.PrintOn(Dest: TStream);
var
  B: Byte;
  I: Integer;
begin
  B := Self.Version shl 6;
  if Self.HasPadding then B := B or $20;
  if Self.HasExtension then B := B or $10;
  B := B or Self.CsrcCount;
  Dest.Write(B, SizeOf(B));

  B := Self.PayloadType;
  if Self.IsMarker then B := B or $80;
  Dest.Write(B, SizeOf(B));

  WriteWord(Dest, Self.SequenceNo);

  WriteCardinal(Dest, Self.Timestamp);
  WriteCardinal(Dest, Self.SyncSrcID);

  for I := 0 to Self.CsrcCount - 1 do
    WriteCardinal(Dest, Self.CsrcIDs[I]);

  Self.Payload.PrintOn(Dest);
end;

procedure TIdRTPPacket.ReadPayload(Src: TStream);
begin
  Self.CreatePayload(Self.Profile.EncodingFor(Self.PayloadType));
  Self.Payload.ReadFrom(Src);
end;

procedure TIdRTPPacket.ReadPayload(Src: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(Src);
  try
    Self.ReadPayload(S);
  finally
    S.Free;
  end;
end;

//* TIdRTPPacket Private methods ***********************************************

procedure TIdRTPPacket.CreatePayload(const Encoding: TIdRTPEncoding);
begin
  fPayload := TIdRTPPayload.CreatePayload(Encoding);
end;

function TIdRTPPacket.GetCsrcCount: TIdRTPCsrcCount;
begin
  Result := fCsrcCount;
end;

function TIdRTPPacket.GetCsrcID(Index: TIdRTPCsrcCount): Cardinal;
begin
  Result := fCsrcIDs[Index];
end;

procedure TIdRTPPacket.SetCsrcCount(const Value: TIdRTPCsrcCount);
begin
  fCsrcCount := Value;

  SetLength(fCsrcIDs, Value);
end;

procedure TIdRTPPacket.SetCsrcID(Index: TIdRTPCsrcCount; const Value: Cardinal);
begin
  fCsrcIDs[Index] := Value;
end;

end.
