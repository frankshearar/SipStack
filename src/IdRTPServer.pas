unit IdRTPServer;

interface

uses
  Classes, Contnrs, IdSocketHandle, IdUDPServer;

type
  TIdCardinalArray  = array of Cardinal;
  TIdNTPTimestamp   = Cardinal;
  TIdRTPCsrcCount   = 0..15;
  TIdRTPPayloadType = 0..127;
  TIdRTPSequenceNo  = Word;
  TIdRTPVersion     = 0..3;
  TIdT140BlockCount = Word;

  // I am an Encoding. I am described in an SDP payload (RFC 2327),
  // and instantiated by things that need to describe these sorts
  // of encodings. I am a Value Object.
  TIdRTPEncoding = class(TObject)
  private
    fClockRate:  Cardinal;
    fName:       String;
    fParameters: String;
  public
    constructor Create(const Name: String;
                       const ClockRate: Cardinal;
                       const Parameters: String = ''); overload; virtual;
    constructor Create(const Src: TIdRTPEncoding); overload; virtual;

    function AsString: String; virtual;
    function IsEqualTo(const OtherEncoding: TIdRTPEncoding): Boolean;
    function IsNull: Boolean; virtual;

    property ClockRate:  Cardinal read fClockRate;
    property Name:       String   read fName;
    property Parameters: String   read fParameters;
  end;

  // I am the Null Encoding. I represent the fact that there is no defined
  // encoding.
  TIdRTPNullEncoding = class(TIdRTPEncoding)
  public
    constructor Create(const Name: String;
                       const ClockRate: Cardinal;
                       const Parameters: String = ''); overload; override;
    constructor Create(const Src: TIdRTPEncoding); overload; override;

    function AsString: String; override;
    function IsNull: Boolean; override;
  end;

  // I am a 1-1 association map between encodings and RTP Payload Types.
  TIdRTPProfile = class(TObject)
  private
    Encodings:    TObjectList;
    PayloadTypes: TList;
    NullEncoding: TIdRTPEncoding;

    function IndexOfEncoding(const Encoding: TIdRTPEncoding): Integer;
    function IndexOfPayloadType(const PayloadType: TIdRTPPayloadType): Integer;
  protected
    procedure AddEncodingAsReference(Encoding: TIdRTPEncoding;
                                     const PayloadType: TIdRTPPayloadType);
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddEncoding(const Encoding: TIdRTPEncoding;
                          const PayloadType: TIdRTPPayloadType);
    function  Count: Integer;
    function  HasEncoding(const Encoding: TIdRTPEncoding): Boolean;
    function  HasPayloadType(const PayloadType: TIdRTPPayloadType): Boolean;
    function  EncodingFor(const PayloadType: TIdRTPPayloadType): TIdRTPEncoding;
    function  PayloadTypeFor(const Encoding: TIdRTPEncoding): TIdRTPPayloadType;
  end;

  TIdAudioVisualProfile = class(TIdRTPProfile)
  public
    constructor Create; override;
  end;

  TIdRTPPayload = class(TObject)
  public
    class function CreatePayload(const Encoding: TIdRTPEncoding): TIdRTPPayload;
    class function CreateFrom(const Encoding: TIdRTPEncoding;
                              Src: TStream): TIdRTPPayload;

    procedure ReadFrom(Src: TStream); virtual;
    procedure PrintOn(Dest: TStream); virtual;
  end;

  TIdT140Payload = class(TIdRTPPayload)
  private
    fBlock:      String;
    fBlockCount: TIdT140BlockCount;
  public
    procedure ReadFrom(Src: TStream); override;
    procedure PrintOn(Dest: TStream); override;

    property Block:      String            read fBlock write fBlock; //TODO: this must be UTF-8
    property BlockCount: TIdT140BlockCount read fBlockCount write fBlockCount;
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
    fPayloadType:  TIdRTPPayloadType;
    fSequenceNo:   TIdRTPSequenceNo;
    fSyncSrcID:    Cardinal;
    fTimestamp:    TIdNTPTimestamp;
    fVersion:      TIdRTPVersion;
    Profile:       TIdRTPProfile;

    function  GetCsrcCount: TIdRTPCsrcCount;
    function  GetCsrcID(Index: TIdRTPCsrcCount): Cardinal;
    procedure SetCsrcCount(const Value: TIdRTPCsrcCount);
    procedure SetCsrcID(Index: TIdRTPCsrcCount; const Value: Cardinal);
  public
    constructor Create(const Profile: TIdRTPProfile);

    function  DefaultVersion: TIdRTPVersion;
    procedure ReadFrom(Src: TStream);
    procedure PrintOn(Dest: TStream);
    procedure ReadPayload(Src: TStream);

    property CsrcCount:                       TIdRTPCsrcCount   read GetCsrcCount write SetCsrcCount;
    property CsrcIDs[Index: TIdRTPCsrcCount]: Cardinal          read GetCsrcID write SetCsrcID;
    property HasExtension:                    Boolean           read fHasExtension write fHasExtension;
    property HasPadding:                      Boolean           read fHasPadding write fHasPadding;
    property IsMarker:                        Boolean           read fIsMarker write fIsMarker;
    property PayloadType:                     TIdRTPPayloadType read fPayloadType write fPayloadType;
    property SequenceNo:                      TIdRTPSequenceNo  read fSequenceNo write fSequenceNo;
    property SyncSrcID:                       Cardinal          read fSyncSrcID write fSyncSrcID;
    property Timestamp:                       TIdNTPTimestamp   read fTimestamp write fTimestamp;
    property Version:                         TIdRTPVersion     read fVersion write fVersion;
  end;

  TIdRTPReadEvent = procedure(Sender: TObject;
                              APacket: TIdRTPPacket;
                              ABinding: TIdSocketHandle) of object;

  TIdRTPServer = class(TIdUDPServer)
  private
    FProfile:   TIdRTPProfile;
    FOnRTPRead: TIdRTPReadEvent;

    procedure DoOnRTPRead(APacket: TIdRTPPacket; ABinding: TIdSocketHandle);
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    property Profile: TIdRTPProfile read FProfile;
  published
    property OnRTPRead: TIdRTPReadEvent read FOnRTPRead write FOnRTPRead;
  end;

function EncodeAsString(Value: Cardinal): String; overload;
function EncodeAsString(Value: Word): String; overload;
function HtoNL(Value: Cardinal): Cardinal;
function HtoNS(Value: Word): Word;
function NtoHL(Value: Cardinal): Cardinal;
function NtoHS(Value: Word): Cardinal;

function  ReadCardinal(Src: TStream): Cardinal;
function  ReadWord(Src: TStream): Word;
procedure WriteCardinal(Dest: TStream; Value: Cardinal);
procedure WriteWord(Dest: TStream; Value: Word);

implementation

uses
  IdSipConsts, SysUtils;

//******************************************************************************
//* Unit public functions & procedures                                         *
//******************************************************************************

function EncodeAsString(Value: Cardinal): String;
begin
  Result := Chr((Value and $FF000000) shr 24)
          + Chr((Value and $00FF0000) shr 16)
          + Chr((Value and $0000FF00) shr 8)
          + Chr( Value and $000000FF);
end;

function EncodeAsString(Value: Word): String;
begin
  Result := Chr((Value and $FF00) shr 8)
          + Chr( Value and $00FF);
end;

function HtoNL(Value: Cardinal): Cardinal;
begin
  Result := ((Value and $FF000000) shr 24)
         or ((Value and $00FF0000) shr 8)
         or ((Value and $0000FF00) shl 8)
         or ((Value and $000000FF) shl 24);
end;

function HtoNS(Value: Word): Word;
begin
  Result := ((Value and $00FF) shl 8) or ((Value  and $FF00) shr 8);
end;

function NtoHL(Value: Cardinal): Cardinal;
begin
  Result := HtoNL(Value);
end;

function NtoHS(Value: Word): Cardinal;
begin
  Result := HtoNS(Value);
end;

function ReadCardinal(Src: TStream): Cardinal;
begin
  Src.Read(Result, SizeOf(Result));
  Result := NtoHL(Result);
end;

function ReadWord(Src: TStream): Word;
begin
  Src.Read(Result, SizeOf(Result));
  Result := NtoHS(Result);
end;

procedure WriteCardinal(Dest: TStream; Value: Cardinal);
begin
  Value := HtoNL(Value);
  Dest.Write(Value, SizeOf(Value));
end;

procedure WriteWord(Dest: TStream; Value: Word);
begin
  Value := HtoNS(Value);
  Dest.Write(Value, SizeOf(Value));
end;

//******************************************************************************
//* TIdRTPEncoding                                                             *
//******************************************************************************
//* TIdRTPEncoding Public methods **********************************************

constructor TIdRTPEncoding.Create(const Name: String;
                                  const ClockRate: Cardinal;
                                  const Parameters: String = '');
begin
  inherited Create;

  fClockRate  := ClockRate;
  fName       := Name;
  fParameters := Parameters;
end;

constructor TIdRTPEncoding.Create(const Src: TIdRTPEncoding);
begin
  inherited Create;

  fClockRate  := Src.ClockRate;
  fName       := Src.Name;
  fParameters := Src.Parameters;
end;

function TIdRTPEncoding.AsString: String;
begin
  Result := Self.Name + '/' + IntToStr(Self.ClockRate);

  if (Self.Parameters <> '') then
    Result := Result + '/' + Self.Parameters;
end;

function TIdRTPEncoding.IsEqualTo(const OtherEncoding: TIdRTPEncoding): Boolean;
begin
  Result := (Self.Name = OtherEncoding.Name)
        and (Self.ClockRate = OtherEncoding.ClockRate)
        and (Self.Parameters = OtherEncoding.Parameters);
end;

function TIdRTPEncoding.IsNull: Boolean;
begin
  Result := false;
end;

//******************************************************************************
//* TIdRTPNullEncoding                                                         *
//******************************************************************************
//* TIdRTPNullEncoding Public methods ******************************************

constructor TIdRTPNullEncoding.Create(const Name: String;
                                      const ClockRate: Cardinal;
                                      const Parameters: String = '');
begin
  inherited Create('', 0, '');
end;

constructor TIdRTPNullEncoding.Create(const Src: TIdRTPEncoding);
begin
  inherited Create('', 0, '');
end;

function TIdRTPNullEncoding.AsString: String;
begin
  Result := '';
end;

function TIdRTPNullEncoding.IsNull: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdRTPProfile                                                              *
//******************************************************************************
//* TIdRTPProfile Public methods ***********************************************

constructor TIdRTPProfile.Create;
begin
  inherited Create;

  Self.Encodings    := TObjectList.Create(true);
  Self.PayloadTypes := TList.Create;
  Self.NullEncoding := TIdRTPNullEncoding.Create('', 0, '');
end;

destructor TIdRTPProfile.Destroy;
begin
  Self.NullEncoding.Free;
  Self.PayloadTypes.Free;
  Self.Encodings.Free;

  inherited Destroy;
end;

procedure TIdRTPProfile.AddEncoding(const Encoding: TIdRTPEncoding;
                                    const PayloadType: TIdRTPPayloadType);
begin
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(Encoding), PayloadType);
end;

function TIdRTPProfile.Count: Integer;
begin
  Result := Self.Encodings.Count;
end;

function TIdRTPProfile.HasEncoding(const Encoding: TIdRTPEncoding): Boolean;
begin
  Result := not Encoding.IsNull and (Self.IndexOfEncoding(Encoding) <> -1);
end;

function TIdRTPProfile.HasPayloadType(const PayloadType: TIdRTPPayloadType): Boolean;
begin
  Result := Self.IndexOfPayloadType(PayloadType) <> -1;
end;

function TIdRTPProfile.EncodingFor(const PayloadType: TIdRTPPayloadType): TIdRTPEncoding;
var
  Index: Integer;
begin
  Index := Self.IndexOfPayloadType(PayloadType);

  if (Index = -1) then
    Result := Self.NullEncoding
  else
    Result := TIdRTPEncoding(Self.Encodings[Index]);
end;

function TIdRTPProfile.PayloadTypeFor(const Encoding: TIdRTPEncoding): TIdRTPPayloadType;
var
  Index: Integer;
begin
  Index := Self.IndexOfEncoding(Encoding);

  if (Index = -1) then
    Result := High(TIdRTPPayloadType)
  else
    Result := TIdRTPPayloadType(Self.PayloadTypes[Index]);
end;

//* TIdRTPProfile Protected methods ********************************************

procedure TIdRTPProfile.AddEncodingAsReference(Encoding: TIdRTPEncoding;
                                               const PayloadType: TIdRTPPayloadType);
begin
  if not Self.HasPayloadType(PayloadType) and not Self.HasEncoding(Encoding) then begin
    Self.Encodings.Add(Encoding);
    Self.PayloadTypes.Add(Pointer(PayloadType));
  end;
end;

//* TIdRTPProfile Private methods **********************************************

function TIdRTPProfile.IndexOfEncoding(const Encoding: TIdRTPEncoding): Integer;
begin
  Result := 0;

  while (Result < Self.Encodings.Count)
    and not TIdRTPEncoding(Self.Encodings[Result]).IsEqualTo(Encoding) do
      Inc(Result);

  if (Result = Self.Encodings.Count) then
    Result := -1;
end;

function TIdRTPProfile.IndexOfPayloadType(const PayloadType: TIdRTPPayloadType): Integer;
begin
  Result := Self.PayloadTypes.IndexOf(Pointer(PayloadType));
end;

//******************************************************************************
//* TIdAudioVisualProfile                                                      *
//******************************************************************************
//* TIdAudioVisualProfile Public methods ***************************************

constructor TIdAudioVisualProfile.Create;
var
  I: Integer;
begin
  inherited Create;

  Self.AddEncodingAsReference(TIdRTPEncoding.Create('PCMU',  8000),        0);
  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),           1);
  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),           2);
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
  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),          19);

  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),          20);
  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),          21);
  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),          22);
  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),          23);
  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),          24);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('CelB',  90000),      25);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('JPEG',  90000),      26);
  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),          27);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('nv',    90000),      28);
  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),          29);
  Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0),          30);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('H261',  90000),      31);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('MPV',   90000),      32);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('MP2T',  90000),      33);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create('H263',  90000),      34);

  for I := 35 to 95 do
    Self.AddEncodingAsReference(TIdRTPNullEncoding.Create('',  0), I);
end;

//******************************************************************************
//* TIdRTPPayload                                                              *
//******************************************************************************
//* TIdRTPPayload Public methods ***********************************************

class function TIdRTPPayload.CreatePayload(const Encoding: TIdRTPEncoding): TIdRTPPayload;
begin
  if (Encoding.Name = T140EncodingName) then
    Result := TIdT140Payload.Create
  else
    Result := nil;
end;

class function TIdRTPPayload.CreateFrom(const Encoding: TIdRTPEncoding;
                                        Src: TStream): TIdRTPPayload;
begin
  Result := Self.CreatePayload(Encoding);
  try
    Result.ReadFrom(Src);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

procedure TIdRTPPayload.ReadFrom(Src: TStream);
begin
end;

procedure TIdRTPPayload.PrintOn(Dest: TStream);
begin
end;

//******************************************************************************
//* TIdT140Payload                                                             *
//******************************************************************************
//* TIdT140Payload Public methods **********************************************

procedure TIdT140Payload.ReadFrom(Src: TStream);
var
  S: TStringStream;
begin
  Self.BlockCount := ReadWord(Src);

  S := TStringStream.Create('');
  try
    S.CopyFrom(Src, Src.Size - SizeOf(Self.BlockCount));

    Self.Block := S.DataString;
  finally
    S.Free;
  end;
end;

procedure TIdT140Payload.PrintOn(Dest: TStream);
begin
  WriteWord(Dest, Self.BlockCount);
  Dest.Write(Self.Block[1], Length(Self.Block));
end;

//******************************************************************************
//* TIdRTPPacket                                                               *
//******************************************************************************
//* TIdRTPPacket Public methods ************************************************

constructor TIdRTPPacket.Create(const Profile: TIdRTPProfile);
begin
  inherited Create;

  Self.Profile := Profile;
  Self.Version := Self.DefaultVersion;
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
end;

procedure TIdRTPPacket.ReadPayload(Src: TStream);
var
  Payload: TIdRTPPayload;
begin
  Payload := TIdRTPPayload.CreatePayload(Self.Profile.EncodingFor(Self.PayloadType));
  try
    Payload.ReadFrom(Src);
  finally
    Payload.Free;
  end;
end;

//* TIdRTPPacket Private methods ***********************************************

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

//******************************************************************************
//* TIdRTPServer                                                               *
//******************************************************************************
//* TIdRTPServer Public methods ************************************************

constructor TIdRTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.FProfile := TIdAudioVisualProfile.Create;
  
  Self.ThreadedEvent := true;
end;

destructor TIdRTPServer.Destroy;
begin
  Self.Profile.Free;

  inherited Destroy;
end;

//* TIdRTPServer Protected methods *********************************************

procedure TIdRTPServer.DoUDPRead(AData: TStream; ABinding: TIdSocketHandle);
var
  Pkt: TIdRTPPacket;
begin
  inherited DoUDPRead(AData, ABinding);

  Pkt := TIdRTPPacket.Create(Self.Profile);
  try
    Pkt.ReadFrom(AData);
//    Pkt.ReadPayload(AData);
    Self.DoOnRTPRead(Pkt, ABinding);
  finally
    Pkt.Free;
  end;
end;

//* TIdRTPServer Private methods ***********************************************

procedure TIdRTPServer.DoOnRTPRead(APacket: TIdRTPPacket;
                                   ABinding: TIdSocketHandle);
begin
  if Assigned(Self.OnRTPRead) then
    Self.OnRTPRead(Self, APacket, ABinding);
end;

end.
