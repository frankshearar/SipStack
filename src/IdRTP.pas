unit IdRTP;

interface

uses
  Classes, SysUtils;

type
  TIdCardinalArray        = array of Cardinal;
  TIdTelephoneEventVolume = 0..63;
  TIdNTPTimestamp         = Cardinal;
  TIdRTCPSourceCount      = 0..31;
  TIdRTPCsrcCount         = 0..15;
  TIdRTPPayloadType       = 0..127;
  TIdRTPSequenceNo        = Word;
  TIdRTPVersion           = 0..3;
  TIdT140BlockCount       = Word;

  TIdRTPPayload = class;
  TIdRTPPayloadClass = class of TIdRTPPayload;

  // I am an Encoding. I am described in an SDP payload (RFC 2327),
  // and instantiated by things that need to describe these sorts
  // of encodings. I am a Value Object.
  TIdRTPEncoding = class(TObject)
  private
    fClockRate:  Cardinal;
    fName:       String;
    fParameters: String;
  public
    class function CreateEncoding(Value: String): TIdRTPEncoding;

    constructor Create(const Name: String;
                       const ClockRate: Cardinal;
                       const Parameters: String = ''); overload; virtual;
    constructor Create(const Src: TIdRTPEncoding); overload; virtual;

    function AsString: String; virtual;
    function Clone: TIdRTPEncoding; virtual;
    function CreatePayload: TIdRTPPayload; virtual;
    function IsEqualTo(const OtherEncoding: TIdRTPEncoding): Boolean;
    function IsNull: Boolean; virtual;
    function IsReserved: Boolean; virtual;
    function PayloadType: TIdRTPPayloadClass; virtual;

    property ClockRate:  Cardinal read fClockRate;
    property Name:       String   read fName;
    property Parameters: String   read fParameters;
  end;

  TIdRTPEncodingClass = class of TIdRTPEncoding;

  TIdRTPT140Encoding = class(TIdRTPEncoding)
  public
    function PayloadType: TIdRTPPayloadClass; override;
  end;

  TIdRTPTelephoneEventEncoding = class(TIdRTPEncoding)
  public
    function PayloadType: TIdRTPPayloadClass; override;
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
    function Clone: TIdRTPEncoding; override;
    function IsNull: Boolean; override;
  end;

  // I am a Reserved or Unassigned encoding in an RTP profile. In other words, I
  // do nothing other than say to you "you may not use this payload type".
  TIdRTPReservedEncoding = class(TIdRTPEncoding)
  public
    constructor Create(const Name: String;
                       const ClockRate: Cardinal;
                       const Parameters: String = ''); overload; override;
    constructor Create(const Src: TIdRTPEncoding); overload; override;

    function AsString: String; override;
    function Clone: TIdRTPEncoding; override;
    function IsReserved: Boolean; override;
  end;

  // I am the payload in an RTP packet. My subclasses implement appropriate
  // parsing and outputting.
  //
  // I offer a Flyweight Null Payload.
  TIdRTPPayload = class(TObject)
  public
    class function CreatePayload(const Encoding: TIdRTPEncoding): TIdRTPPayload;
    class function CreateFrom(const Encoding: TIdRTPEncoding;
                              Src: TStream): TIdRTPPayload;
    class function NullPayload: TIdRTPPayload;

    function  IsNull: Boolean; virtual;
    procedure ReadFrom(Src: TStream); virtual;
    procedure PrintOn(Dest: TStream); virtual;
  end;

  // I am the Null payload - a Null Object representing the absence of a
  // payload.
  TIdNullPayload = class(TIdRTPPayload)
  public
    function IsNull: Boolean; override;
  end;

  // I am the Raw payload. That means that my content is completely unparsed.
  // I'm mostly a fallback case for a malconfigured RTP server.
  TIdRawPayload = class(TIdRTPPayload)
  private
    fData: String;
  public
    procedure ReadFrom(Src: TStream); override;
    procedure PrintOn(Dest: TStream); override;

    property Data: String read fData write fData;
  end;

  // I am a T.140 payload, as defined in RFC 2793 (and the bis draft)
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

  // I represent DTMF signals and such, as defined in RFC 2833
  TIdTelephoneEventPayload = class(TIdRTPPayload)
  private
    fDuration:    Word;
    fEvent:       Byte;
    fIsEnd:       Boolean;
    fReservedBit: Boolean;
    fVolume:      TIdTelephoneEventVolume;
  public
    procedure ReadFrom(Src: TStream); override;
    procedure PrintOn(Dest: TStream); override;

    property Duration:    Word                    read fDuration write fDuration;
    property Event:       Byte                    read fEvent write fEvent;
    property IsEnd:       Boolean                 read fIsEnd write fIsEnd;
    property ReservedBit: Boolean                 read fReservedBit write fReservedBit;
    property Volume:      TIdTelephoneEventVolume read fVolume write fVolume;
  end;

  TIdPayloadArray = array[Low(TIdRTPPayloadType)..High(TIdRTPPayloadType)] of TIdRTPEncoding;

  // I am a 1-1 association map between encodings and RTP Payload Types. RTP
  // packets use me to determine how their payload should be interpreted.
  // Because I am 1-1, you cannot add the same encoding to me twice. If you try,
  // the payload type you try will remain unchanged.
  TIdRTPProfile = class(TPersistent)
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
    procedure Assign(Src: TPersistent); override;
    procedure Clear;
    function  Count: Integer;
    function  FirstFreePayloadType: TIdRTPPayloadType;
    function  IsFull: Boolean;
    function  HasEncoding(const Encoding: TIdRTPEncoding): Boolean;
    function  HasPayloadType(const PayloadType: TIdRTPPayloadType): Boolean;
    function  EncodingFor(const PayloadType: TIdRTPPayloadType): TIdRTPEncoding;
    function  PayloadTypeFor(const Encoding: TIdRTPEncoding): TIdRTPPayloadType;
    function  TransportDesc: String; virtual;
  end;

  // I am the profile defined in RFC 3551. As such, don't bother trying to
  // change the encodings of any reserved or assigned payload types. The
  // only payload types I allow to be altered are the dynamic payload types -
  // 96-127.
  TIdAudioVisualProfile = class(TIdRTPProfile)
  private
    procedure ReserveRange(const LowPT, HighPT: TIdRTPPayloadType);
  public
    constructor Create; override;

    procedure Assign(Src: TPersistent); override;
    function  TransportDesc: String; override;
  end;

  // I'm a Header Extension. RFC 3550 doesn't define an interpretation for my
  // data beyond the length field, so it's up to my subclasses to provide more
  // meaningful properties.
  TIdRTPHeaderExtension = class(TObject)
  private
    fData:                array of Cardinal;
    fProfileDefinedValue: Word;

    function  GetData(Index: Word): Cardinal;
    function  GetLength: Word;
    procedure SetData(Index: Word; const Value: Cardinal);
    procedure SetLength(const Value: Word);
  public
    procedure ReadFrom(Src: TStream);
    procedure PrintOn(Dest: TStream);

    property Length:              Word     read GetLength write SetLength;
    property ProfileDefinedValue: Word     read fProfileDefinedValue write fProfileDefinedValue;
    property Data[Index: Word]:   Cardinal read GetData write SetData;
  end;

  // I am a packet of the Real-time Transport Protocol.
  //
  // I currently do not support the Header Extension as defined in RFC 3550,
  // section 5.3.1, nor do I have timestamp (either RTP or NTP) information.
  TIdRTPPacket = class(TObject)
  private
    fCsrcCount:       TIdRTPCsrcCount;
    fCsrcIDs:         TIdCardinalArray;
    fHasExtension:    Boolean;
    fHasPadding:      Boolean;
    fHeaderExtension: TIdRTPHeaderExtension;
    fIsMarker:        Boolean;
    fPayload:         TIdRTPPayload;
    fPayloadType:     TIdRTPPayloadType;
    fSequenceNo:      TIdRTPSequenceNo;
    fSyncSrcID:       Cardinal;
    fTimestamp:       TIdNTPTimestamp;
    fVersion:         TIdRTPVersion;
    Profile:          TIdRTPProfile;

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

    property CsrcCount:                       TIdRTPCsrcCount       read GetCsrcCount write SetCsrcCount;
    property CsrcIDs[Index: TIdRTPCsrcCount]: Cardinal              read GetCsrcID write SetCsrcID;
    property HasExtension:                    Boolean               read fHasExtension write fHasExtension;
    property HasPadding:                      Boolean               read fHasPadding write fHasPadding;
    property HeaderExtension:                 TIdRTPHeaderExtension read fHeaderExtension;
    property IsMarker:                        Boolean               read fIsMarker write fIsMarker;
    property Payload:                         TIdRTPPayload         read fPayload;
    property PayloadType:                     TIdRTPPayloadType     read fPayloadType write fPayloadType;
    property SequenceNo:                      TIdRTPSequenceNo      read fSequenceNo write fSequenceNo;
    property SyncSrcID:                       Cardinal              read fSyncSrcID write fSyncSrcID;
    property Timestamp:                       TIdNTPTimestamp       read fTimestamp write fTimestamp;
    property Version:                         TIdRTPVersion         read fVersion write fVersion;
  end;

  // I am a packet in the Real-time Transport Control Protocol, as defined in
  // RFC 3550 section 6.
  TIdRTCPPacket = class(TObject)
  protected
    function GetPacketType: Cardinal; virtual; abstract;
  public
    procedure PrintOn(Dest: TStream); virtual; abstract;
    procedure ReadFrom(Src: TStream); virtual; abstract;
  end;

  TIdRTCPByePacket = class(TIdRTCPPacket)
  private
    fSources:      TIdCardinalArray;
    fHasPadding:   Boolean;
    fLength:       Word;
    fReason:       String;
    fReasonLength: Word;
    fVersion:      TIdRTPVersion;

    function  GetSourceCount: TIdRTCPSourceCount;
    function  GetSource(Index: TIdRTCPSourceCount): Cardinal;
    procedure SetSourceCount(const Value: TIdRTCPSourceCount);
    procedure SetSource(Index: TIdRTCPSourceCount;
                        const Value: Cardinal);
  protected
    function GetPacketType: Cardinal; override;
  public
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;

    property HasPadding:                         Boolean            read fHasPadding write fHasPadding;
    property Length:                             Word               read fLength write fLength;
    property PacketType:                         Cardinal           read GetPacketType;
    property Reason:                             String             read fReason write fReason;
    property ReasonLength:                       Word               read fReasonLength write fReasonLength;
    property SourceCount:                        TIdRTCPSourceCount read GetSourceCount write SetSourceCount;
    property Sources[Index: TIdRTCPSourceCount]: Cardinal           read GetSource write SetSource;
    property Version:                            TIdRTPVersion      read fVersion write fVersion;
  end;

  ENoPayloadTypeFound = class(Exception);

function EncodeAsString(Value: Cardinal): String; overload;
function EncodeAsString(Value: Word): String; overload;
function HtoNL(Value: Cardinal): Cardinal;
function HtoNS(Value: Word): Word;
function NtoHL(Value: Cardinal): Cardinal;
function NtoHS(Value: Word): Cardinal;

function  ReadCardinal(Src: TStream): Cardinal;
function  ReadRemainderOfStream(Src: TStream): String;
function  ReadString(Src: TStream; const Length: Cardinal): String;
function  ReadWord(Src: TStream): Word;
procedure WriteCardinal(Dest: TStream; Value: Cardinal);
procedure WriteWord(Dest: TStream; Value: Word);

const
  AudioVisualProfile = 'RTP/AVP'; // defined in RFC 3551

  CelBEncoding                = 'CelB';
  CNEncoding                  = 'CN';
  DVI4Encoding                = 'DVI4';
  G722Encoding                = 'G722';
  G723Encoding                = 'G723';
  G728Encoding                = 'G728';
  G729Encoding                = 'G729';
  GSMEncoding                 = 'GSM';
  H261Encoding                = 'H261';
  H263Encoding                = 'H263';
  InterleavedT140ClockRate    = 8000;
  JPEGEncoding                = 'JPEG';
  L16Encoding                 = 'L16';
  LPCEncoding                 = 'LPC';
  MP2TEncoding                = 'MP2T';
  MPAEncoding                 = 'MPA';
  MPVEncoding                 = 'MPV';
  NVEncoding                  = 'nv';
  PCMMuLawEncoding            = 'PCMU';
  PCMALawEncoding             = 'PCMA';
  QCELPEncoding               = 'QCELP';
  RedundancyEncoding          = 'RED';
  TelephoneEventEncoding      = 'telephone-event';
  RedundancyEncodingParameter = 'red';
  T140ClockRate               = 1000;
  T140Encoding                = 't140';

  TelephoneEventMimeType  = 'audio/' + TelephoneEventEncoding;
  InterleavedT140MimeType = 'audio/' + T140Encoding;
  RedundantT140MimeType   = 'text/' + RedundancyEncoding;
  T140MimeType            = 'text/' + T140Encoding;

  DTMF0     = 0;
  DTMF1     = 1;
  DTMF2     = 2;
  DTMF3     = 3;
  DTMF4     = 4;
  DTMF5     = 5;
  DTMF6     = 6;
  DTMF7     = 7;
  DTMF8     = 8;
  DTMF9     = 9;
  DTMFStar  = 10;
  DTMFHash  = 11;
  DTMFA     = 12;
  DTMFB     = 13;
  DTMFC     = 14;
  DTMFD     = 15;
  DTMFFlash = 16;


  RTCPSenderReport       = 200;
  RTCPReceiverReport     = 201;
  RTCPSourceDescription  = 202;
  RTCPGoodbye            = 203;
  RTCPApplicationDefined = 204;

  SDESEnd   = 0;
  SDESCName = 1;
  SDESName  = 2;
  SDESEmail = 3;
  SDESPhone = 4;
  SDESLoc   = 5;
  SDESTool  = 6;
  SDESNote  = 7;
  SDESPriv  = 8;

implementation

uses
  IdGlobal;

var
  GNullPayload: TIdRTPPayload;

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

function ReadRemainderOfStream(Src: TStream): String;
const
  BufLen = 100;
var
  Buf:  array[1..BufLen] of Char;
  Read: Integer;
begin
  FillChar(Buf, Length(Buf), 0);
  Result := '';

  repeat
    Read := Src.Read(Buf, BufLen);
    Result := Result + Copy(Buf, 1, Read);
  until (Read < BufLen);
end;

function ReadString(Src: TStream; const Length: Cardinal): String;
const
  BufLen = 100;
var
  Buf:   array[1..100] of Char;
  Read:  Integer;
  Total: Cardinal;
begin
  FillChar(Buf, System.Length(Buf), 0);
  Result := '';

  Total := 0;
  repeat
    Read := Src.Read(Buf, Min(BufLen, Length));
    Inc(Total, Read);
    Result := Result + Copy(Buf, 1, Read);
  until (Total >= Length) or (Read = 0);
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

class function TIdRTPEncoding.CreateEncoding(Value: String): TIdRTPEncoding;
var
  Name:       String;
  ClockRate:  Cardinal;
  Parameters: String;
begin
  Name       := Fetch(Value, '/');
  ClockRate  := StrToInt(Fetch(Value, '/'));
  Parameters := Value;

  if (Name = T140Encoding) then
    Result := TIdRTPT140Encoding.Create(Name,
                                        ClockRate,
                                        Parameters)
  else if (Name = TelephoneEventEncoding) then
    Result := TIdRTPTelephoneEventEncoding.Create(Name,
                                                  ClockRate,
                                                  Parameters)
  else
    Result := TIdRTPEncoding.Create(Name,
                                    ClockRate,
                                    Parameters);
end;

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

function TIdRTPEncoding.Clone: TIdRTPEncoding;
begin
  Result := TIdRTPEncodingClass(Self.ClassType).Create(Self);
end;

function TIdRTPEncoding.CreatePayload: TIdRTPPayload;
begin
  Result := Self.PayloadType.Create;
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

function TIdRTPEncoding.IsReserved: Boolean;
begin
  Result := false;
end;

function TIdRTPEncoding.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdRawPayload;
end;

//******************************************************************************
//* TIdRTPT140Encoding                                                         *
//******************************************************************************
//* TIdRTPT140Encoding Public methods ******************************************

function TIdRTPT140Encoding.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdT140Payload;
end;

//******************************************************************************
//* TIdRTPTelephoneEventEncoding                                               *
//******************************************************************************
//* TIdRTPTelephoneEventEncoding Public methods ********************************

function TIdRTPTelephoneEventEncoding.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdTelephoneEventPayload;
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

function TIdRTPNullEncoding.Clone: TIdRTPEncoding;
begin
  Result := TIdRTPNullEncoding.Create(Self);
end;

function TIdRTPNullEncoding.IsNull: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdRTPReservedEncoding                                                     *
//******************************************************************************
//* TIdRTPReservedEncoding Public methods **************************************

constructor TIdRTPReservedEncoding.Create(const Name: String;
                                          const ClockRate: Cardinal;
                                          const Parameters: String = '');
begin
  inherited Create('', 0, '');
end;

constructor TIdRTPReservedEncoding.Create(const Src: TIdRTPEncoding);
begin
  inherited Create('', 0, '');
end;

function TIdRTPReservedEncoding.AsString: String;
begin
  Result := '';
end;

function TIdRTPReservedEncoding.Clone: TIdRTPEncoding;
begin
  Result := TIdRTPReservedEncoding.Create(Self);
end;

function TIdRTPReservedEncoding.IsReserved: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdRTPPayload                                                              *
//******************************************************************************
//* TIdRTPPayload Public methods ***********************************************

class function TIdRTPPayload.CreatePayload(const Encoding: TIdRTPEncoding): TIdRTPPayload;
begin
  if Encoding.IsNull then
    Result := TIdRawPayload.Create
  else if (Encoding.Name = T140Encoding) then
    Result := TIdT140Payload.Create
  else if (Encoding.Name = TelephoneEventEncoding) then
    Result := TIdTelephoneEventPayload.Create
  else
    Result := TIdRawPayload.Create;
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

class function TIdRTPPayload.NullPayload: TIdRTPPayload;
begin
  if not Assigned(GNullPayload) then
    GNullPayload := TIdNullPayload.Create;

  Result := GNullPayload;
end;

function TIdRTPPayload.IsNull: Boolean;
begin
  Result := false;
end;

procedure TIdRTPPayload.ReadFrom(Src: TStream);
begin
end;

procedure TIdRTPPayload.PrintOn(Dest: TStream);
begin
end;

//******************************************************************************
//* TIdNullPayload                                                             *
//******************************************************************************
//* TIdNullPayload Public methods **********************************************

function TIdNullPayload.IsNull: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIdRawPayload                                                              *
//******************************************************************************
//* TIdRawPayload Public methods ***********************************************

procedure TIdRawPayload.ReadFrom(Src: TStream);
begin
  Self.Data := ReadRemainderOfStream(Src);
end;

procedure TIdRawPayload.PrintOn(Dest: TStream);
begin
  Dest.Write(PChar(Self.Data)^, Length(Self.Data));
end;

//******************************************************************************
//* TIdT140Payload                                                             *
//******************************************************************************
//* TIdT140Payload Public methods **********************************************

procedure TIdT140Payload.ReadFrom(Src: TStream);
begin
  Self.BlockCount := ReadWord(Src);

  Self.Block := ReadRemainderOfStream(Src);
end;

procedure TIdT140Payload.PrintOn(Dest: TStream);
begin
  WriteWord(Dest, Self.BlockCount);

  if (Self.Block <> '') then
    Dest.Write(PChar(Self.Block)^, Length(Self.Block));
end;

//******************************************************************************
//* TIdTelephoneEventPayload                                                   *
//******************************************************************************
//* TIdTelephoneEventPayload Public methods ***********************************

procedure TIdTelephoneEventPayload.ReadFrom(Src: TStream);
var
  B: Byte;
begin
  Src.Read(B, 1);
  Self.Event := B;

  Src.Read(B, 1);
  Self.IsEnd       := B and $80 <> 0;
  Self.ReservedBit := B and $40 <> 0;
  Self.Volume      := B and $3F;

  Self.Duration := ReadWord(Src);
end;

procedure TIdTelephoneEventPayload.PrintOn(Dest: TStream);
begin
end;

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
  if Encoding.IsNull then
    Self.RemoveEncoding(PayloadType)
  else
    Self.AddEncodingAsReference(Encoding.Clone, PayloadType);
end;

procedure TIdRTPProfile.Assign(Src: TPersistent);
var
  I:            TIdRTPPayloadType;
  OtherProfile: TIdRTPProfile;
begin
  if (Src is TIdRTPProfile) then begin
    OtherProfile := Src as TIdRTPProfile;

    for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do
      Self.AddEncoding(OtherProfile.EncodingFor(I), I);
  end
  else
    inherited Assign(Src);
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
  if    (Self.Encodings[PayloadType] <> Self.NullEncoding)
    and (Self.Encodings[PayloadType] <> Self.Reserved) then
    Self.Encodings[PayloadType].Free;
    
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
  if    (Self.Encodings[PayloadType] <> Self.NullEncoding)
    and (Self.Encodings[PayloadType] <> Self.Reserved) then begin
    Self.Encodings[PayloadType].Free;
    Self.Encodings[PayloadType] := Self.NullEncoding;
  end;
end;

//******************************************************************************
//* TIdAudioVisualProfile                                                      *
//******************************************************************************
//* TIdAudioVisualProfile Public methods ***************************************

constructor TIdAudioVisualProfile.Create;
begin
  inherited Create;

  Self.AddEncodingAsReference(TIdRTPEncoding.Create(PCMMuLawEncoding, 8000),        0);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(GSMEncoding,      8000),        3);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(G723Encoding,     8000),        4);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(DVI4Encoding,     8000),        5);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(DVI4Encoding,     16000),       6);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(LPCEncoding,      8000),        7);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(PCMALawEncoding,  8000),        8);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(G722Encoding,     8000),        9);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(L16Encoding,      44100, '2'), 10);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(L16Encoding,      44100, '1'), 11);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(QCELPEncoding,    8000),       12);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(CNEncoding,       8000),       13);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(MPAEncoding,      90000),      14);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(G728Encoding,     8000),       15);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(DVI4Encoding,     11025),      16);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(DVI4Encoding,     22050),      17);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(G729Encoding,     8000),       18);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(CelBEncoding,     90000),      25);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(JPEGEncoding,     90000),      26);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(NVEncoding,       90000),      28);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(H261Encoding,     90000),      31);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(MPVEncoding,      90000),      32);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(MP2TEncoding,     90000),      33);
  Self.AddEncodingAsReference(TIdRTPEncoding.Create(H263Encoding,     90000),      34);

  Self.ReserveRange(1,  2);
  Self.ReserveRange(19, 24);

  Self.ReservePayloadType(27);

  Self.ReserveRange(29, 30);
  Self.ReserveRange(35, 95);
end;

procedure TIdAudioVisualProfile.Assign(Src: TPersistent);
var
  I:            TIdRTPPayloadType;
  OtherProfile: TIdRTPProfile;
begin
  if (Src is TIdRTPProfile) then begin
    OtherProfile := Src as TIdRTPProfile;

    for I := 96 to 127 do
      Self.AddEncoding(OtherProfile.EncodingFor(I), I);
  end
  else
    inherited Assign(Src);
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
//* TIdRTPHeaderExtension                                                      *
//******************************************************************************
//* TIdRTPHeaderExtension Public methods ***************************************

procedure TIdRTPHeaderExtension.ReadFrom(Src: TStream);
var
  I: Integer;
begin
  Self.ProfileDefinedValue := ReadWord(Src);
  Self.Length              := ReadWord(Src);

  for I := 0 to Self.Length - 1 do
    Self.Data[I] := ReadCardinal(Src);
end;

procedure TIdRTPHeaderExtension.PrintOn(Dest: TStream);
var
  I: Integer;
begin
  WriteWord(Dest, Self.ProfileDefinedValue);
  WriteWord(Dest, Self.Length);

  for I := 0 to Self.Length - 1 do
    WriteCardinal(Dest, Self.Data[I]);
end;

//* TIdRTPHeaderExtension Private methods ***************************************

function TIdRTPHeaderExtension.GetData(Index: Word): Cardinal;
begin
  Result := fData[Index];
end;

function TIdRTPHeaderExtension.GetLength: Word;
begin
  Result := System.Length(fData);
end;

procedure TIdRTPHeaderExtension.SetData(Index: Word; const Value: Cardinal);
begin
  fData[Index] := Value;
end;

procedure TIdRTPHeaderExtension.SetLength(const Value: Word);
begin
  System.SetLength(fData, Value);
end;

//******************************************************************************
//* TIdRTPPacket                                                               *
//******************************************************************************
//* TIdRTPPacket Public methods ************************************************

constructor TIdRTPPacket.Create(const Profile: TIdRTPProfile);
begin
  inherited Create;

  fHeaderExtension := TIdRTPHeaderExtension.Create;
  fPayload         := TIdRTPPayload.NullPayload;

  Self.Profile := Profile;
  Self.Version := Self.DefaultVersion;
end;

destructor TIdRTPPacket.Destroy;
begin
  fHeaderExtension.Free;

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

  if Self.HasExtension then
    Self.HeaderExtension.ReadFrom(Src);

  Self.ReadPayload(Src);  
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

//******************************************************************************
//* TIdRTCPByePacket                                                           *
//******************************************************************************
//* TIdRTCPByePacket Public methods ********************************************

procedure TIdRTCPByePacket.PrintOn(Dest: TStream);
begin
end;

procedure TIdRTCPByePacket.ReadFrom(Src: TStream);
var
  B: Byte;
  I: Integer;
begin
  Src.Read(B, 1);
  Self.Version    := B and $C0 shr 6;
  Self.HasPadding := (B and $20) <> 0;

  Self.SourceCount := B and $1F;

  Src.Read(B, 1);
  Assert(RTCPGoodBye = B, 'TIdRTCPByePacket packet type');

  Self.Length := ReadWord(Src);

  for I := 0 to Self.SourceCount - 1 do
    Self.Sources[I] := ReadCardinal(Src);

  Self.ReasonLength := ReadWord(Src);

  Self.Reason := ReadString(Src, Self.ReasonLength);
end;

//* TIdRTCPByePacket Protected methods *****************************************

function TIdRTCPByePacket.GetPacketType: Cardinal;
begin
  Result := RTCPGoodbye;
end;

//* TIdRTCPByePacket Private methods *******************************************

function TIdRTCPByePacket.GetSourceCount: TIdRTCPSourceCount;
begin
  Result := System.Length(fSources);
end;

function TIdRTCPByePacket.GetSource(Index: TIdRTCPSourceCount): Cardinal;
begin
  Result := fSources[Index];
end;

procedure TIdRTCPByePacket.SetSourceCount(const Value: TIdRTCPSourceCount);
begin
  SetLength(fSources, Value);
end;

procedure TIdRTCPByePacket.SetSource(Index: TIdRTCPSourceCount;
                                     const Value: Cardinal);
begin
  fSources[Index] := Value;
end;

initialization
finalization
  GNullPayload.Free;
end.
