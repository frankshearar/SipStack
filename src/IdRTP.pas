unit IdRTP;

interface

uses
  Classes, Contnrs, SysUtils;

type
  TIdCardinalArray        = array of Cardinal;
  TIdTelephoneEventVolume = 0..63;
  TIdNTPTimestamp         = record
    IntegerPart:    Cardinal;
    FractionalPart: Cardinal;
  end;
  TIdLoss                 = 0..16777215; // 3-byte integer: 2^24 - 1
  TIdFiveBitInt           = 0..31;
  TIdRTCPSourceCount      = TIdFiveBitInt;
  TIdRTCPSubType          = TIdFiveBitInt;
  TIdRTCPReceptionCount   = TIdFiveBitInt;
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

  // I represent the Null Encoding.
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

  // I represent the payload in an RTP packet. My subclasses implement
  // appropriate parsing and outputting.
  //
  // I offer a Flyweight Null Payload.
  TIdRTPPayload = class(TObject)
  public
    class function CreatePayload(const Encoding: TIdRTPEncoding): TIdRTPPayload;
    class function CreateFrom(const Encoding: TIdRTPEncoding;
                              Src: TStream): TIdRTPPayload;
    class function NullPayload: TIdRTPPayload;

    function  IsNull: Boolean; virtual;
    function  Length: Cardinal; virtual;
    procedure ReadFrom(Src: TStream); virtual;
    procedure PrintOn(Dest: TStream); virtual;
  end;

  // I represent the Null payload - a Null Object representing the absence of a
  // payload.
  TIdNullPayload = class(TIdRTPPayload)
  public
    function IsNull: Boolean; override;
  end;

  // I represent a raw (i.e., unparsed) payload.
  // I typically provide a fallback case for a malconfigured RTP server.
  TIdRawPayload = class(TIdRTPPayload)
  private
    fData: String;
  public
    function  Length: Cardinal; override;
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

  // I represent a 1-1 association map between encodings and RTP Payload Types.
  // RTP packets use me to determine how their payload should be interpreted.
  // Because I represent a  1-1 relation, you cannot add the same encoding to
  // me twice. If you try, the payload type you try to overwrite will remain
  // unchanged.
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

  // I represent the profile defined in RFC 3551. As such, don't bother trying
  // to change the encodings of any reserved or assigned payload types. I only
  // allow the alteration of the dynamic payload types - 96-127.
  TIdAudioVisualProfile = class(TIdRTPProfile)
  private
    procedure ReserveRange(const LowPT, HighPT: TIdRTPPayloadType);
  public
    constructor Create; override;

    procedure Assign(Src: TPersistent); override;
    function  TransportDesc: String; override;
  end;

  // I represent a Header Extension. RFC 3550 doesn't define an interpretation
  // for my data beyond the length field, so my subclasses must provide more
  // meaningful properties for this data.
  TIdRTPHeaderExtension = class(TObject)
  private
    fData:                array of Cardinal;
    fProfileDefinedValue: Word;

    function  GetData(Index: Word): Cardinal;
    function  GetLength: Word;
    procedure SetData(Index: Word; const Value: Cardinal);
    procedure SetLength(const Value: Word);
  public
    function  OctetCount: Cardinal;
    procedure ReadFrom(Src: TStream);
    procedure PrintOn(Dest: TStream);

    property Length:              Word     read GetLength write SetLength;
    property ProfileDefinedValue: Word     read fProfileDefinedValue write fProfileDefinedValue;
    property Data[Index: Word]:   Cardinal read GetData write SetData;
  end;

  TIdRTCPReportBlock = class(TObject)
  private
    fCumulativeLoss:     TIdLoss;
    fDelaySinceLastSR:   Cardinal;
    fFractionLost:       Byte;
    fHighestSeqNo:       Cardinal;
    fInterArrivalJitter: Cardinal;
    fLastSenderReport:   Cardinal;
    fSyncSrcID:          Cardinal;
  public
    procedure PrintOn(Dest: TStream);
    procedure ReadFrom(Src: TStream);

    property CumulativeLoss:     TIdLoss  read fCumulativeLoss write fCumulativeLoss;
    property DelaySinceLastSR:   Cardinal read fDelaySinceLastSR write fDelaySinceLastSR;
    property FractionLost:       Byte     read fFractionLost write fFractionLost;
    property HighestSeqNo:       Cardinal read fHighestSeqNo write fHighestSeqNo;
    property InterArrivalJitter: Cardinal read fInterArrivalJitter write fInterArrivalJitter;
    property LastSenderReport:   Cardinal read fLastSenderReport write fLastSenderReport;
    property SyncSrcID:          Cardinal read fSyncSrcID write fSyncSrcID;
  end;

  TIdRTPBasePacket = class(TObject)
  private
    fHasPadding: Boolean;
    fLength:     Word;
    fSyncSrcID:  Cardinal;
    fVersion:    TIdRTPVersion;
  protected
    function  GetSyncSrcID: Cardinal; virtual;
    procedure PrintPadding(Dest: TStream);
    procedure SetSyncSrcID(const Value: Cardinal); virtual;
  public
    class function IsRTCPPayloadType(const PayloadType: Byte): Boolean;
    class function CreateFrom(Src: TStream;
                              const Profile: TIdRTPProfile): TIdRTPBasePacket;

    constructor Create;

    function IsRTCP: Boolean; virtual; abstract;
    function IsRTP: Boolean; virtual; abstract;
    procedure ReadFrom(Src: TStream); virtual; abstract;
    function  RealLength: Word; virtual; abstract;
    procedure PrintOn(Dest: TStream); virtual; abstract;

    property HasPadding: Boolean       read fHasPadding write fHasPadding;
    property Length:     Word          read fLength write fLength;
    property SyncSrcID:  Cardinal      read GetSyncSrcID write SetSyncSrcID;
    property Version:    TIdRTPVersion read fVersion write fVersion;
  end;

  // I represent a packet of the Real-time Transport Protocol.
  TIdRTPPacket = class(TIdRTPBasePacket)
  private
    fCsrcCount:       TIdRTPCsrcCount;
    fCsrcIDs:         TIdCardinalArray;
    fHasExtension:    Boolean;
    fHeaderExtension: TIdRTPHeaderExtension;
    fIsMarker:        Boolean;
    fPayload:         TIdRTPPayload;
    fPayloadType:     TIdRTPPayloadType;
    fSequenceNo:      TIdRTPSequenceNo;
    fTimestamp:       Cardinal;
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
    function  IsRTCP: Boolean; override;
    function  IsRTP: Boolean; override;
    procedure ReadFrom(Src: TStream); override;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadPayload(Src: TStream); overload;
    procedure ReadPayload(Src: String); overload;
    function  RealLength: Word; override;

    property CsrcCount:                       TIdRTPCsrcCount       read GetCsrcCount write SetCsrcCount;
    property CsrcIDs[Index: TIdRTPCsrcCount]: Cardinal              read GetCsrcID write SetCsrcID;
    property HasExtension:                    Boolean               read fHasExtension write fHasExtension;
    property HeaderExtension:                 TIdRTPHeaderExtension read fHeaderExtension;
    property IsMarker:                        Boolean               read fIsMarker write fIsMarker;
    property Payload:                         TIdRTPPayload         read fPayload;
    property PayloadType:                     TIdRTPPayloadType     read fPayloadType write fPayloadType;
    property SequenceNo:                      TIdRTPSequenceNo      read fSequenceNo write fSequenceNo;
    property Timestamp:                       Cardinal              read fTimestamp write fTimestamp;
  end;

  TIdRTCPPacket = class;
  TIdRTCPPacketClass = class of TIdRTCPPacket;

  // I represent a packet in the Real-time Transport Control Protocol, as defined in
  // RFC 3550 section 6.
  TIdRTCPPacket = class(TIdRTPBasePacket)
  protected
    function GetPacketType: Cardinal; virtual; abstract;
  public
    class function RTCPType(const PacketType: Byte): TIdRTCPPacketClass;

    constructor Create; virtual;

    function IsRTCP: Boolean; override;
    function IsRTP: Boolean; override;

    property PacketType: Cardinal read GetPacketType;
  end;

  // I represent an SR RTCP packet. Please note that I clobber my report
  // objects when you change my ReceptionReportCount property - I free all
  // my existing reports and create new instances. I guarantee that you
  // won't get a nil pointer from ReportAt.
  // Active senders of data in an RTP session send me to give transmission
  // and reception statistics.
  TIdRTCPSenderReportPacket = class(TIdRTCPPacket)
  private
    fExtension:        String;
    fNTPTimestamp:     TIdNTPTimestamp;
    fOctetCount:       Cardinal;
    fPacketCount:      Cardinal;
    fReceptionReports: array of TIdRTCPReportBlock;
    fRTPTimestamp:     Cardinal;

    procedure ClearReportBlocks;
    function  GetReceptionReportCount: TIdRTCPReceptionCount;
    procedure ReadAllReportBlocks(Src: TStream);
    procedure ReInitialiseReportBlocks;
    procedure SetReceptionReportCount(const Value: TIdRTCPReceptionCount);
  protected
    function  GetPacketType: Cardinal; override;
  public
    destructor Destroy; override;

    function  ReportAt(const Index: Integer): TIdRTCPReportBlock;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    function  RealLength: Word; override;

    property Extension:            String                read fExtension write fExtension;
    property NTPTimestamp:         TIdNTPTimestamp       read fNTPTimestamp write fNTPTimestamp;
    property OctetCount:           Cardinal              read fOctetCount write fOctetCount;
    property PacketCount:          Cardinal              read fPacketCount write fPacketCount;
    property ReceptionReportCount: TIdRTCPReceptionCount read GetReceptionReportCount write SetReceptionReportCount;
    property RTPTimestamp:         Cardinal              read fRTPTimestamp write fRTPTimestamp;
  end;

  TIdRTCPReceiverReportPacket = class(TIdRTCPPacket);
  TIdRTCPSourceDescriptionPacket = class(TIdRTCPPacket);

  // I represent an RTCP Bye packet. You use me to remove yourself from an RTP
  // session. RTP servers that receive me remove the SSRC that sent me from
  // their member tables.
  TIdRTCPByePacket = class(TIdRTCPPacket)
  private
    fSources:      TIdCardinalArray;
    fReason:       String;
    fReasonLength: Word;

    function  GetSourceCount: TIdRTCPSourceCount;
    function  GetSource(Index: TIdRTCPSourceCount): Cardinal;
    procedure SetReason(const Value: String);
    procedure SetSource(Index: TIdRTCPSourceCount;
                        const Value: Cardinal);
    procedure SetSourceCount(const Value: TIdRTCPSourceCount);
  protected
    function  GetPacketType: Cardinal; override;
    function  GetSyncSrcID: Cardinal; override;
    procedure SetSyncSrcID(const Value: Cardinal); override;
  public
    constructor Create; override;

    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    function  RealLength: Word; override;

    property Reason:                             String             read fReason write SetReason;
    property ReasonLength:                       Word               read fReasonLength write fReasonLength;
    property SourceCount:                        TIdRTCPSourceCount read GetSourceCount write SetSourceCount;
    property Sources[Index: TIdRTCPSourceCount]: Cardinal           read GetSource write SetSource;
  end;

  TIdRTCPApplicationDefinedPacket = class(TIdRTCPPacket)
  private
    fData:    String;
    fName:    String;
    fSubType: TIdRTCPSubType;

    procedure SetData(const Value: String);
    procedure SetName(const Value: String);
  protected
    function GetPacketType: Cardinal; override;
  public
    constructor Create; override;

    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    function  RealLength: Word; override;

    property Data:    String         read fData write SetData;
    property Name:    String         read fName write SetName;
    property SubType: TIdRTCPSubType read fSubType write fSubType;
  end;

  // I provide a buffer to objects that receive RTP packets. I assemble these
  // packets, making sure I assemble the RTP stream in the correct order.
  TIdRTPPacketBuffer = class(TObject)
  private
    List: TObjectList;

    function  AppropriateIndex(const Pkt: TIdRTPPacket): Integer;
    procedure Clear;
    function  PacketAt(const Index: Integer): TIdRTPPacket;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(const Pkt: TIdRTPPacket);
    function  Last: TIdRTPPacket;
    procedure RemoveLast;
  end;

  ENoPayloadTypeFound = class(Exception);

function DateTimeToNTPSeconds(const DT: TDateTime): Cardinal;
function DateTimeToNTPTimestamp(const DT: TDateTime): TIdNTPTimestamp;
function EncodeAsString(Value: Cardinal): String; overload;
function EncodeAsString(Value: Word): String; overload;
function HtoNL(Value: Cardinal): Cardinal;
function HtoNS(Value: Word): Word;
function MultiplyCardinal(FirstValue, SecondValue: Cardinal): Cardinal;
function NtoHL(Value: Cardinal): Cardinal;
function NtoHS(Value: Word): Cardinal;

function  ReadCardinal(Src: TStream): Cardinal;
procedure ReadNTPTimestamp(Src: TStream; var Timestamp: TIdNTPTimestamp);
function  ReadRemainderOfStream(Src: TStream): String;
function  ReadString(Src: TStream; const Length: Cardinal): String;
function  ReadWord(Src: TStream): Word;
procedure WriteCardinal(Dest: TStream; Value: Cardinal);
procedure WriteNTPTimestamp(Dest: TStream; Value: TIdNTPTimestamp);
procedure WriteWord(Dest: TStream; Value: Word);

// From RFC 3550 and 3551
const
  RFC3550Version     = 2;
  AudioVisualProfile = 'RTP/AVP';

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

// From RFC 2793
const
  RedundancyEncoding          = 'RED';
  RedundancyEncodingParameter = 'RED';
  T140ClockRate               = 1000;
  T140Encoding                = 'T140';
  InterleavedT140MimeType     = 'audio/' + T140Encoding;
  RedundantT140MimeType       = 'text/' + RedundancyEncoding;
  T140MimeType                = 'text/' + T140Encoding;

// From RFC 2833
const
  DTMF0                  = 0;
  DTMF1                  = 1;
  DTMF2                  = 2;
  DTMF3                  = 3;
  DTMF4                  = 4;
  DTMF5                  = 5;
  DTMF6                  = 6;
  DTMF7                  = 7;
  DTMF8                  = 8;
  DTMF9                  = 9;
  DTMFStar               = 10;
  DTMFHash               = 11;
  DTMFA                  = 12;
  DTMFB                  = 13;
  DTMFC                  = 14;
  DTMFD                  = 15;
  DTMFFlash              = 16;
  TelephoneEventEncoding = 'telephone-event';
  TelephoneEventMimeType = 'audio/' + TelephoneEventEncoding;

implementation

uses
  DateUtils, IdGlobal;

var
  GNullPayload: TIdRTPPayload;

const
  JanOne1900 = 2;

//******************************************************************************
//* Unit public functions & procedures                                         *
//******************************************************************************

function DateTimeToNTPSeconds(const DT: TDateTime): Cardinal;
var
  Days: Cardinal;
begin
  Days := Trunc(DT) - JanOne1900;

  Result := MultiplyCardinal(Days, SecsPerDay) + SecondOfTheDay(DT);
end;

// Caveat Programmer: TDateTime has the floating point nature. Don't expect
// enormous precision.
function DateTimeToNTPTimestamp(const DT: TDateTime): TIdNTPTimestamp;
var
  Divisor:         Int64;
  Fraction:        Double;
  FractionBit:     Cardinal;
  PartOfOneSecond: Double;
begin
  if (DT < 2) then
    raise EConvertError.Create('DT < 1900/01/01');

  Result.IntegerPart := DateTimeToNTPSeconds(DT);

  PartOfOneSecond       := MilliSecondOfTheSecond(DT)/1000;
  Result.FractionalPart := 0;
  Divisor               := 2;
  FractionBit           := $80000000;
  while (Divisor <= $40000000) and (PartOfOneSecond > 0) do begin
    Fraction := 1/Divisor;
    if ((PartOfOneSecond - Fraction) >= 0) then begin
      Result.FractionalPart := Result.FractionalPart or FractionBit;
      PartOfOneSecond := PartOfOneSecond - Fraction;
    end;
    FractionBit := FractionBit div 2;
    Divisor := MultiplyCardinal(2, Divisor);
  end;
end;

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

// Delphi 6 & 7 both compile FirstValue*SecondValue as an imul
// opcode. imul performs a SIGNED integer multiplication, and so if
// FirstValue * SecondValue > $7fffffff then the overflow flag gets set. If you
// have overflow checking on, that means that FOR A PERFECTLY VALID
// multiplication (e.g., $7f000000 * 2) you will get an EIntOverflow.
function MultiplyCardinal(FirstValue, SecondValue: Cardinal): Cardinal;
asm
  mul edx
  jno @end
  call System.@IntOver
 @end:
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

procedure ReadNTPTimestamp(Src: TStream; var Timestamp: TIdNTPTimestamp);
begin
  Timestamp.IntegerPart    := ReadCardinal(Src);
  Timestamp.FractionalPart := ReadCardinal(Src);
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

procedure WriteNTPTimestamp(Dest: TStream; Value: TIdNTPTimestamp);
begin
  WriteCardinal(Dest, Value.IntegerPart);
  WriteCardinal(Dest, Value.FractionalPart);
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

  if (Lowercase(Name) = Lowercase(T140Encoding)) then
    Result := TIdRTPT140Encoding.Create(Name,
                                        ClockRate,
                                        Parameters)
  else if (Lowercase(Name) = Lowercase(TelephoneEventEncoding)) then
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

function TIdRTPPayload.Length: Cardinal;
begin
  Result := 0;
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

function TIdRawPayload.Length: Cardinal;
begin
  Result := System.Length(Self.Data);
end;

procedure TIdRawPayload.ReadFrom(Src: TStream);
begin
  Self.Data := ReadRemainderOfStream(Src);
end;

procedure TIdRawPayload.PrintOn(Dest: TStream);
begin
  Dest.Write(PChar(Self.Data)^, System.Length(Self.Data));
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
    Dest.Write(PChar(Self.Block)^, System.Length(Self.Block));
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
var
  B: Byte;
begin
  Dest.Write(Self.Event, 1);

  B := Self.Volume;
  if Self.IsEnd then
    B := B or $80;
  Dest.Write(B, 1);

  WriteWord(Dest, Self.Duration);
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

function TIdRTPHeaderExtension.OctetCount: Cardinal;
begin
  Result := 4*Self.Length + 4;
end;

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
//* TIdRTCPReportBlock                                                         *
//******************************************************************************
//* TIdRTCPReportBlock Public methods ******************************************

procedure TIdRTCPReportBlock.PrintOn(Dest: TStream);
var
  Loss: Cardinal;
begin
  WriteCardinal(Dest, Self.SyncSrcID);
  Loss := (Self.FractionLost shl 24) or Self.CumulativeLoss;
  WriteCardinal(Dest, Loss);
  WriteCardinal(Dest, Self.HighestSeqNo);
  WriteCardinal(Dest, Self.InterArrivalJitter);
  WriteCardinal(Dest, Self.LastSenderReport);
  WriteCardinal(Dest, Self.DelaySinceLastSR);
end;

procedure TIdRTCPReportBlock.ReadFrom(Src: TStream);
var
  Loss: Cardinal;
begin
  Self.SyncSrcID          := ReadCardinal(Src);
  Loss                    := ReadCardinal(Src);
  Self.FractionLost       := Loss shr 24;
  Self.CumulativeLoss     := Loss and $00ffffff;
  Self.HighestSeqNo       := ReadCardinal(Src);
  Self.InterArrivalJitter := ReadCardinal(Src);
  Self.LastSenderReport   := ReadCardinal(Src);
  Self.DelaySinceLastSR   := ReadCardinal(Src);
end;

//******************************************************************************
//* TIdRTPBasePacket                                                           *
//******************************************************************************
//* TIdRTPBasePacket Public methods ********************************************

class function TIdRTPBasePacket.IsRTCPPayloadType(const PayloadType: Byte): Boolean;
begin
  Result := (PayloadType >= RTCPSenderReport)
        and (PayloadType <= RTCPApplicationDefined);
end;

class function TIdRTPBasePacket.CreateFrom(Src: TStream;
                                           const Profile: TIdRTPProfile): TIdRTPBasePacket;
var
  FirstWord: Word;
  PacketType: Byte;
begin
  FirstWord := ReadWord(Src);
  Src.Seek(0, soFromBeginning);

  PacketType := FirstWord and $00FF;

  if Self.IsRTCPPayloadType(PacketType) then begin
    Result := TIdRTCPPacket.RTCPType(PacketType).Create;
  end
  else
    Result := TIdRTPPacket.Create(Profile);
end;

constructor TIdRTPBasePacket.Create;
begin
  Self.Version := RFC3550Version;
end;

//* TIdRTPBasePacket Protected methods *****************************************

function TIdRTPBasePacket.GetSyncSrcID: Cardinal;
begin
  Result := fSyncSrcID;
end;

procedure TIdRTPBasePacket.PrintPadding(Dest: TStream);
var
  B:         Byte;
  I:         Integer;
  PadLength: Byte;
begin
  //   padding (P): 1 bit
  //      If the padding bit is set, this individual RTCP packet contains
  //      some additional padding octets at the end which are not part of
  //      the control information but are included in the length field.  The
  //      last octet of the padding is a count of how many padding octets
  //      should be ignored, including itself (it will be a multiple of
  //      four).
  Assert(Self.Length > Self.RealLength,
         'Length must be set before padding is printed');
  Assert((Self.Length - Self.RealLength) mod 4 = 0,
         'Padding must be a multiple of 4');

  B := 0;
  PadLength := Self.Length - Self.RealLength;
  for I := 1 to PadLength - 1 do
    Dest.Write(B, 1);

  // The written padding length includes itself
  Dest.Write(PadLength, 1);
end;

procedure TIdRTPBasePacket.SetSyncSrcID(const Value: Cardinal);
begin
  fSyncSrcID := Value;
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

function TIdRTPPacket.IsRTCP: Boolean;
begin
  Result := false;
end;

function TIdRTPPacket.IsRTP: Boolean;
begin
  Result := true;
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

function TIdRTPPacket.RealLength: Word;
begin
  Result := 12
          + Self.CsrcCount*4
          + Self.Payload.Length;

  if Self.HasExtension then
    Result := Result + Self.HeaderExtension.OctetCount;
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
//* TIdRTCPPacket                                                              *
//******************************************************************************
//* TIdRTCPPacket Public methods ***********************************************

class function TIdRTCPPacket.RTCPType(const PacketType: Byte): TIdRTCPPacketClass;
begin
  case PacketType of
    RTCPSenderReport:       Result := TIdRTCPSenderReportPacket;
    RTCPReceiverReport:     Result := TIdRTCPReceiverReportPacket;
    RTCPSourceDescription:  Result := TIdRTCPSourceDescriptionPacket;
    RTCPGoodbye:            Result := TIdRTCPByePacket;
    RTCPApplicationDefined: Result := TIdRTCPApplicationDefinedPacket;
  else
    Result := nil;
  end;
end;

constructor TIdRTCPPacket.Create;
begin
  inherited Create;
end;

function TIdRTCPPacket.IsRTCP: Boolean;
begin
  Result := true;
end;

function TIdRTCPPacket.IsRTP: Boolean;
begin
  Result := false;
end;

//******************************************************************************
//* TIdRTCPSenderReportPacket                                                  *
//******************************************************************************
//* TIdRTCPSenderReportPacket Public methods ***********************************

destructor TIdRTCPSenderReportPacket.Destroy;
begin
  Self.ClearReportBlocks;

  inherited Destroy;
end;

function TIdRTCPSenderReportPacket.ReportAt(const Index: Integer): TIdRTCPReportBlock;
begin
  if (Index < 0) or (Index >= Self.ReceptionReportCount) then
    raise EListError.Create('List index out of bounds (' + IntToStr(Index) + ')');

  Result := fReceptionReports[Index];
end;

procedure TIdRTCPSenderReportPacket.PrintOn(Dest: TStream);
var
  B: Byte;
  I: Integer;
begin
  // Calculate length for real here? Hard-code RTP version too?
  B := Self.Version shl 6;
  if Self.HasPadding then B := B or $20;
  B := B or Self.ReceptionReportCount;
  Dest.Write(B, 1);

  B := Self.GetPacketType;
  Dest.Write(B, 1);

  WriteWord(Dest, Self.Length);
  WriteCardinal(Dest, Self.SyncSrcID);
  WriteNTPTimestamp(Dest, Self.NTPTimestamp);
  WriteCardinal(Dest, Self.RTPTimestamp);
  WriteCardinal(Dest, Self.PacketCount);
  WriteCardinal(Dest, Self.OctetCount);

  for I := 0 to Self.ReceptionReportCount - 1 do
    Self.ReportAt(I).PrintOn(Dest);

  if Self.HasPadding then
    Self.PrintPadding(Dest);
end;

procedure TIdRTCPSenderReportPacket.ReadFrom(Src: TStream);
var
  B: Byte;
  T: TIdNTPTimestamp;
begin
  Src.Read(B, 1);
  Self.Version              := B shr 6;
  Self.HasPadding           := (B and $20) > 0;
  Self.ReceptionReportCount := B and $1F;
  Src.Read(B, 1);

  Self.Length       := ReadWord(Src);
  Self.SyncSrcID    := ReadCardinal(Src);
  T.IntegerPart     := ReadCardinal(Src);
  T.FractionalPart  := ReadCardinal(Src);
  Self.NTPTimestamp := T;
  Self.RTPTimestamp := ReadCardinal(Src);
  Self.PacketCount  := ReadCardinal(Src);
  Self.OctetCount   := ReadCardinal(Src);

  Self.ReadAllReportBlocks(Src);
end;

function TIdRTCPSenderReportPacket.RealLength: Word;
begin
  // I contain at minimum 28 octets, and another 24 octets per report block.
  // And I sometimes contain header extensions.
  Result := 7*4 + Self.ReceptionReportCount*6*4 + System.Length(Self.Extension);
end;

//* TIdRTCPSenderReportPacket Protected methods ********************************

function TIdRTCPSenderReportPacket.GetPacketType: Cardinal;
begin
  Result := RTCPSenderReport;
end;

//* TIdRTCPSenderReportPacket Private methods **********************************

procedure TIdRTCPSenderReportPacket.ClearReportBlocks;
var
  I: Integer;
begin
  for I := Low(fReceptionReports) to High(fReceptionReports) do
    fReceptionReports[I].Free;
end;

procedure TIdRTCPSenderReportPacket.ReadAllReportBlocks(Src: TStream);
var
  I: Integer;
begin
  for I := Low(fReceptionReports) to High(fReceptionReports) do
    fReceptionReports[I].ReadFrom(Src);
end;

procedure TIdRTCPSenderReportPacket.ReInitialiseReportBlocks;
var
  I: Integer;
begin
  for I := Low(fReceptionReports) to High(fReceptionReports) do
    fReceptionReports[I] := TIdRTCPReportBlock.Create;
end;

function TIdRTCPSenderReportPacket.GetReceptionReportCount: TIdRTCPReceptionCount;
begin
  Result := System.Length(fReceptionReports);
end;

procedure TIdRTCPSenderReportPacket.SetReceptionReportCount(const Value: TIdRTCPReceptionCount);
begin
  Self.ClearReportBlocks;
  SetLength(fReceptionReports, Value);
  Self.ReInitialiseReportBlocks;
end;

//******************************************************************************
//* TIdRTCPByePacket                                                           *
//******************************************************************************
//* TIdRTCPByePacket Public methods ********************************************

constructor TIdRTCPByePacket.Create;
begin
  inherited Create;

  Self.SourceCount := 1;
end;

procedure TIdRTCPByePacket.PrintOn(Dest: TStream);
var
  B: Byte;
  I: Integer;
begin
  B := Self.Version shl 6;

  if Self.HasPadding then B := B or $20;
  Dest.Write(B, 1);

  B := Self.GetPacketType;
  Dest.Write(B, 1);

  WriteWord(Dest, Self.Length);

  for I := 0 to Self.SourceCount - 1 do
    WriteCardinal(Dest, Self.Sources[I]);

  if (Self.ReasonLength > 0) then begin
    WriteWord(Dest, Self.ReasonLength);
    Dest.Write(Self.Reason[1], System.Length(Self.Reason));
  end;

  if Self.HasPadding then
    Self.PrintPadding(Dest);
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

function TIdRTCPByePacket.RealLength: Word;
begin
  Result := 4 + Self.SourceCount*4;

  if (Self.Reason <> '') then
    Result := Result + SizeOf(Self.ReasonLength) + System.Length(Self.Reason);
end;

//* TIdRTCPByePacket Protected methods *****************************************

function TIdRTCPByePacket.GetPacketType: Cardinal;
begin
  Result := RTCPGoodbye;
end;

function TIdRTCPByePacket.GetSyncSrcID: Cardinal;
begin
  Result := Self.Sources[0];
end;

procedure TIdRTCPByePacket.SetSyncSrcID(const Value: Cardinal);
begin
  Self.Sources[0] := Value;
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

procedure TIdRTCPByePacket.SetReason(const Value: String);
begin
  fReason := Value;
  Self.ReasonLength := System.Length(Value);
end;

procedure TIdRTCPByePacket.SetSource(Index: TIdRTCPSourceCount;
                                     const Value: Cardinal);
begin
  Self.SourceCount := Max(Self.SourceCount, Index + 1);
  fSources[Index] := Value;
end;

procedure TIdRTCPByePacket.SetSourceCount(const Value: TIdRTCPSourceCount);
begin
  SetLength(fSources, Value);
end;

//******************************************************************************
//* TIdRTCPApplicationDefinedPacket                                            *
//******************************************************************************
//* TIdRTCPApplicationDefinedPacket Public methods *****************************

constructor TIdRTCPApplicationDefinedPacket.Create;
begin
  inherited Create;

  Self.Name := #0#0#0#0;
  Self.Length := 12;
end;

procedure TIdRTCPApplicationDefinedPacket.PrintOn(Dest: TStream);
var
  B: Byte;
begin
  B := Self.Version shl 6;

  if Self.HasPadding then B := B or $20;
  Dest.Write(B, 1);

  B := Self.GetPacketType;
  Dest.Write(B, 1);

  WriteWord(Dest, Self.Length);

  WriteCardinal(Dest, Self.SyncSrcID);

  Dest.Write(Self.Name[1], SizeOf(Self.Name));

  if (Self.Data <> '') then
    Dest.Write(Self.Data[1], SizeOf(Self.Data));

  if Self.HasPadding then
    Self.PrintPadding(Dest);  
end;

procedure TIdRTCPApplicationDefinedPacket.ReadFrom(Src: TStream);
const
  // The size of the set headers of an Application-Defined RTCP packet -
  // cf. RFC 3550, section 6.7
  DataOffset = 12;
var
  B:    Byte;
  Name: array[0..3] of Char;
begin
  Src.Read(B, 1);
  Self.Version    := B and $C0 shr 6;
  Self.HasPadding := (B and $20) <> 0;

  Src.Read(B, 1);
  Assert(RTCPApplicationDefined = B, 'TIdRTCPApplicationDefinedPacket packet type');

  Self.Length := ReadWord(Src);
  Self.SyncSrcID := ReadCardinal(Src);

  Src.Read(Name, System.Length(Name));
  Self.Name := Name;

  Self.Data := ReadString(Src, Self.Length - DataOffset);
end;

function TIdRTCPApplicationDefinedPacket.RealLength: Word;
begin
  Result := 8
            + System.Length(Self.Name)
            + System.Length(Self.Data);
end;

//* TIdRTCPApplicationDefinedPacket Protected methods **************************

function TIdRTCPApplicationDefinedPacket.GetPacketType: Cardinal;
begin
  Result := RTCPApplicationDefined;
end;

//* TIdRTCPApplicationDefinedPacket Private methods ****************************

procedure TIdRTCPApplicationDefinedPacket.SetData(const Value: String);
var
  Len: Integer;
begin
  fData := Value;

  Len := System.Length(fData);

  if (Len mod 4 <> 0) then
    while System.Length(fData) < 4*((Len div 4) + 1) do
      fData := fData + #0;
end;

procedure TIdRTCPApplicationDefinedPacket.SetName(const Value: String);
begin
  if (System.Length(Value) > 4) then
    fName := Copy(Value, 1, 4)
  else if (System.Length(Value) < 4) then begin
    fName := Value;
    while (System.Length(fName) < 4) do
      fName := fName + #0;
  end
  else
    fName := Value;
end;

//******************************************************************************
//* TIdRTPPacketBuffer                                                         *
//******************************************************************************
//* TIdRTPPacketBuffer Public methods ******************************************

constructor TIdRTPPacketBuffer.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create;
end;

destructor TIdRTPPacketBuffer.Destroy;
begin
  Self.Clear;
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdRTPPacketBuffer.Add(const Pkt: TIdRTPPacket);
begin

  Self.List.Insert(Self.AppropriateIndex(Pkt), Pkt);
end;

function TIdRTPPacketBuffer.Last: TIdRTPPacket;
begin
  Result := TIdRTPPacket(Self.List[0]);
end;

procedure TIdRTPPacketBuffer.RemoveLast;
begin
  Self.List.Remove(Self.Last);
end;

//* TIdRTPPacketBuffer Private methods *****************************************

function TIdRTPPacketBuffer.AppropriateIndex(const Pkt: TIdRTPPacket): Integer;
begin
  Result := 0;
  while (Result < Self.List.Count)
    and (Self.PacketAt(Result).Timestamp < Pkt.Timestamp) do
    Inc(Result);
end;

procedure TIdRTPPacketBuffer.Clear;
begin
  Self.List.Clear;
end;

function TIdRTPPacketBuffer.PacketAt(const Index: Integer): TIdRTPPacket;
begin
  Result := TIdRTPPacket(Self.List[Index]);
end;

initialization
finalization
  GNullPayload.Free;
end.
