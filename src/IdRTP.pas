unit IdRTP;

interface

uses
  Classes, Contnrs, SysUtils, Types;

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
  protected
    function GetName: String; virtual;
  public
    class function CreateEncoding(Value: String): TIdRTPEncoding;
    class function NullEncoding: TIdRTPEncoding;

    constructor Create(Name: String;
                       ClockRate: Cardinal;
                       Parameters: String = ''); overload; virtual;
    constructor Create(Src: TIdRTPEncoding); overload; virtual;

    function AsString: String; virtual;
    function Clone: TIdRTPEncoding; virtual;
    function CreatePayload: TIdRTPPayload; virtual;
    function IsEqualTo(const OtherEncoding: TIdRTPEncoding): Boolean;
    function IsNull: Boolean; virtual;
    function IsReserved: Boolean; virtual;
    function PayloadType: TIdRTPPayloadClass; virtual;

    property ClockRate:  Cardinal read fClockRate;
    property Name:       String   read GetName;
    property Parameters: String   read fParameters;
  end;

  TIdRTPEncodingClass = class of TIdRTPEncoding;

  TIdT140Encoding = class(TIdRTPEncoding)
  public
    function PayloadType: TIdRTPPayloadClass; override;
  end;

  TIdTelephoneEventEncoding = class(TIdRTPEncoding)
  public
    function PayloadType: TIdRTPPayloadClass; override;
  end;

  // I represent the Null Encoding.
  TIdRTPNullEncoding = class(TIdRTPEncoding)
  public
    constructor Create(Name: String;
                       ClockRate: Cardinal;
                       Parameters: String = ''); overload; override;
    constructor Create(Src: TIdRTPEncoding); overload; override;

    function AsString: String; override;
    function Clone: TIdRTPEncoding; override;
    function IsNull: Boolean; override;
  end;

  // I am a Reserved or Unassigned encoding in an RTP profile. In other words, I
  // do nothing other than say to you "you may not use this payload type".
  TIdRTPReservedEncoding = class(TIdRTPEncoding)
  public
    constructor Create(Name: String;
                       ClockRate: Cardinal;
                       Parameters: String = ''); overload; override;
    constructor Create(Src: TIdRTPEncoding); overload; override;

    function AsString: String; override;
    function Clone: TIdRTPEncoding; override;
    function IsReserved: Boolean; override;
  end;

  // I represent the payload in an RTP packet. I store a reference to an
  // encoding. I strongly suggest that you don't mix-and-match payloads and
  // encodings. An RFC 2833 payload must work with an RFC 2833 encoding, for
  // instance.
  //
  // I offer a Flyweight Null Payload.
  TIdRTPPayload = class(TObject)
  private
    fEncoding: TIdRTPEncoding;
  public
    class function CreatePayload(Encoding: TIdRTPEncoding): TIdRTPPayload;
    class function CreateFrom(Encoding: TIdRTPEncoding;
                              Src: TStream): TIdRTPPayload;
    class function NullPayload: TIdRTPPayload;

    constructor Create(Encoding: TIdRTPEncoding);

    function  HasKnownLength: Boolean; virtual;
    function  IsNull: Boolean; virtual;
    function  Length: Cardinal; virtual;
    function  NumberOfSamples: Cardinal; virtual;
    procedure ReadFrom(Src: TStream); virtual;
    procedure PrintOn(Dest: TStream); virtual;

    property Encoding: TIdRTPEncoding read fEncoding;
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
    fBlock: String;
  public
    function  HasKnownLength: Boolean; override;
    function  Length: Cardinal; override;
    procedure ReadFrom(Src: TStream); override;
    procedure PrintOn(Dest: TStream); override;

    property Block: String read fBlock write fBlock;
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
    function  NumberOfSamples: Cardinal; override;
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
                                     PayloadType: TIdRTPPayloadType);

    procedure ReservePayloadType(const PayloadType: TIdRTPPayloadType);
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddEncoding(Encoding: TIdRTPEncoding;
                          PayloadType: TIdRTPPayloadType); overload;
    procedure AddEncoding(Name: String;
                          ClockRate: Cardinal;
                          Params: String;
                          PayloadType: TIdRTPPayloadType); overload;
    function  AllowsHeaderExtensions: Boolean; virtual;
    procedure Assign(Src: TPersistent); override;
    procedure Clear;
    function  Count: Integer;
    function  FirstFreePayloadType: TIdRTPPayloadType;
    function  IsFull: Boolean;
    function  HasEncoding(const Encoding: TIdRTPEncoding): Boolean;
    function  HasPayloadType(PayloadType: TIdRTPPayloadType): Boolean;
    function  EncodingFor(PayloadType: TIdRTPPayloadType): TIdRTPEncoding;
    function  PayloadTypeFor(Encoding: TIdRTPEncoding): TIdRTPPayloadType;
    function  TransportDesc: String; virtual;
  end;

  // I represent the profile defined in RFC 3551. As such, don't bother trying
  // to change the encodings of any reserved or assigned payload types. I only
  // allow the alteration of the dynamic payload types - 96-127.
  TIdAudioVisualProfile = class(TIdRTPProfile)
  private
    procedure ReserveRange(LowPT, HighPT: TIdRTPPayloadType);
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

  // Note that my Length property says how many 32-bit words (-1) I contain.
  // IT IS NOT AN OCTET-BASED LENGTH.
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

    function  IsRTCP: Boolean; virtual; abstract;
    function  IsRTP: Boolean; virtual; abstract;
    function  IsValid: Boolean; virtual; abstract;
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
    procedure ReadPayloadAndPadding(Src: TStream);
    procedure SetCsrcCount(const Value: TIdRTPCsrcCount);
    procedure SetCsrcID(Index: TIdRTPCsrcCount; const Value: Cardinal);
  public
    constructor Create(const Profile: TIdRTPProfile);
    destructor  Destroy; override;

    function  DefaultVersion: TIdRTPVersion;
    function  IsRTCP: Boolean; override;
    function  IsRTP: Boolean; override;
    function  IsValid: Boolean; override;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
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
    procedure AssertPacketType(const PT: Byte);
    function  GetPacketType: Cardinal; virtual; abstract;
  public
    class function RTCPType(const PacketType: Byte): TIdRTCPPacketClass;

    constructor Create; virtual;

    function IsBye: Boolean; virtual;
    function IsReceiverReport: Boolean; virtual;
    function IsSenderReport: Boolean; virtual;
    function IsRTCP: Boolean; override;
    function IsRTP: Boolean; override;
    function IsValid: Boolean; override;

    property PacketType: Cardinal read GetPacketType;
  end;

  TIdRTCPMultiSSRCPacket = class(TIdRTCPPacket)
  public
    function GetAllSrcIDs: TCardinalDynArray; virtual; abstract;
  end;

  TIdRTCPReceiverReport = class(TIdRTCPMultiSSRCPacket)
  private
    fExtension:        String;
    fReceptionReports: array of TIdRTCPReportBlock;

    procedure ClearReportBlocks;
    function  GetReports(Index: Integer): TIdRTCPReportBlock;
    function  GetReceptionReportCount: TIdRTCPReceptionCount;
    procedure ReInitialiseReportBlocks;
    procedure ReadAllReportBlocks(Src: TStream);
    function  ReportByteLength: Word;
    procedure SetReceptionReportCount(const Value: TIdRTCPReceptionCount);
  protected
    function  FixedHeaderByteLength: Word; virtual;
    function  GetPacketType: Cardinal; override;
    procedure PrintFixedHeadersOn(Dest: TStream); virtual;
    procedure ReadFixedHeadersFrom(Src: TStream); virtual;
  public
    function  GetAllSrcIDs: TCardinalDynArray; override;
    function IsReceiverReport: Boolean; override;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    function  RealLength: Word; override;

    property Extension:               String                read fExtension write fExtension;
    property ReceptionReportCount:    TIdRTCPReceptionCount read GetReceptionReportCount write SetReceptionReportCount;
    property Reports[Index: Integer]: TIdRTCPReportBlock    read GetReports;
  end;

  // I represent an SR RTCP packet. Please note that I clobber my report
  // objects when you change my ReceptionReportCount property - I free all
  // my existing reports and create new instances. I guarantee that you
  // won't get a nil pointer from ReportAt.
  // Active senders of data in an RTP session send me to give transmission
  // and reception statistics.
  TIdRTCPSenderReport = class(TIdRTCPReceiverReport)
  private
    fNTPTimestamp: TIdNTPTimestamp;
    fOctetCount:   Cardinal;
    fPacketCount:  Cardinal;
    fRTPTimestamp: Cardinal;

  protected
    function  FixedHeaderByteLength: Word; override;
    function  GetPacketType: Cardinal; override;
    procedure PrintFixedHeadersOn(Dest: TStream); override;
    procedure ReadFixedHeadersFrom(Src: TStream); override;
  public
    destructor Destroy; override;

    function IsReceiverReport: Boolean; override;
    function IsSenderReport: Boolean; override;

    property NTPTimestamp: TIdNTPTimestamp read fNTPTimestamp write fNTPTimestamp;
    property OctetCount:   Cardinal        read fOctetCount write fOctetCount;
    property PacketCount:  Cardinal        read fPacketCount write fPacketCount;
    property RTPTimestamp: Cardinal        read fRTPTimestamp write fRTPTimestamp;
  end;

  TIdSrcDescChunkItem = class;
  TIdSrcDescChunkItemClass = class of TIdSrcDescChunkItem;

  TIdSrcDescChunkItem = class(TObject)
  private
    fData: String;
  protected
    procedure SetData(const Value: String); virtual;
  public
    class function ItemType(ID: Byte): TIdSrcDescChunkItemClass;

    function  ID: Byte; virtual; abstract;
    function  Length: Byte;
    procedure PrintOn(Dest: TStream); virtual;
    procedure ReadFrom(Src: TStream); virtual;
    function  RealLength: Cardinal; virtual;

    property Data: String read fData write SetData;
  end;

  TIdSDESCanonicalName = class(TIdSrcDescChunkItem)
  public
    function ID: Byte; override;
  end;

  TIdSDESUserName = class(TIdSrcDescChunkItem)
  public
    function ID: Byte; override;
  end;

  TIdSDESEmail = class(TIdSrcDescChunkItem)
  public
    function ID: Byte; override;
  end;

  TIdSDESPhone = class(TIdSrcDescChunkItem)
  public
    function ID: Byte; override;
  end;

  TIdSDESLocation = class(TIdSrcDescChunkItem)
  public
    function ID: Byte; override;
  end;

  TIdSDESTool = class(TIdSrcDescChunkItem)
  public
    function ID: Byte; override;
  end;

  TIdSDESNote = class(TIdSrcDescChunkItem)
  public
    function ID: Byte; override;
  end;

  TIdSDESPriv = class(TIdSrcDescChunkItem)
  private
    fPrefix: String;

    function  MaxDataLength: Byte;
    function  MaxPrefixLength: Byte;
    procedure SetPrefix(const Value: String);
    procedure TruncateData;
  protected
    procedure SetData(const Value: String); override;
  public
    function  ID: Byte; override;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    function  RealLength: Cardinal; override;

    property Prefix: String read fPrefix write SetPrefix;
  end;

  TIdRTCPSrcDescChunk = class(TObject)
  private
    fSyncSrcID: Cardinal;
    ItemList:   TObjectList;

    function  AddCanonicalHeader: TIdSDESCanonicalName; overload;
    function  AddItem(ID: Byte): TIdSrcDescChunkItem;
    function  GetItems(Index: Integer): TIdSrcDescChunkItem;
    function  HasMoreItems(Src: TStream): Boolean;
    procedure PrintAlignmentPadding(Dest: TStream);
    procedure ReadAlignmentPadding(Src: TStream);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddCanonicalName(Name: String); overload;
    function  ItemCount: Integer;
    procedure PrintOn(Dest: TStream);
    procedure ReadFrom(Src: TStream);
    function  RealLength: Cardinal;

    property Items[Index: Integer]: TIdSrcDescChunkItem read GetItems;
    property SyncSrcID:             Cardinal            read fSyncSrcID write fSyncSrcID;
  end;

  TIdRTCPSourceDescription = class(TIdRTCPMultiSSRCPacket)
  private
    ChunkList: TObjectList;

    function  GetChunks(Index: Integer): TIdRTCPSrcDescChunk;
    procedure ReadChunk(Src: TStream);
  protected
    function  GetPacketType: Cardinal; override;
    function  GetSyncSrcID: Cardinal; override;
    procedure SetSyncSrcID(const Value: Cardinal); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  AddChunk: TIdRTCPSrcDescChunk;
    function  ChunkCount: TIdRTCPSourceCount;
    function  GetAllSrcIDs: TCardinalDynArray; override;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    function  RealLength: Word; override;

    property Chunks[Index: Integer]: TIdRTCPSrcDescChunk read GetChunks;
  end;

  // I represent an RTCP Bye packet. You use me to remove yourself from an RTP
  // session. RTP servers that receive me remove the SSRC that sent me from
  // their member tables.
  TIdRTCPBye = class(TIdRTCPMultiSSRCPacket)
  private
    fSources:      TIdCardinalArray;
    fReason:       String;
    fReasonLength: Word;

    function  GetSourceCount: TIdRTCPSourceCount;
    function  GetSource(Index: TIdRTCPSourceCount): Cardinal;
    procedure ReadReasonPadding(Src: TStream);
    procedure SetReason(const Value: String);
    procedure SetSource(Index: TIdRTCPSourceCount;
                        const Value: Cardinal);
    procedure SetSourceCount(const Value: TIdRTCPSourceCount);
    function  StreamHasReason: Boolean;
  protected
    function  GetPacketType: Cardinal; override;
    function  GetSyncSrcID: Cardinal; override;
    procedure SetSyncSrcID(const Value: Cardinal); override;
  public
    constructor Create; override;

    function  GetAllSrcIDs: TCardinalDynArray; override;
    function  IsBye: Boolean; override;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    function  RealLength: Word; override;

    property Reason:                             String             read fReason write SetReason;
    property ReasonLength:                       Word               read fReasonLength write fReasonLength;
    property SourceCount:                        TIdRTCPSourceCount read GetSourceCount write SetSourceCount;
    property Sources[Index: TIdRTCPSourceCount]: Cardinal           read GetSource write SetSource;
  end;

  TIdRTCPApplicationDefined = class(TIdRTCPPacket)
  private
    fData:    String;
    fName:    String;
    fSubType: TIdRTCPSubType;

    function  LengthOfName: Byte;
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

  TIdCompoundRTCPPacket = class(TIdRTCPPacket)
  private
    Packets: TObjectList;

    function Add(PacketType: TIdRTCPPacketClass): TIdRTCPPacket;
  protected
    function GetPacketType: Cardinal; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  AddApplicationDefined: TIdRTCPApplicationDefined;
    function  AddBye: TIdRTCPBye;
    function  AddReceiverReport: TIdRTCPReceiverReport;
    function  AddSenderReport: TIdRTCPSenderReport;
    function  AddSourceDescription: TIdRTCPSourceDescription;
    function  FirstPacket: TIdRTCPPacket;
    function  HasBye: Boolean;
    function  IsRTCP: Boolean; override;
    function  IsRTP: Boolean; override;
    function  IsValid: Boolean; override;
    function  PacketAt(Index: Cardinal): TIdRTCPPacket;
    function  PacketCount: Cardinal;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    function  RealLength: Word; override;
  end;

  // I provide a buffer to objects that receive RTP packets. I assemble these
  // packets, making sure I assemble the RTP stream in the correct order.
  TIdRTPPacketBuffer = class(TObject)
  private
    List: TObjectList;

    function  AppropriateIndex(Pkt: TIdRTPPacket): Integer;
    procedure Clear;
    function  PacketAt(Index: Integer): TIdRTPPacket;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Pkt: TIdRTPPacket);
    function  Last: TIdRTPPacket;
    procedure RemoveLast;
  end;

  ENoPayloadTypeFound = class(Exception);
  EStreamTooShort = class(Exception);
  EUnknownSDES = class(Exception);

function AddModulo(Addend, Augend: Cardinal; Radix: Cardinal): Cardinal;
function AddModuloWord(Addend, Augend: Word): Word;
function DateTimeToNTPFractionsOfASecond(DT: TDateTime): Cardinal;
function DateTimeToNTPSeconds(DT: TDateTime): Cardinal;
function DateTimeToNTPTimestamp(DT: TDateTime): TIdNTPTimestamp;
function EncodeAsString(Value: Cardinal): String; overload;
function EncodeAsString(Value: Word): String; overload;
function HtoNL(Value: Cardinal): Cardinal;
function HtoNS(Value: Word): Word;
function MultiplyCardinal(FirstValue, SecondValue: Cardinal): Cardinal;
function NowAsNTP: TIdNTPTimestamp;
function NtoHL(Value: Cardinal): Cardinal;
function NtoHS(Value: Word): Cardinal;

function  PeekByte(Src: TStream): Byte;
function  PeekWord(Src: TStream): Word;
function  ReadByte(Src: TStream): Byte;
function  ReadCardinal(Src: TStream): Cardinal;
procedure ReadNTPTimestamp(Src: TStream; var Timestamp: TIdNTPTimestamp);
function  ReadRemainderOfStream(Src: TStream): String;
function  ReadString(Src: TStream; Length: Cardinal): String;
function  ReadWord(Src: TStream): Word;
procedure WriteByte(Dest: TStream; Value: Byte);
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
  InterleavedT140ClockRate    = 8000;
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
  DateUtils, IdGlobal, IdRandom, IdRTPTimerQueue;

var
  GNullEncoding: TIdRTPEncoding;
  GNullPayload:  TIdRTPPayload;

const
  JanOne1900 = 2;

//******************************************************************************
//* Unit public functions & procedures                                         *
//******************************************************************************

function AddModulo(Addend, Augend: Cardinal; Radix: Cardinal): Cardinal;
begin
  Result := (Int64(Addend) + Augend) mod Radix
end;

function AddModuloWord(Addend, Augend: Word): Word;
begin
  Result := AddModulo(Addend, Augend, High(Addend));
end;

function DateTimeToNTPFractionsOfASecond(DT: TDateTime): Cardinal;
var
  Divisor:         Int64;
  Fraction:        Double;
  FractionBit:     Cardinal;
  PartOfOneSecond: Double;
begin
//  if (DT < 2) then
//    raise EConvertError.Create('DT < 1900/01/01');

  PartOfOneSecond := MilliSecondOfTheSecond(DT)/1000;
  Result          := 0;
  Divisor         := 2;
  FractionBit     := $80000000;
  while (Divisor <= $40000000) and (PartOfOneSecond > 0) do begin
    Fraction := 1/Divisor;
    if ((PartOfOneSecond - Fraction) >= 0) then begin
      Result := Result or FractionBit;
      PartOfOneSecond := PartOfOneSecond - Fraction;
    end;
    FractionBit := FractionBit div 2;
    Divisor := MultiplyCardinal(2, Divisor);
  end;
end;

function DateTimeToNTPSeconds(DT: TDateTime): Cardinal;
var
  Days: Cardinal;
begin
  if (DT < 2) then
    raise EConvertError.Create('DT < 1900/01/01');

  Days := Trunc(DT) - JanOne1900;

  Result := MultiplyCardinal(Days, SecsPerDay) + SecondOfTheDay(DT);
end;

// Caveat Programmer: TDateTime has the floating point nature. Don't expect
// enormous precision.
// TODO: Maybe we can find a platform-independent way of getting an accurate
// timestamp?
function DateTimeToNTPTimestamp(DT: TDateTime): TIdNTPTimestamp;
begin
  if (DT < 2) then
    raise EConvertError.Create('DT < 1900/01/01');

  Result.IntegerPart    := DateTimeToNTPSeconds(DT);
  Result.FractionalPart := DateTimeToNTPFractionsOfASecond(DT);
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

function NowAsNTP: TIdNTPTimestamp;
begin
  // TODO: This is ugly, but at least it's a bit more portable.
  Result := DateTimeToNTPTimestamp(SysUtils.Now);
end;

function NtoHL(Value: Cardinal): Cardinal;
begin
  Result := HtoNL(Value);
end;

function NtoHS(Value: Word): Cardinal;
begin
  Result := HtoNS(Value);
end;

function PeekByte(Src: TStream): Byte;
begin
  Result := 0;
  Src.Read(Result, SizeOf(Result));
  Src.Seek(-SizeOf(Result), soFromCurrent);
end;

function PeekWord(Src: TStream): Word;
begin
  Result := 0;
  try
    Result := ReadWord(Src);
  except
    on EStreamTooShort do;
  end;
  Src.Seek(-SizeOf(Result), soFromCurrent);
end;

function ReadByte(Src: TStream): Byte;
begin
  if (Src.Read(Result, SizeOf(Result)) < SizeOf(Result)) then
    raise EStreamTooShort.Create('ReadByte');
end;

function ReadCardinal(Src: TStream): Cardinal;
begin
  if (Src.Read(Result, SizeOf(Result)) < SizeOf(Result)) then
    raise EStreamTooShort.Create('ReadCardinal');

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

function ReadString(Src: TStream; Length: Cardinal): String;
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

  if (Total < Length) then
    raise EStreamTooShort.Create('ReadString');
end;

function ReadWord(Src: TStream): Word;
begin
  if (Src.Read(Result, SizeOf(Result)) < SizeOf(Result)) then
    raise EStreamTooShort.Create('ReadWord');

  Result := NtoHS(Result);
end;

procedure WriteByte(Dest: TStream; Value: Byte);
begin
  Dest.Write(Value, SizeOf(Value));
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
    Result := TIdT140Encoding.Create(Name,
                                        ClockRate,
                                        Parameters)
  else if (Lowercase(Name) = Lowercase(TelephoneEventEncoding)) then
    Result := TIdTelephoneEventEncoding.Create(Name,
                                                  ClockRate,
                                                  Parameters)
  else
    Result := TIdRTPEncoding.Create(Name,
                                    ClockRate,
                                    Parameters);
end;

class function TIdRTPEncoding.NullEncoding: TIdRTPEncoding;
begin
  if not Assigned(GNullEncoding) then
    GNullEncoding := TIdRTPNullEncoding.Create;

  Result := GNullEncoding;
end;

constructor TIdRTPEncoding.Create(Name: String;
                                  ClockRate: Cardinal;
                                  Parameters: String = '');
begin
  inherited Create;

  fClockRate  := ClockRate;
  fName       := Name;
  fParameters := Parameters;
end;

constructor TIdRTPEncoding.Create(Src: TIdRTPEncoding);
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
  Result := Self.PayloadType.Create(Self);
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

//* TIdRTPEncoding Private methods *********************************************

function TIdRTPEncoding.GetName: String;
begin
  Result := fName;
end;

//******************************************************************************
//* TIdT140Encoding                                                            *
//******************************************************************************
//* TIdT140Encoding Public methods *********************************************

function TIdT140Encoding.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdT140Payload;
end;

//******************************************************************************
//* TIdTelephoneEventEncoding                                                  *
//******************************************************************************
//* TIdTelephoneEventEncoding Public methods ***********************************

function TIdTelephoneEventEncoding.PayloadType: TIdRTPPayloadClass;
begin
  Result := TIdTelephoneEventPayload;
end;

//******************************************************************************
//* TIdRTPNullEncoding                                                         *
//******************************************************************************
//* TIdRTPNullEncoding Public methods ******************************************

constructor TIdRTPNullEncoding.Create(Name: String;
                                      ClockRate: Cardinal;
                                      Parameters: String = '');
begin
  inherited Create('', 0, '');
end;

constructor TIdRTPNullEncoding.Create(Src: TIdRTPEncoding);
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

constructor TIdRTPReservedEncoding.Create(Name: String;
                                          ClockRate: Cardinal;
                                          Parameters: String = '');
begin
  inherited Create('', 0, '');
end;

constructor TIdRTPReservedEncoding.Create(Src: TIdRTPEncoding);
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

class function TIdRTPPayload.CreatePayload(Encoding: TIdRTPEncoding): TIdRTPPayload;
var
  PayloadType: TIdRTPPayloadClass;
begin

  if Encoding.IsNull then
    PayloadType := TIdRawPayload
  else
    PayloadType := Encoding.PayloadType;

  Result := PayloadType.Create(Encoding);
end;

class function TIdRTPPayload.CreateFrom(Encoding: TIdRTPEncoding;
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
    GNullPayload := TIdNullPayload.Create(TIdRTPEncoding.NullEncoding);

  Result := GNullPayload;
end;

constructor TIdRTPPayload.Create(Encoding: TIdRTPEncoding);
begin
  inherited Create;

  fEncoding := Encoding;
end;

function TIdRTPPayload.HasKnownLength: Boolean;
begin
  Result := false;
end;

function TIdRTPPayload.IsNull: Boolean;
begin
  Result := false;
end;

function TIdRTPPayload.Length: Cardinal;
begin
  Result := 0;
end;

function TIdRTPPayload.NumberOfSamples: Cardinal;
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

function TIdT140Payload.HasKnownLength: Boolean;
begin
  Result := false;
end;

function TIdT140Payload.Length: Cardinal;
begin
  Result := System.Length(Self.Block);
end;

procedure TIdT140Payload.ReadFrom(Src: TStream);
begin
  Self.Block := ReadRemainderOfStream(Src);
end;

procedure TIdT140Payload.PrintOn(Dest: TStream);
begin
  if (Self.Block <> '') then
    Dest.Write(PChar(Self.Block)^, System.Length(Self.Block));
end;

//******************************************************************************
//* TIdTelephoneEventPayload                                                   *
//******************************************************************************
//* TIdTelephoneEventPayload Public methods ************************************

function TIdTelephoneEventPayload.NumberOfSamples: Cardinal;
begin
  Result := Self.Duration;
end;

procedure TIdTelephoneEventPayload.ReadFrom(Src: TStream);
var
  B: Byte;
begin
  Self.Event := ReadByte(Src);

  B := ReadByte(Src);
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

procedure TIdRTPProfile.AddEncoding(Encoding: TIdRTPEncoding;
                                    PayloadType: TIdRTPPayloadType);
begin
  if Encoding.IsNull then
    Self.RemoveEncoding(PayloadType)
  else
    Self.AddEncodingAsReference(Encoding.Clone, PayloadType);
end;

procedure TIdRTPProfile.AddEncoding(Name: String;
                                    ClockRate: Cardinal;
                                    Params: String;
                                    PayloadType: TIdRTPPayloadType);
var
  Enc: TIdRTPEncoding;
begin
  Enc := TIdRTPEncoding.Create(Name, ClockRate, Params);
  try
    Self.AddEncoding(Enc, PayloadType);
  finally
    Enc.Free;
  end;
end;

function TIdRTPProfile.AllowsHeaderExtensions: Boolean;
begin
  Result := true;
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

function TIdRTPProfile.HasPayloadType(PayloadType: TIdRTPPayloadType): Boolean;
begin
  Result := not Self.EncodingAt(PayloadType).IsNull;
end;

function TIdRTPProfile.EncodingFor(PayloadType: TIdRTPPayloadType): TIdRTPEncoding;
begin
  Result := Self.EncodingAt(PayloadType)
end;

function TIdRTPProfile.PayloadTypeFor(Encoding: TIdRTPEncoding): TIdRTPPayloadType;
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
                                               PayloadType: TIdRTPPayloadType);
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

procedure TIdAudioVisualProfile.ReserveRange(LowPT, HighPT: TIdRTPPayloadType);
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

  if Self.IsRTCPPayloadType(PacketType) then
    Result := TIdCompoundRTCPPacket.Create
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

  // RFC 3550, section 4: "Octets designated as padding have the value zero."
  PadLength := Self.Length - Self.RealLength;
  for I := 1 to PadLength - 1 do
    WriteByte(Dest, 0);

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

function TIdRTPPacket.IsValid: Boolean;
begin
  Result := (Self.Version = RFC3550Version)
         and Self.Profile.HasPayloadType(Self.PayloadType)
         and (Self.Profile.AllowsHeaderExtensions or not Self.HasExtension);
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
  Dest.Write(B, 1);

  B := Self.PayloadType;
  if Self.IsMarker then B := B or $80;
  Dest.Write(B, 1);

  WriteWord(Dest, Self.SequenceNo);

  WriteCardinal(Dest, Self.Timestamp);
  WriteCardinal(Dest, Self.SyncSrcID);

  for I := 0 to Self.CsrcCount - 1 do
    WriteCardinal(Dest, Self.CsrcIDs[I]);

  Self.Payload.PrintOn(Dest);

  if Self.HasPadding then
    Self.PrintPadding(Dest);
end;

procedure TIdRTPPacket.ReadFrom(Src: TStream);
var
  B: Byte;
  I: TIdRTPCsrcCount;
begin
  B := ReadByte(Src);
  Self.Version      := (B and $C0) shr 6;
  Self.HasPadding   := (B and $20) <> 0;
  Self.HasExtension := (B and $10) <> 0;
  Self.CsrcCount    :=  B and $0F;

  B := ReadByte(Src);
  Self.IsMarker    := (B and $80) <> 0;
  Self.PayloadType :=  B and $7F;

  Self.SequenceNo := ReadWord(Src);
  Self.Timestamp  := ReadCardinal(Src);
  Self.SyncSrcID  := ReadCardinal(Src);

  // Remember that the first Csrc = Self.SyncSrcID
  for I := 1 to Self.CsrcCount do
    Self.CsrcIDs[I - 1] := ReadCardinal(Src);

  if Self.HasExtension then
    Self.HeaderExtension.ReadFrom(Src);

  if Self.HasPadding then
    Self.ReadPayloadAndPadding(Src)
  else
    Self.ReadPayload(Src);
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
  // TODO: is this safe?
  if not Self.Payload.IsNull then
    fPayload.Free;

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

procedure TIdRTPPacket.ReadPayloadAndPadding(Src: TStream);
var
  CurrentPos:    Int64;
  Padding:       Byte;
  PayloadStream: TMemoryStream;
begin
  CurrentPos := Src.Position;
  Src.Seek(1, soFromEnd);
  Padding := ReadByte(Src);
  Src.Seek(CurrentPos, soFromBeginning);

  PayloadStream := TMemoryStream.Create;
  try
    PayloadStream.CopyFrom(Src, Src.Size - Src.Position - Padding);
    PayloadStream.Seek(0, soFromBeginning);
    Self.ReadPayload(PayloadStream);
  finally
    PayloadStream.Free;
  end;
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
    RTCPSenderReport:       Result := TIdRTCPSenderReport;
    RTCPReceiverReport:     Result := TIdRTCPReceiverReport;
    RTCPSourceDescription:  Result := TIdRTCPSourceDescription;
    RTCPGoodbye:            Result := TIdRTCPBye;
    RTCPApplicationDefined: Result := TIdRTCPApplicationDefined;
  else
    Result := nil;
  end;
end;

constructor TIdRTCPPacket.Create;
begin
  inherited Create;
end;

function TIdRTCPPacket.IsBye: Boolean;
begin
  Result := false;
end;

function TIdRTCPPacket.IsReceiverReport: Boolean;
begin
  Result := false;
end;

function TIdRTCPPacket.IsSenderReport: Boolean;
begin
  Result := false;
end;

function TIdRTCPPacket.IsRTCP: Boolean;
begin
  Result := true;
end;

function TIdRTCPPacket.IsRTP: Boolean;
begin
  Result := false;
end;

function TIdRTCPPacket.IsValid: Boolean;
begin
  Result := Self.Version = RFC3550Version;
end;

//* TIdRTCPPacket Protected methods ********************************************

procedure TIdRTCPPacket.AssertPacketType(const PT: Byte);
begin
  Assert(PT = Self.GetPacketType,
         Self.ClassName + ' packet type');
end;

//******************************************************************************
//* TIdRTCPReceiverReport                                                      *
//******************************************************************************
//* TIdRTCPReceiverReport Public methods ***************************************

function TIdRTCPReceiverReport.GetAllSrcIDs: TCardinalDynArray;
var
  I, J: Integer;
begin
  SetLength(Result, Self.ReceptionReportCount + 1);
  Result[Low(Result)] := Self.SyncSrcID;

  J := Low(Result) + 1;
  for I := 0 to Self.ReceptionReportCount - 1 do begin
    Result[J] := Self.Reports[I].SyncSrcID;
    Inc(J);
  end;
end;

function TIdRTCPReceiverReport.IsReceiverReport: Boolean;
begin
  Result := true;
end;

procedure TIdRTCPReceiverReport.PrintOn(Dest: TStream);
var
  I: Integer;
begin
  Self.PrintFixedHeadersOn(Dest);

  for I := 0 to Self.ReceptionReportCount - 1 do
    Self.Reports[I].PrintOn(Dest);

  if Self.HasPadding then
    Self.PrintPadding(Dest);
end;

procedure TIdRTCPReceiverReport.ReadFrom(Src: TStream);
begin
  Self.ReadFixedHeadersFrom(Src);
  Self.ReadAllReportBlocks(Src);
end;

function TIdRTCPReceiverReport.RealLength: Word;
begin
  Result := Self.FixedHeaderByteLength
          + Self.ReceptionReportCount * Self.ReportByteLength
          + Abs(System.Length(Self.Extension));
end;

//* TIdRTCPReceiverReport Protected methods ************************************

function TIdRTCPReceiverReport.FixedHeaderByteLength: Word;
begin
  Result := 2*4;
end;

function TIdRTCPReceiverReport.GetPacketType: Cardinal;
begin
  Result := RTCPReceiverReport;
end;

procedure TIdRTCPReceiverReport.PrintFixedHeadersOn(Dest: TStream);
var
  B: Byte;
begin
  B := Self.Version shl 6;
  if Self.HasPadding then B := B or $20;
  B := B or Self.ReceptionReportCount;
  Dest.Write(B, 1);

  WriteByte(Dest, Self.GetPacketType);

  WriteWord(Dest, Self.Length);
  WriteCardinal(Dest, Self.SyncSrcID);
end;

procedure TIdRTCPReceiverReport.ReadFixedHeadersFrom(Src: TStream);
var
  B: Byte;
begin
  B := ReadByte(Src);
  Self.Version              := B shr 6;
  Self.HasPadding           := (B and $20) > 0;
  Self.ReceptionReportCount := B and $1F;
  Self.AssertPacketType(ReadByte(Src));

  Self.Length    := ReadWord(Src);
  Self.SyncSrcID := ReadCardinal(Src);
end;

//* TIdRTCPReceiverReport Private methods **************************************

procedure TIdRTCPReceiverReport.ClearReportBlocks;
var
  I: Integer;
begin
  for I := Low(fReceptionReports) to High(fReceptionReports) do
    fReceptionReports[I].Free;
end;

function TIdRTCPReceiverReport.GetReports(Index: Integer): TIdRTCPReportBlock;
begin
  if (Index < 0) or (Index >= Self.ReceptionReportCount) then
    raise EListError.Create('List index out of bounds (' + IntToStr(Index) + ')');

  Result := fReceptionReports[Index];
end;

function TIdRTCPReceiverReport.GetReceptionReportCount: TIdRTCPReceptionCount;
begin
  Result := System.Length(fReceptionReports);
end;

procedure TIdRTCPReceiverReport.ReadAllReportBlocks(Src: TStream);
var
  I: Integer;
begin
  for I := Low(fReceptionReports) to High(fReceptionReports) do
    fReceptionReports[I].ReadFrom(Src);
end;

function TIdRTCPReceiverReport.ReportByteLength: Word;
begin
  Result := 6*4;
end;

procedure TIdRTCPReceiverReport.ReInitialiseReportBlocks;
var
  I: Integer;
begin
  for I := Low(fReceptionReports) to High(fReceptionReports) do
    fReceptionReports[I] := TIdRTCPReportBlock.Create;
end;

procedure TIdRTCPReceiverReport.SetReceptionReportCount(const Value: TIdRTCPReceptionCount);
begin
  Self.ClearReportBlocks;
  SetLength(fReceptionReports, Value);
  Self.ReInitialiseReportBlocks;
end;

//******************************************************************************
//* TIdRTCPSenderReport                                                        *
//******************************************************************************
//* TIdRTCPSenderReport Public methods *****************************************

destructor TIdRTCPSenderReport.Destroy;
begin
  Self.ClearReportBlocks;

  inherited Destroy;
end;

function TIdRTCPSenderReport.IsReceiverReport: Boolean;
begin
  // We inherit from ReceiverReport for code reuse ONLY. A Sender Report
  // IS NOT a Receiver Report. TODO: This indicates ugliness.
  // SR/RR relationship needs revisiting, possibly using delegation for
  // common behaviour
  Result := false;
end;

function TIdRTCPSenderReport.IsSenderReport: Boolean;
begin
  Result := true;
end;

//* TIdRTCPSenderReport Protected methods **************************************

function TIdRTCPSenderReport.FixedHeaderByteLength: Word;
begin
  Result := 7*4;
end;

function TIdRTCPSenderReport.GetPacketType: Cardinal;
begin
  Result := RTCPSenderReport;
end;

procedure TIdRTCPSenderReport.PrintFixedHeadersOn(Dest: TStream);
begin
  inherited PrintFixedHeadersOn(Dest);

  WriteNTPTimestamp(Dest, Self.NTPTimestamp);
  WriteCardinal(Dest, Self.RTPTimestamp);
  WriteCardinal(Dest, Self.PacketCount);
  WriteCardinal(Dest, Self.OctetCount);
end;

procedure TIdRTCPSenderReport.ReadFixedHeadersFrom(Src: TStream);
var
  T: TIdNTPTimestamp;
begin
  inherited ReadFixedHeadersFrom(Src);
  
  T.IntegerPart     := ReadCardinal(Src);
  T.FractionalPart  := ReadCardinal(Src);
  Self.NTPTimestamp := T;
  Self.RTPTimestamp := ReadCardinal(Src);
  Self.PacketCount  := ReadCardinal(Src);
  Self.OctetCount   := ReadCardinal(Src);
end;

//******************************************************************************
//* TIdSrcDescChunkItem                                                        *
//******************************************************************************
//* TIdSrcDescChunkItem Public methods *****************************************

class function TIdSrcDescChunkItem.ItemType(ID: Byte): TIdSrcDescChunkItemClass;
begin
  case ID of
    SDESCName: Result := TIdSDESCanonicalName;
    SDESName:  Result := TIdSDESUserName;
    SDESEmail: Result := TIdSDESEmail;
    SDESPhone: Result := TIdSDESPhone;
    SDESLoc:   Result := TIdSDESLocation;
    SDESTool:  Result := TIdSDESTool;
    SDESNote:  Result := TIdSDESNote;
    SDESPriv:  Result := TIdSDESPriv;
  else
    raise EUnknownSDES.Create('Unknown SDES type ' + IntToStr(ID));
  end;
end;

function TIdSrcDescChunkItem.Length: Byte;
begin
  Result := System.Length(Self.Data);
end;

procedure TIdSrcDescChunkItem.PrintOn(Dest: TStream);
begin
  WriteByte(Dest, Self.ID);
  WriteByte(Dest, Self.Length);
  Dest.Write(Self.Data[1], System.Length(Self.Data));
end;

procedure TIdSrcDescChunkItem.ReadFrom(Src: TStream);
var
  ID:  Byte;
  Len: Byte;
begin
  ID := ReadByte(Src);
  Assert(ID = Self.ID, Self.ClassName + ' SDES item ID');

  Len := ReadByte(Src);
  Self.Data := ReadString(Src, Len);
end;

function TIdSrcDescChunkItem.RealLength: Cardinal;
begin
  Result := 2 + System.Length(Self.Data);
end;

//* TIdSrcDescChunkItem Protected methods **************************************

procedure TIdSrcDescChunkItem.SetData(const Value: String);
begin
  fData := Copy(Value, 1, High(Self.Length));
end;

//******************************************************************************
//* TIdSDESCanonicalName                                                       *
//******************************************************************************
//* TIdSDESCanonicalName Public methods ****************************************

function TIdSDESCanonicalName.ID: Byte;
begin
  Result := SDESCName;
end;

//******************************************************************************
//* TIdSDESUserName                                                            *
//******************************************************************************
//* TIdSDESUserName Public methods *********************************************

function TIdSDESUserName.ID: Byte;
begin
  Result := SDESName;
end;

//******************************************************************************
//* TIdSDESEmail                                                               *
//******************************************************************************
//* TIdSDESEmail Public methods ************************************************

function TIdSDESEmail.ID: Byte;
begin
  Result := SDESEmail;
end;

//******************************************************************************
//* TIdSDESPhone                                                               *
//******************************************************************************
//* TIdSDESPhone Public methods ************************************************

function TIdSDESPhone.ID: Byte;
begin
  Result := SDESPhone;
end;

//******************************************************************************
//* TIdSDESLocation                                                            *
//******************************************************************************
//* TIdSDESLocation Public methods *********************************************

function TIdSDESLocation.ID: Byte;
begin
  Result := SDESLoc;
end;

//******************************************************************************
//* TIdSDESTool                                                                *
//******************************************************************************
//* TIdSDESTool Public methods *************************************************

function TIdSDESTool.ID: Byte;
begin
  Result := SDESTool;
end;

//******************************************************************************
//* TIdSDESNote                                                                *
//******************************************************************************
//* TIdSDESNote Public methods *************************************************

function TIdSDESNote.ID: Byte;
begin
  Result := SDESNote;
end;

//******************************************************************************
//* TIdSDESPriv                                                                *
//******************************************************************************
//* TIdSDESPriv Public methods *************************************************

function TIdSDESPriv.ID: Byte;
begin
  Result := SDESPriv;
end;

procedure TIdSDESPriv.PrintOn(Dest: TStream);
begin
  WriteByte(Dest, Self.ID);
  WriteByte(Dest, Self.Length + System.Length(Self.Prefix) + 1);
  WriteByte(Dest, System.Length(Self.Prefix));

  if (Self.Prefix <> '') then
    Dest.Write(Self.Prefix[1], System.Length(Self.Prefix));

  if (Self.Data <> '') then
    Dest.Write(Self.Data[1],   System.Length(Self.Data));
end;

procedure TIdSDESPriv.ReadFrom(Src: TStream);
var
  PrefixLen: Byte;
begin
  inherited ReadFrom(Src);

  if (Self.Data = '') then
    raise EStreamTooShort.Create('Missing prefix length');

  PrefixLen := Ord(Self.Data[1]);
  Self.Data := Copy(Self.Data, 2, System.Length(Self.Data));
  Self.Prefix := Copy(Self.Data, 1, PrefixLen);
  Self.Data := Copy(Self.Data, PrefixLen + 1, System.Length(Self.Data));
end;

function TIdSDESPriv.RealLength: Cardinal;
begin
  Result := 3 + System.Length(Self.Prefix) + System.Length(Self.Data);
end;

//* TIdSDESPriv Protected methods **********************************************

procedure TIdSDESPriv.SetData(const Value: String);
begin
  inherited SetData(Copy(Value, 1, Self.MaxDataLength));
end;

//* TIdSDESPriv Private methods ************************************************

function TIdSDESPriv.MaxDataLength: Byte;
begin
  Result := Self.MaxPrefixLength - System.Length(Self.Prefix);
end;

function TIdSDESPriv.MaxPrefixLength: Byte;
begin
  Result := High(Byte) - 1;
end;

procedure TIdSDESPriv.SetPrefix(const Value: String);
begin
  fPrefix := Copy(Value, 1, Self.MaxPrefixLength);

  Self.TruncateData;
end;

procedure TIdSDESPriv.TruncateData;
begin
  if (Self.Data <> '') then
    Self.Data := Self.Data;
end;

//******************************************************************************
//* TIdRTCPSrcDescChunk                                                        *
//******************************************************************************
//* TIdRTCPSrcDescChunk Public methods *****************************************

constructor TIdRTCPSrcDescChunk.Create;
begin
  inherited Create;

  Self.ItemList := TObjectList.Create(true);
end;

destructor TIdRTCPSrcDescChunk.Destroy;
begin
  Self.ItemList.Free;

  inherited Destroy;
end;

procedure TIdRTCPSrcDescChunk.AddCanonicalName(Name: String);
begin
  Self.AddCanonicalHeader.Data := Name;
end;

function TIdRTCPSrcDescChunk.ItemCount: Integer;
begin
  Result := Self.ItemList.Count;
end;

procedure TIdRTCPSrcDescChunk.PrintOn(Dest: TStream);
var
  I: Integer;
begin
  WriteCardinal(Dest, Self.SyncSrcID);

  for I := 0 to Self.ItemCount - 1 do
    Self.Items[I].PrintOn(Dest);

  Self.PrintAlignmentPadding(Dest);
end;

procedure TIdRTCPSrcDescChunk.ReadFrom(Src: TStream);
var
  ID: Byte;
begin
  Self.SyncSrcID := ReadCardinal(Src);

  while Self.HasMoreItems(Src) do begin
    ID := PeekByte(Src);
    if (ID <> SDESEnd) then
      Self.AddItem(ID).ReadFrom(Src);
  end;

  Self.ReadAlignmentPadding(Src);
end;

function TIdRTCPSrcDescChunk.RealLength: Cardinal;
var
  I: Integer;
begin
  Result := 4;

  for I := 0 to Self.ItemCount - 1 do
    Result := Result + Self.Items[I].RealLength;
end;

//* TIdRTCPSrcDescChunk Private methods ****************************************

function TIdRTCPSrcDescChunk.AddCanonicalHeader: TIdSDESCanonicalName;
begin
  Result := Self.AddItem(SDESCName) as TIdSDESCanonicalName;
end;

function TIdRTCPSrcDescChunk.AddItem(ID: Byte): TIdSrcDescChunkItem;
begin
  Result := TIdSrcDescChunkItem.ItemType(ID).Create;
  try
    Self.ItemList.Add(Result);
  except
    if (Self.ItemList.IndexOf(Result) <> -1) then
      Self.ItemList.Remove(Result)
    else
      FreeAndNil(Result);

    raise;
  end;
end;

function TIdRTCPSrcDescChunk.GetItems(Index: Integer): TIdSrcDescChunkItem;
begin
  Result := Self.ItemList[Index] as TIdSrcDescChunkItem;
end;

function TIdRTCPSrcDescChunk.HasMoreItems(Src: TStream): Boolean;
begin
  Result := PeekByte(Src) <> 0;
end;

procedure TIdRTCPSrcDescChunk.PrintAlignmentPadding(Dest: TStream);
var
  I: Integer;
begin
  if (Self.RealLength mod 4 > 0) then
    for I := 1 to 4 - (Self.RealLength mod 4) do
      WriteByte(Dest, 0);
end;

procedure TIdRTCPSrcDescChunk.ReadAlignmentPadding(Src: TStream);
var
  I: Integer;
begin
  if (Self.RealLength mod 4 <> 0) then begin
    for I := 1 to 4 - (Self.RealLength mod 4) do
      ReadByte(Src);
  end;
end;

//******************************************************************************
//* TIdRTCPSourceDescription                                                   *
//******************************************************************************
//* TIdRTCPSourceDescription Public methods ************************************

constructor TIdRTCPSourceDescription.Create;
begin
  inherited Create;

  Self.ChunkList := TObjectList.Create(true);
end;

destructor TIdRTCPSourceDescription.Destroy;
begin
  Self.ChunkList.Free;

  inherited Destroy;
end;

function TIdRTCPSourceDescription.AddChunk: TIdRTCPSrcDescChunk;
begin
  if (Self.ChunkCount = High(Self.ChunkCount)) then begin
    Result := nil;
    Exit;
  end;

  Result := TIdRTCPSrcDescChunk.Create;
  try
    Self.ChunkList.Add(Result);
  except
    if (Self.ChunkList.IndexOf(Result) <> -1) then
      Self.ChunkList.Remove(Result)
    else
      FreeAndNil(Result);

    raise;
  end;
end;

function TIdRTCPSourceDescription.ChunkCount: TIdRTCPSourceCount;
begin
  Result := Self.ChunkList.Count;
end;

function TIdRTCPSourceDescription.GetAllSrcIDs: TCardinalDynArray;
var
  I, J: Integer;
begin
  SetLength(Result, Self.ChunkCount);

  J := Low(Result);
  for I := 0 to Self.ChunkCount - 1 do begin
    Result[J] := Self.Chunks[I].SyncSrcID;
    Inc(J);
  end;
end;

procedure TIdRTCPSourceDescription.PrintOn(Dest: TStream);
var
  B: Byte;
  I: Integer;
begin
  B := Self.Version shl 6;
  if Self.HasPadding then
    B := B or $20;
  B := B or Self.ChunkCount;
  Dest.Write(B, 1);

  WriteByte(Dest, Self.PacketType);

  WriteWord(Dest, Self.Length);

  for I := 0 to Self.ChunkCount - 1 do
    Self.Chunks[I].PrintOn(Dest);

  if Self.HasPadding then
    Self.PrintPadding(Dest);
end;

procedure TIdRTCPSourceDescription.ReadFrom(Src: TStream);
var
  B:         Byte;
  I:         Integer;
  NumChunks: TIdFiveBitInt;
begin
  B := ReadByte(Src);
  Self.Version    := (B and $C0) shr 6;
  Self.HasPadding := (B and $20) <> 0;
  NumChunks := B and $1F;

  Self.AssertPacketType(ReadByte(Src));
  Self.Length := ReadWord(Src);

  for I := 1 to NumChunks do
    Self.ReadChunk(Src);
end;

function TIdRTCPSourceDescription.RealLength: Word;
var
  I: Integer;
begin
  Result := 4;
  for I := 0 to Self.ChunkCount - 1 do
    Result := Result + Self.Chunks[I].RealLength;
end;

//* TIdRTCPSourceDescription Protected methods *********************************

function TIdRTCPSourceDescription.GetPacketType: Cardinal;
begin
  Result := RTCPSourceDescription;
end;

function TIdRTCPSourceDescription.GetSyncSrcID: Cardinal;
begin
  if (Self.ChunkCount = 0) then
    Result := 0
  else
    Result := Self.Chunks[0].SyncSrcID;
end;

procedure TIdRTCPSourceDescription.SetSyncSrcID(const Value: Cardinal);
begin
  if (Self.ChunkCount = 0) then
    Self.AddChunk;

  Self.Chunks[0].SyncSrcID := Value;
end;

//* TIdRTCPSourceDescription Private methods ***********************************

function TIdRTCPSourceDescription.GetChunks(Index: Integer): TIdRTCPSrcDescChunk;
begin
  Result := Self.ChunkList[Index] as TIdRTCPSrcDescChunk;
end;

procedure TIdRTCPSourceDescription.ReadChunk(Src: TStream);
begin
  Self.AddChunk.ReadFrom(Src);
end;

//******************************************************************************
//* TIdRTCPBye                                                                 *
//******************************************************************************
//* TIdRTCPBye Public methods **************************************************

constructor TIdRTCPBye.Create;
begin
  inherited Create;

  Self.SourceCount := 1;
end;

procedure TIdRTCPBye.PrintOn(Dest: TStream);
var
  B: Byte;
  I: Integer;
begin
  B := Self.Version shl 6;
  if Self.HasPadding then B := B or $20;
  Dest.Write(B, 1);

  WriteByte(Dest, Self.GetPacketType);

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

function TIdRTCPBye.GetAllSrcIDs: TCardinalDynArray;
var
  I, J: Integer;
begin
  SetLength(Result, Self.SourceCount);

  J := Low(Result);
  for I := 0 to Self.SourceCount - 1 do begin
    Result[J] := Self.Sources[I];
    Inc(J);
  end;
end;

function TIdRTCPBye.IsBye: Boolean;
begin
  Result := true;
end;

procedure TIdRTCPBye.ReadFrom(Src: TStream);
var
  B: Byte;
  I: Integer;
begin
  B := ReadByte(Src);
  Self.Version    := B and $C0 shr 6;
  Self.HasPadding := (B and $20) <> 0;

  Self.SourceCount := B and $1F;

  Self.AssertPacketType(ReadByte(Src));

  Self.Length := ReadWord(Src);

  for I := 0 to Self.SourceCount - 1 do
    Self.Sources[I] := ReadCardinal(Src);

  if Self.StreamHasReason then begin
    Self.ReasonLength := ReadByte(Src);

    Self.Reason := ReadString(Src, Self.ReasonLength);
    Self.ReadReasonPadding(Src);
  end;
end;

function TIdRTCPBye.RealLength: Word;
begin
  Result := 4 + Self.SourceCount*4;

  if (Self.Reason <> '') then
    Result := Result + SizeOf(Self.ReasonLength) + System.Length(Self.Reason);
end;

//* TIdRTCPBye Protected methods ***********************************************

function TIdRTCPBye.GetPacketType: Cardinal;
begin
  Result := RTCPGoodbye;
end;

function TIdRTCPBye.GetSyncSrcID: Cardinal;
begin
  Result := Self.Sources[0];
end;

procedure TIdRTCPBye.SetSyncSrcID(const Value: Cardinal);
begin
  Self.Sources[0] := Value;
end;

//* TIdRTCPBye Private methods *************************************************

function TIdRTCPBye.GetSourceCount: TIdRTCPSourceCount;
begin
  Result := System.Length(fSources);
end;

function TIdRTCPBye.GetSource(Index: TIdRTCPSourceCount): Cardinal;
begin
  Result := fSources[Index];
end;

procedure TIdRTCPBye.ReadReasonPadding(Src: TStream);
var
  Mod4Length: Byte;
begin
  // Self.ReasonLength consumes 1 byte; ergo, to align the Reason on a 32-bit
  // word boundary requires that you pad on one less than ReasonLength.
  Mod4Length := (Self.ReasonLength + 1) mod 4;

  if (Mod4Length <> 0) then
    ReadString(Src, 4 - Mod4Length);
end;

procedure TIdRTCPBye.SetReason(const Value: String);
begin
  fReason := Value;
  Self.ReasonLength := System.Length(Value);
end;

procedure TIdRTCPBye.SetSource(Index: TIdRTCPSourceCount;
                                     const Value: Cardinal);
begin
  Self.SourceCount := Max(Self.SourceCount, Index + 1);
  fSources[Index] := Value;
end;

procedure TIdRTCPBye.SetSourceCount(const Value: TIdRTCPSourceCount);
begin
  SetLength(fSources, Value);
end;

function TIdRTCPBye.StreamHasReason: Boolean;
begin
  // Hackish. Length is the length of this RTCP packet in 32-bit words minus
  // one. SourceCount tells us how many SSRCS this packet contains, and SSRCs
  // are 32-bit words. Bye packets have one 32-bit word other than the SSRCs.
  // Therefore if Self.Length > Self.SourceCount we must have a Reason field.
  Result := Self.Length > Self.SourceCount;
end;

//******************************************************************************
//* TIdRTCPApplicationDefined                                                  *
//******************************************************************************
//* TIdRTCPApplicationDefined Public methods ***********************************

constructor TIdRTCPApplicationDefined.Create;
begin
  inherited Create;

  Self.Name := #0#0#0#0;
  Self.Length := 2;
end;

procedure TIdRTCPApplicationDefined.PrintOn(Dest: TStream);
var
  B: Byte;
  I: Integer;
begin
  B := Self.Version shl 6;
  if Self.HasPadding then B := B or $20;
  Dest.Write(B, 1);

  WriteByte(Dest, Self.GetPacketType);

  WriteWord(Dest, Self.Length);

  WriteCardinal(Dest, Self.SyncSrcID);

  Dest.Write(Self.Name[1], System.Length(Self.Name));
  for I := 1 to 4 - System.Length(Name) do
    WriteByte(Dest, 0);

  if (Self.Data <> '') then begin
    Dest.Write(Self.Data[1], System.Length(Self.Data));
  end;

  if Self.HasPadding then
    Self.PrintPadding(Dest);
end;

procedure TIdRTCPApplicationDefined.ReadFrom(Src: TStream);
const
  // The size of the set headers of an Application-Defined RTCP packet
  // IN 32-BIT WORDS - cf. RFC 3550, section 6.7
  DataOffset = 3;
var
  B:    Byte;
  Name: array[0..3] of Char;
begin
  B := ReadByte(Src);
  Self.Version    := B and $C0 shr 6;
  Self.HasPadding := (B and $20) <> 0;

  Self.AssertPacketType(ReadByte(Src));

  Self.Length := ReadWord(Src);
  Self.SyncSrcID := ReadCardinal(Src);

  Src.Read(Name, System.Length(Name));
  Self.Name := Name;

  if (Self.Length > DataOffset) then
    Self.Data := ReadString(Src, (Self.Length - DataOffset + 1)*4);
end;

function TIdRTCPApplicationDefined.RealLength: Word;
begin
  Result := 8
            + Self.LengthOfName
            + System.Length(Self.Data);
end;

//* TIdRTCPApplicationDefined Protected methods ********************************

function TIdRTCPApplicationDefined.GetPacketType: Cardinal;
begin
  Result := RTCPApplicationDefined;
end;

//* TIdRTCPApplicationDefined Private methods **********************************

function TIdRTCPApplicationDefined.LengthOfName: Byte;
begin
  Result := 4;
end;

procedure TIdRTCPApplicationDefined.SetData(const Value: String);
var
  Len: Integer;
begin
  fData := Value;

  Len := System.Length(fData);

  if (Len mod 4 <> 0) then
    while System.Length(fData) < 4*((Len div 4) + 1) do
      fData := fData + #0;
end;

procedure TIdRTCPApplicationDefined.SetName(const Value: String);
begin
  if (System.Length(Value) > 4) then
    fName := Copy(Value, 1, 4)
  else
    fName := Value;
end;

//******************************************************************************
//* TIdCompoundRTCPPacket                                                      *
//******************************************************************************
//* TIdCompoundRTCPPacket Public methods ***************************************

constructor TIdCompoundRTCPPacket.Create;
begin
  inherited Create;

  Self.Packets := TObjectList.Create(true);
end;

destructor TIdCompoundRTCPPacket.Destroy;
begin
  Self.Packets.Free;

  inherited Destroy;
end;

function TIdCompoundRTCPPacket.AddApplicationDefined: TIdRTCPApplicationDefined;
begin
  Result := Self.Add(TIdRTCPApplicationDefined) as TIdRTCPApplicationDefined;
end;

function TIdCompoundRTCPPacket.AddBye: TIdRTCPBye;
begin
  Result := Self.Add(TIdRTCPBye) as TIdRTCPBye;
end;

function TIdCompoundRTCPPacket.AddReceiverReport: TIdRTCPReceiverReport;
begin
  Result := Self.Add(TIdRTCPReceiverReport) as TIdRTCPReceiverReport;
end;

function TIdCompoundRTCPPacket.AddSenderReport: TIdRTCPSenderReport;
begin
  Result := Self.Add(TIdRTCPSenderReport) as TIdRTCPSenderReport;
end;

function TIdCompoundRTCPPacket.AddSourceDescription: TIdRTCPSourceDescription;
begin
  Result := Self.Add(TIdRTCPSourceDescription) as TIdRTCPSourceDescription;
end;

function TIdCompoundRTCPPacket.FirstPacket: TIdRTCPPacket;
begin
  if (Self.PacketCount > 0) then
    Result := Self.PacketAt(0)
  else
    Result := nil;
end;

function TIdCompoundRTCPPacket.HasBye: Boolean;
var
  I: Cardinal;
begin
  Result := false;
  
  I := 0;
  while (I < Self.PacketCount) and not Result do begin
    if Self.PacketAt(I).IsBye then
      Result := true;
    Inc(I);
  end;
end;

function TIdCompoundRTCPPacket.IsRTCP: Boolean;
begin
  Result := true;
end;

function TIdCompoundRTCPPacket.IsRTP: Boolean;
begin
  Result := false;
end;

function TIdCompoundRTCPPacket.IsValid: Boolean;
begin
  Result := inherited IsValid
        and (Self.PacketCount > 0)
        and (Self.FirstPacket.IsSenderReport or Self.FirstPacket.IsReceiverReport)
        and not Self.FirstPacket.HasPadding;
end;

function TIdCompoundRTCPPacket.PacketAt(Index: Cardinal): TIdRTCPPacket;
begin
  Result := Self.Packets[Index] as TIdRTCPPacket;
end;

function TIdCompoundRTCPPacket.PacketCount: Cardinal;
begin
  Result := Self.Packets.Count;
end;

procedure TIdCompoundRTCPPacket.PrintOn(Dest: TStream);
var
  I: Integer;
begin
  if (Self.PacketCount > 0) then
    for I := 0 to Self.PacketCount - 1 do
      Self.PacketAt(I).PrintOn(Dest);
end;

procedure TIdCompoundRTCPPacket.ReadFrom(Src: TStream);
var
  Peek: Word;
begin
  Peek := PeekWord(Src);
  while (Peek <> 0) do begin
    Self.Add(TIdRTCPPacket.RTCPType(Peek and $00ff)).ReadFrom(Src);
    Peek := PeekWord(Src);
  end;
end;

function TIdCompoundRTCPPacket.RealLength: Word;
begin
  Result := Self.Length * SizeOf(Cardinal);
end;

//* TIdCompoundRTCPPacket Protected methods ************************************

function TIdCompoundRTCPPacket.GetPacketType: Cardinal;
begin
  if (Self.PacketCount > 0) then
    Result := Self.PacketAt(0).PacketType
  else
    Result := 0;
end;

//* TIdCompoundRTCPPacket Private methods **************************************

function TIdCompoundRTCPPacket.Add(PacketType: TIdRTCPPacketClass): TIdRTCPPacket;
begin
  Result := PacketType.Create;
  try
    Self.Packets.Add(Result)
  except
    if (Self.Packets.IndexOf(Result) <> -1) then
      Self.Packets.Remove(Result)
    else
      Result.Free;
    raise;
  end;
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

procedure TIdRTPPacketBuffer.Add(Pkt: TIdRTPPacket);
begin
  Self.List.Insert(Self.AppropriateIndex(Pkt), Pkt);
end;

function TIdRTPPacketBuffer.Last: TIdRTPPacket;
begin
  Result := Self.PacketAt(0);
end;

procedure TIdRTPPacketBuffer.RemoveLast;
begin
  Self.List.Remove(Self.Last);
end;

//* TIdRTPPacketBuffer Private methods *****************************************

function TIdRTPPacketBuffer.AppropriateIndex(Pkt: TIdRTPPacket): Integer;
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

function TIdRTPPacketBuffer.PacketAt(Index: Integer): TIdRTPPacket;
begin
  Result := Self.List[Index] as TIdRTPPacket;
end;

initialization
finalization
  GNullEncoding.Free;
  GNullPayload.Free;
end.
