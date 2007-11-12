{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdRTP;

interface

uses
  Classes, Contnrs, IdConnectionBindings, IdInterfacedObject, IdNotification,
  IdTimerQueue, SyncObjs, SysUtils, Types;

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
  TIdRTPPayloadType       = Byte;
  TIdRTPSequenceNo        = Word;
  TIdRTPTimestamp         = Cardinal;
  TIdRTPVersion           = 0..3;
  TIdT140BlockCount       = Word;

  TIdRTPPayload = class;
  TIdRTPPayloadClass = class of TIdRTPPayload;

  // I represent the payload in an RTP packet. I store a reference to an
  // encoding. I strongly suggest that you don't mix-and-match payloads and
  // encodings. An RFC 2833 payload must work with an RFC 2833 encoding, for
  // instance.
  //
  // I offer a Flyweight Null Payload.
  TIdRTPPayload = class(TPersistent)
  private
    fClockRate:    Cardinal;
    fParameters:   String;
    fSamplingRate: Cardinal;
    fStartTime:    TDateTime;
  protected
    function  DefaultSamplingRate: Cardinal; virtual;
    function  GetName: String; virtual;
    function  GetStartTime: TDateTime; virtual;
    procedure SetStartTime(Value: TDateTime); virtual;
  public
    class function CreateFrom(Payload: TIdRTPPayload;
                              Src: TStream): TIdRTPPayload;
    class function CreatePayload(Name: String): TIdRTPPayload;
    class function CreateNullPayload: TIdRTPPayload;
    class function EncodingName(const Name: String;
                                ClockRate: Cardinal;
                                const Parameters: String = ''): String; overload;

    constructor Create; overload; virtual;

    procedure Assign(Src: TPersistent); override;
    function  Copy: TIdRTPPayload;
    function  EncodingName: String; overload;
    function  HasKnownLength: Boolean; virtual;
    function  HasSameEncoding(Other: TIdRTPPayload): Boolean;
    function  IsNull: Boolean; virtual;
    function  IsReserved: Boolean; virtual;
    function  Length: Cardinal; virtual;
    function  NumberOfSamples: Cardinal; virtual;
    procedure ReadFrom(Src: TStream); virtual;
    procedure PrintOn(Dest: TStream); virtual;

    property ClockRate:    Cardinal  read fClockRate write fClockRate;
    property Name:         String    read GetName;
    property Parameters:   String    read fParameters write fParameters;
    property SamplingRate: Cardinal  read fSamplingRate write fSamplingRate;
    property StartTime:    TDateTime read GetStartTime write SetStartTime;
  end;

  // I represent the Null payload - a Null Object representing the absence of a
  // payload.
  TIdNullPayload = class(TIdRTPPayload)
  protected
    function  GetName: String; override;
    function  GetStartTime: TDateTime; override;
    procedure SetStartTime(Value: TDateTime); override;
  public
    function IsNull: Boolean; override;
  end;

  // I am a Reserved or Unassigned encoding in an RTP profile. In other words, I
  // do nothing other than say to you "you may not use this payload type".
  TIdRTPReservedPayload = class(TIdRTPPayload)
  protected
    function  GetName: String; override;
    function  GetStartTime: TDateTime; override;
    procedure SetStartTime(Value: TDateTime); override;
  public
    function IsReserved: Boolean; override;
  end;

  // I represent a raw (i.e., unparsed) payload.
  // I typically provide a fallback case for a malconfigured RTP server.
  // I slurp up the entire contents of a source stream, because there's not
  // much else I can do - I don't know my own length (or at least, I can't
  // derive my expected length from a source stream).
  TIdRTPRawPayload = class(TIdRTPPayload)
  private
    fData: String;
    fName: String;
  protected
    function GetName: String; override;
  public
    constructor Create; overload; override;

    function  Length: Cardinal; override;
    procedure ReadFrom(Src: TStream); override;
    procedure PrintOn(Dest: TStream); override;
    procedure SetName(const Value: String);

    property Data: String read fData write fData;
  end;

  // I am a T.140 payload, as defined in RFC 4103 (which obsoletes RFC 2793).
  // Note that while I provide Unicode (i.e., UCS-2) access to my data, I read
  // to and write from the network in UTF-8, as per RFC 4103.
  TIdRTPT140Payload = class(TIdRTPPayload)
  private
    fBlock: WideString;
  protected
    function GetName: String; override;
  public
    constructor Create; override;

    function  HasKnownLength: Boolean; override;
    function  Length: Cardinal; override;
    procedure ReadFrom(Src: TStream); override;
    procedure PrintOn(Dest: TStream); override;

    property Block: WideString read fBlock write fBlock;
  end;

  // I represent DTMF signals and such, as defined in RFC 2833
  TIdRTPTelephoneEventPayload = class(TIdRTPPayload)
  private
    fDuration:    Word;
    fEvent:       Byte;
    fIsEnd:       Boolean;
    fReservedBit: Boolean;
    fVolume:      TIdTelephoneEventVolume;
  protected
    function GetName: String; override;
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

  TIdRTPPCMMuLawPayload = class(TIdRTPRawPayload)
  protected
    function DefaultSamplingRate: Cardinal; override;
  end;

  TIdPayloadArray = array[Low(TIdRTPPayloadType)..High(TIdRTPPayloadType)] of TIdRTPPayload;

  TIdRTPBasePacket = class;

  // I represent a 1-1 association map between encodings and RTP Payload Types.
  // RTP packets use me to determine how their payload should be interpreted.
  // Because I represent a  1-1 relation, you cannot add the same encoding to
  // me twice. If you try, the payload type you try to overwrite will remain
  // unchanged.
  TIdRTPProfile = class(TPersistent)
  private
    Encodings:        TIdPayloadArray;
    NullEncoding:     TIdRTPPayload;
    ReservedEncoding: TIdRTPPayload;

    function  IndexOfEncoding(Encoding: TIdRTPPayload): Integer; overload;
    function  IndexOfEncoding(const EncodingName: String): Integer; overload;
    procedure Initialize;
    procedure RemoveEncoding(PayloadType: TIdRTPPayloadType);
  protected
    procedure AddEncodingAsReference(Encoding: TIdRTPPayload;
                                     PayloadType: TIdRTPPayloadType);

    procedure ReservePayloadType(PayloadType: TIdRTPPayloadType);
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure AddEncoding(Encoding: TIdRTPPayload;
                          PayloadType: TIdRTPPayloadType); overload;
    procedure AddEncoding(Name: String;
                          ClockRate: Cardinal;
                          Params: String;
                          PayloadType: TIdRTPPayloadType); overload;
    function  AllowsHeaderExtensions: Boolean; virtual;
    procedure Assign(Src: TPersistent); override;
    procedure Clear;
    function  Count: Integer;
    function  CreatePacket(Src: TStream): TIdRTPBasePacket;
    function  EncodingFor(PayloadType: TIdRTPPayloadType): TIdRTPPayload; overload;
    function  EncodingFor(EncodingName: String): TIdRTPPayload; overload;
    function  FirstFreePayloadType: TIdRTPPayloadType;
    function  HasEncoding(Encoding: TIdRTPPayload): Boolean;
    function  HasPayloadType(PayloadType: TIdRTPPayloadType): Boolean;
    function  IsFull: Boolean;
    function  IsRTCPPayloadType(PayloadType: Byte): Boolean;
    function  PayloadTypeFor(Encoding: TIdRTPPayload): TIdRTPPayloadType; overload;
    function  PayloadTypeFor(const EncodingName: String): TIdRTPPayloadType; overload;
    function  StreamContainsEncoding(Src: TStream): TIdRTPPayload;
    function  StreamContainsPayloadType(Src: TStream): TIdRTPPayloadType;
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
    procedure SetData(Index: Word;
                      Value: Cardinal);
    procedure SetLength(Value: Word);
  public
    function  OctetCount: Cardinal;
    procedure ReadFrom(Src: TStream);
    procedure PrintOn(Dest: TStream);

    property Length:              Word     read GetLength write SetLength;
    property ProfileDefinedValue: Word     read fProfileDefinedValue write fProfileDefinedValue;
    property Data[Index: Word]:   Cardinal read GetData write SetData;
  end;

  TIdRTPMember = class;

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
    procedure GatherStatistics(Member: TIdRTPMember);
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

  TIdRTPSession = class;

  // Note that my Length property says how many 32-bit words (-1) I contain.
  // IT DOES NOT COUNT OCTETS.
  // We make Length a read/write property because if you want to pad a packet,
  // you can specify the length explicitly.
  TIdRTPBasePacket = class(TPersistent)
  private
    fHasPadding:        Boolean;
    fLength:            Word;
    fSyncSrcID:         Cardinal;
    fVersion:           TIdRTPVersion;
    LengthSetManually:  Boolean;

    function  GetLength: Word;
    procedure SetLength(const Value: Word);
  protected
    function  GetSyncSrcID: Cardinal; virtual;
    procedure PrintPadding(Dest: TStream);
    procedure SetSyncSrcID(Value: Cardinal); virtual;
  public
    constructor Create;

    procedure Assign(Src: TPersistent); override;
    function  Copy: TIdRTPBasePacket; virtual;
    function  IsRTCP: Boolean; virtual;
    function  IsRTP: Boolean; virtual;
    function  IsValid: Boolean; virtual;
    procedure PrepareForTransmission(Session: TIdRTPSession); virtual;
    procedure PrintOn(Dest: TStream); virtual;
    procedure ReadFrom(Src: TStream); virtual;
    function  RealLength: Word; virtual;

    property HasPadding: Boolean       read fHasPadding write fHasPadding;
    property Length:     Word          read GetLength write SetLength;
    property SyncSrcID:  Cardinal      read GetSyncSrcID write SetSyncSrcID;
    property Version:    TIdRTPVersion read fVersion write fVersion;
  end;

  // I represent a packet of the Real-time Transport Protocol.
  // Before you use the CsrcIDs property make sure you set the CsrcCount
  // property!
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

    function  DefaultVersion: TIdRTPVersion;
    function  GetCsrcCount: TIdRTPCsrcCount;
    function  GetCsrcID(Index: TIdRTPCsrcCount): Cardinal;
    procedure ReadPayloadAndPadding(Src: TStream);
    procedure SetCsrcCount(Value: TIdRTPCsrcCount);
    procedure SetCsrcID(Index: TIdRTPCsrcCount;
                        Value: Cardinal);
    procedure SetPayload(Value: TIdRTPPayload);
  public
    constructor Create(Profile: TIdRTPProfile);
    destructor  Destroy; override;

    function  Copy: TIdRTPBasePacket; override;
    function  CollidesWith(SSRC: Cardinal): Boolean;
    function  GetAllSrcIDs: TCardinalDynArray;
    function  IsRTP: Boolean; override;
    function  IsValid: Boolean; override;
    procedure PrepareForTransmission(Session: TIdRTPSession); override;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    procedure ReadPayload(Src: TStream;
                          Profile: TIdRTPProfile); overload;
    procedure ReadPayload(Src: String;
                          Profile: TIdRTPProfile); overload;
    procedure ReadPayload(Data: TIdRTPPayload); overload;
    function  RealLength: Word; override;

    property CsrcCount:                       TIdRTPCsrcCount       read GetCsrcCount write SetCsrcCount;
    property CsrcIDs[Index: TIdRTPCsrcCount]: Cardinal              read GetCsrcID write SetCsrcID;
    property HasExtension:                    Boolean               read fHasExtension write fHasExtension;
    property HeaderExtension:                 TIdRTPHeaderExtension read fHeaderExtension;
    property IsMarker:                        Boolean               read fIsMarker write fIsMarker;
    property Payload:                         TIdRTPPayload         read fPayload write SetPayload;
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
    procedure AssertPacketType(PT: Byte);
    function  GetPacketType: Byte; virtual;
  public
    class function RTCPType(PacketType: Byte): TIdRTCPPacketClass;

    constructor Create; virtual;

    function Copy: TIdRTPBasePacket; override;
    function IsBye: Boolean; virtual;
    function IsReceiverReport: Boolean; virtual;
    function IsRTCP: Boolean; override;
    function IsSenderReport: Boolean; virtual;
    function IsSourceDescription: Boolean; virtual;
    function IsValid: Boolean; override;

    property PacketType: Byte read GetPacketType;
  end;

  TIdRTCPMultiSSRCPacket = class(TIdRTCPPacket)
  public
    function GetAllSrcIDs: TCardinalDynArray; virtual;
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
    procedure SetReceptionReportCount(Value: TIdRTCPReceptionCount);
  protected
    function  FixedHeaderByteLength: Word; virtual;
    function  GetPacketType: Byte; override;
    procedure PrintFixedHeadersOn(Dest: TStream); virtual;
    procedure ReadFixedHeadersFrom(Src: TStream); virtual;
  public
    function  GetAllSrcIDs: TCardinalDynArray; override;
    function  IsReceiverReport: Boolean; override;
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
    function  GetPacketType: Byte; override;
    procedure PrintFixedHeadersOn(Dest: TStream); override;
    procedure ReadFixedHeadersFrom(Src: TStream); override;
  public
    destructor Destroy; override;

    function  IsReceiverReport: Boolean; override;
    function  IsSenderReport: Boolean; override;
    procedure PrepareForTransmission(Session: TIdRTPSession); override;

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

    function  ID: Byte; virtual;
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
    function  TooManyChunks: Boolean;
  protected
    function  GetPacketType: Byte; override;
    function  GetSyncSrcID: Cardinal; override;
    procedure SetSyncSrcID(Value: Cardinal); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  AddChunk: TIdRTCPSrcDescChunk;
    function  ChunkCount: TIdRTCPSourceCount;
    function  GetAllSrcIDs: TCardinalDynArray; override;
    function  IsSourceDescription: Boolean; override;
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
    fReasonLength: Byte;

    function  GetSourceCount: TIdRTCPSourceCount;
    function  GetSource(Index: TIdRTCPSourceCount): Cardinal;
    procedure PrintReason(Dest: TStream);
    procedure ReadReasonPadding(Src: TStream);
    procedure SetReason(const Value: String);
    procedure SetSource(Index: TIdRTCPSourceCount;
                        Value: Cardinal);
    procedure SetSourceCount(Value: TIdRTCPSourceCount);
    function  StreamHasReason: Boolean;
  protected
    function  GetPacketType: Byte; override;
    function  GetSyncSrcID: Cardinal; override;
    procedure SetSyncSrcID(Value: Cardinal); override;
  public
    constructor Create; override;

    function  GetAllSrcIDs: TCardinalDynArray; override;
    function  IsBye: Boolean; override;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    function  RealLength: Word; override;

    property Reason:                             String             read fReason write SetReason;
    property ReasonLength:                       Byte               read fReasonLength write fReasonLength;
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
    function GetPacketType: Byte; override;
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
    function GetPacketType: Byte; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function  AddApplicationDefined: TIdRTCPApplicationDefined;
    function  AddBye: TIdRTCPBye;
    function  AddReceiverReport: TIdRTCPReceiverReport;
    function  AddSenderReport: TIdRTCPSenderReport;
    function  AddSourceDescription: TIdRTCPSourceDescription;
    procedure Assign(Src: TPersistent); override;
    function  Copy: TIdRTPBasePacket; override;
    function  FirstPacket: TIdRTCPPacket;
    function  HasBye: Boolean;
    function  HasReceiverReport: Boolean;
    function  HasSourceDescription: Boolean;
    function  IsRTCP: Boolean; override;
    function  IsRTP: Boolean; override;
    function  IsValid: Boolean; override;
    function  PacketAt(Index: Cardinal): TIdRTCPPacket;
    function  PacketCount: Cardinal;
    procedure PrepareForTransmission(Session: TIdRTPSession); override;
    procedure PrintOn(Dest: TStream); override;
    procedure ReadFrom(Src: TStream); override;
    function  RealLength: Word; override;
  end;

  // I represent a member in an RTP session.
  // I keep track of Quality of Service statistics and source/control addresses.
  // Too, I provide authentication - a session doesn't regard a packet source as
  // a member until a certain minimum number of packets arrive with sequential
  // sequence numbers (for certain values of "sequential". I store this minimum
  // number in the parameter MinimumSequentialPackets. MaxMisOrder and MaxDropout
  // provide control over what "sequential" means - MaxDropout determines an upper
  // bound on old sequence numbers, and MaxMisOrder an upper bound on sequence
  // number jumps. 
  TIdRTPMember = class(TObject)
  private
    fBadSeqNo:                    Cardinal;
    fBaseSeqNo:                   Word;
    fCanonicalName:               String;
    fControlAddress:              String;
    fControlPort:                 Cardinal;
    fCycles:                      Cardinal; // The (shifted) count of sequence number wraparounds
    fExpectedPrior:               Cardinal; // The previously calculated number of expected received packets
    fHasLeftSession:              Boolean;
    fHasSyncSrcID:                Boolean;
    fHighestSeqNo:                Word;
    fIsSender:                    Boolean;
    fJitter:                      Cardinal;
    fLastRTCPReceiptTime:         TDateTime;
    fLastRTPReceiptTime:          TDateTime;
    fLastSenderReportReceiptTime: TDateTime;
    fLocalAddress:                Boolean;
    fMaxDropout:                  Word;
    fMaxMisOrder:                 Word;
    fMinimumSequentialPackets:    Word;
    fPreviousPacketTransit:       Int64; // Transit time of previous packet in clock rate ticks
//    fPreValidatedData:            TIdRTPpac
    fProbation:                   Cardinal; // The number of packets still to be received before this member's considered valid
    fReceivedPackets:             Cardinal; // The number of received packets as of right now
    fReceivedPrior:               Cardinal; // The number of received packets in the last UpdateStatistics
    fSentControl:                 Boolean; // Have we ever sent RTCP packets?
    fSentData:                    Boolean; // Have we ever sent RTP packets?
    fSourceAddress:               String;
    fSourcePort:                  Cardinal;
    fSyncSrcID:                   Cardinal;

    function  DefaultMaxDropout: Word;
    function  DefaultMaxMisOrder: Word;
    function  DefaultMinimumSequentialPackets: Word;
    function  ExpectedPacketCount: Cardinal;
    procedure UpdateJitter(Data: TIdRTPPacket; CurrentTime: Cardinal);
    procedure UpdatePrior;
    function  UpdateSequenceNo(Data: TIdRTPPacket): Boolean;
  public
    constructor Create;

    function  DelaySinceLastSenderReport: Cardinal;
    procedure InitSequence(Data: TIdRTPPacket);
    function  IsInSequence(Data: TIdRTPPacket): Boolean;
    function  IsUnderProbation: Boolean;
    function  LastSenderReport: Cardinal;
    procedure MarkAsSender(SSRC: Cardinal);
    function  PacketLossCount: Cardinal;
    function  PacketLossFraction: Byte;
    function  SequenceNumberRange: Cardinal;
    procedure SetControlBinding(Binding: TIdConnectionBindings);
    procedure SetDataBinding(Binding: TIdConnectionBindings);
    function  UpdateStatistics(Data:  TIdRTPPacket;
                               CurrentTime: TIdRTPTimestamp): Boolean; overload;
    procedure UpdateStatistics(Stats: TIdRTCPPacket); overload;

    property CanonicalName:               String    read fCanonicalName write fCanonicalName;
    property ControlAddress:              String    read fControlAddress write fControlAddress;
    property ControlPort:                 Cardinal  read fControlPort write fControlPort;
    property HasLeftSession:              Boolean   read fHasLeftSession write fHasLeftSession;
    property HasSyncSrcID:                Boolean   read fHasSyncSrcID write fHasSyncSrcID;
    property IsSender:                    Boolean   read fIsSender write fIsSender;
    property LocalAddress:                Boolean   read fLocalAddress write fLocalAddress;
    property LastRTCPReceiptTime:         TDateTime read fLastRTCPReceiptTime write fLastRTCPReceiptTime;
    property LastRTPReceiptTime:          TDateTime read fLastRTPReceiptTime write fLastRTPReceiptTime;
    property LastSenderReportReceiptTime: TDateTime read fLastSenderReportReceiptTime write fLastSenderReportReceiptTime;
    property SentControl:                 Boolean   read fSentControl write fSentControl;
    property SentData:                    Boolean   read fSentData write fSentData;
    property SourceAddress:               String    read fSourceAddress write fSourceAddress;
    property SourcePort:                  Cardinal  read fSourcePort write fSourcePort;
    property SyncSrcID:                   Cardinal  read fSyncSrcID write fSyncSrcID;

    // Sequence number validity, bytecounts, etc
    property HighestSeqNo:          Word     read fHighestSeqNo write fHighestSeqNo;
    property Cycles:                Cardinal read fCycles write fCycles;
    property BaseSeqNo:             Word     read fBaseSeqNo write fBaseSeqNo;
    property BadSeqNo:              Cardinal read fBadSeqNo write fBadSeqNo;
    property PreviousPacketTransit: Int64    read fPreviousPacketTransit write fPreviousPacketTransit;
    property Probation:             Cardinal read fProbation write fProbation;
    property ReceivedPackets:       Cardinal read fReceivedPackets write fReceivedPackets;
    property ExpectedPrior:         Cardinal read fExpectedPrior write fExpectedPrior;
    property ReceivedPrior:         Cardinal read fReceivedPrior write fReceivedPrior;
    property Jitter:                Cardinal read fJitter write fJitter;

    // Parameters for handling sequence validity
    property MaxDropout:               Word read fMaxDropout write fMaxDropout;
    property MinimumSequentialPackets: Word read fMinimumSequentialPackets write fMinimumSequentialPackets;
    property MaxMisOrder:              Word read fMaxMisOrder write fMaxMisOrder;
  end;

  IIdAbstractRTPPeer = interface;

  TIdRTPMemberTable = class(TObject)
  private
    function CompensationFactor: Double;
    function RandomTimeFactor: Double;
  protected
    List: TObjectList;
  public
    constructor Create;
    destructor  Destroy; override;

    function  Add(SSRC: Cardinal): TIdRTPMember;
    function  AddReceiver(Host: String; Port: Cardinal): TIdRTPMember;
    function  AddSender(SSRC: Cardinal): TIdRTPMember;
    procedure AdjustTransmissionTime(PreviousMemberCount: Cardinal;
                                     var NextTransmissionTime: TDateTime;
                                     var PreviousTransmissionTime: TDateTime);
    function  Contains(SSRC: Cardinal): Boolean;
    function  ContainsReceiver(Host: String; Port: Cardinal): Boolean;
    function  Count: Cardinal;
    function  DeterministicSendInterval(ForSender: Boolean;
                                        Session: TIdRTPSession): TDateTime;
    function  Find(SSRC: Cardinal): TIdRTPMember;
    function  FindReceiver(Host: String; Port: Cardinal): TIdRTPMember;
    function  MemberAt(Index: Cardinal): TIdRTPMember;
    function  MemberTimeout(Session: TIdRTPSession): TDateTime;
    function  ReceiverCount: Cardinal;
    procedure Remove(SSRC: Cardinal); overload;
    procedure Remove(Member: TIdRTPMember); overload;
    procedure RemoveAll;
    procedure RemoveSources(Bye: TIdRTCPBye);
    procedure RemoveTimedOutMembersExceptFor(CutoffTime: TDateTime;
                                             SessionSSRC: Cardinal);
    procedure RemoveTimedOutSendersExceptFor(CutoffTime: TDateTime;
                                             SessionSSRC: Cardinal);
    procedure SendControl(Packet: TIdRTCPPacket;
                          Agent: IIdAbstractRTPPeer);
    procedure SendData(Packet: TIdRTPPacket;
                       Agent: IIdAbstractRTPPeer);
    function  SenderCount: Cardinal;
    function  SenderTimeout(Session: TIdRTPSession): TDateTime;
    function  SendInterval(Session: TIdRTPSession): TDateTime;
    procedure SetControlBinding(SSRC: Cardinal;
                                Binding: TIdConnectionBindings);
    procedure SetControlBindings(SSRCs: TCardinalDynArray;
                                 Binding: TIdConnectionBindings);
    function  SetDataBinding(SSRC: Cardinal; Binding: TIdConnectionBindings): TIdRTPMember;
    procedure UpdateStatistics(CurrentRTPTime: Cardinal;
                               RTP: TIdRTPPacket;
                               Binding: TIdConnectionBindings);

    property Members[Index: Cardinal]: TIdRTPMember read MemberAt;
  end;

  // I am a filter on a TIdRTPMemberTable. Using me allows you to iterate
  // through all the senders in a member table (i.e., members with IsSender
  // set to true) while ignoring the non-senders.
  TIdRTPSenderTable = class(TObject)
  private
    Members: TIdRTPMemberTable;
  public
    constructor Create(MemberTable: TIdRTPMemberTable);

    function  Add(SSRC: Cardinal): TIdRTPMember;
    function  Contains(SSRC: Cardinal): Boolean;
    function  Count: Cardinal;
    function  Find(SSRC: Cardinal): TIdRTPMember;
    function  SenderAt(Index: Cardinal): TIdRTPMember;
    procedure Remove(SSRC: Cardinal); overload;
    procedure Remove(Sender: TIdRTPMember); overload;
    procedure RemoveAll;
  end;

  // I provide a protocol for things that listen for RTP data, that is, for
  // things that have no interest in the infrastructure of RTP, but just the
  // data.
  //
  // Implicitly we assume that the bits you connect together all use the same
  // profile.
  IIdRTPDataListener = interface
    ['{B378CDAA-1B15-4BE9-8C41-D7B90DEAD654}']
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnectionBindings);
  end;

  // I provide a protocol for things that listen for RTP packets. RTP agents,
  // monitors, etc., can use me to keep track of an RTP session.
  IIdRTPListener = interface
    ['{2B5E040A-0B8E-4C44-9769-80E3AAE35C41}']
    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnectionBindings);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnectionBindings);
  end;

  // I provide a protocol for notifying of sent data/control packets.
  IIdRTPSendListener = interface
    ['{228672D3-EC22-4A59-978A-9ED26A6DBCAB}']
    procedure OnSendRTCP(Packet: TIdRTCPPacket;
                         Binding: TIdConnectionBindings);
    procedure OnSendRTP(Packet: TIdRTPPacket;
                        Binding: TIdConnectionBindings);
  end;

  // I provide a protocol for sending RTP/RTCP data as well as notifying
  // reception of same.
  IIdAbstractRTPPeer = interface
    ['{10608EEF-CE5B-44AE-9B22-34CC19D0BE07}']
    procedure AddListener(const Listener: IIdRTPListener);
    procedure NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                    Binding: TIdConnectionBindings);
    procedure NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                   Binding: TIdConnectionBindings);
    procedure RemoveListener(const Listener: IIdRTPListener);
    procedure SendPacket(const Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket);
  end;

  TIdBaseRTPAbstractPeer = class(TIdInterfacedObject,
                                 IIdAbstractRTPPeer)
  private
    fLocalProfile:  TIdRTPProfile;
    fRemoteProfile: TIdRTPProfile;
    fSession:       TIdRTPSession;
    Listeners:      TIdNotificationList;
    SendListeners:  TIdNotificationList;

    procedure NotifyListenersOfSingleSentRTCP(Packet: TIdRTCPPacket;
                                              Binding: TIdConnectionBindings);
    procedure NotifyListenersOfSingleRTCP(Packet: TIdRTCPPacket;
                                          Binding: TIdConnectionBindings);
  protected
    function  GetActive: Boolean; virtual;
    function  GetAddress: String; virtual;
    function  GetDefaultPort: Integer; virtual;
    function  GetRTCPPort: Cardinal; virtual;
    function  GetRTPPort: Cardinal; virtual;
    function  GetSession: TIdRTPSession; virtual;
    function  GetTimer: TIdTimerQueue; virtual;
    procedure NotifyListenersOfSentRTCP(Packet: TIdRTCPPacket;
                                        Binding: TIdConnectionBindings); virtual;
    procedure NotifyListenersOfSentRTP(Packet: TIdRTPPacket;
                                       Binding: TIdConnectionBindings); virtual;
    procedure NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                    Binding: TIdConnectionBindings); virtual;
    procedure NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                   Binding: TIdConnectionBindings); virtual;
    procedure SetActive(Value: Boolean); virtual;
    procedure SetAddress(Value: String); virtual;
    procedure SetDefaultPort(Value: Integer); virtual;
    procedure SetLocalProfile(Value: TIdRTPProfile); virtual;
    procedure SetRemoteProfile(Value: TIdRTPProfile); virtual;
    procedure SetRTCPPort(Value: Cardinal); virtual;
    procedure SetRTPPort(Value: Cardinal); virtual;
    procedure SetTimer(Value: TIdTimerQueue); virtual;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddListener(const Listener: IIdRTPListener);
    procedure AddSendListener(const Listener: IIdRTPSendListener);
    procedure ReceivePacket(Packet: TIdRTPBasePacket;
                            Binding: TIdConnectionBindings); virtual;
    procedure RemoveListener(const Listener: IIdRTPListener);
    procedure RemoveSendListener(const Listener: IIdRTPSendListener);
    procedure SendPacket(const Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket); virtual;

    property Active:        Boolean          read GetActive write SetActive;
    property Address:       String           read GetAddress write SetAddress;
    property DefaultPort:   Integer          read GetDefaultPort write SetDefaultPort;
    property LocalProfile:  TIdRTPProfile    read fLocalProfile write SetLocalProfile;
    property RemoteProfile: TIdRTPProfile    read fRemoteProfile write SetRemoteProfile;
    property RTCPPort:      Cardinal         read GetRTCPPort write SetRTCPPort;
    property RTPPort:       Cardinal         read GetRTPPort write SetRTPPort;
    property Session:       TIdRTPSession    read GetSession;
    property Timer:         TIdTimerQueue    read GetTimer write SetTimer;
  end;

  // I provide a way for tests to reference RTPPeers without exposing the peers
  // directly in owning classes. For instance, TIdSDPMediaStream uses RTPPeers,
  // but we don't want normal code to reference those peers except through
  // TIdSDPMediaStream's methods, but tests must ALSO access the peers in order
  // to, for instance, simulate the receipt of a packet.
  TIdRTPPeerRegistry = class(TObject)
  private
    class function GetAllServers: TStrings;
  public
    class function FindServer(const ServerID: String): TIdBaseRTPAbstractPeer;
    class function ServerOn(Host: String; Port: Cardinal): TIdBaseRTPAbstractPeer;
    class function ServerRunningOn(Host: String; Port: Cardinal): Boolean;
  end;

  TIdBaseRTPAbstractPeerClass = class of TIdBaseRTPAbstractPeer;

  // I provide a self-contained SSRC space.
  // All values involving time represent milliseconds / ticks.
  //
  // current responsibilities:
  // * Keep track of members/senders
  // * Keep track of timing stuff
  // * Keep track of session state
  TIdRTPSession = class(TIdInterfacedObject,
                        IIdRTPListener)
  private
    Agent:                      IIdAbstractRTPPeer;
    BaseTime:                   TDateTime;
    BaseTimestamp:              Cardinal; // in clock rate ticks
    fSyncSrcID:                 Cardinal;
    fAssumedMTU:                Cardinal;
    fAvgRTCPSize:               Cardinal;
    fCanonicalName:             String;
    fLocalProfile:              TIdRTPProfile;
    fNoControlSent:             Boolean;
    fMaxRTCPBandwidth:          Cardinal; // octets per second
    fMinimumRTCPSendInterval:   TDateTime; // in seconds
    fPreviousMemberCount:       Cardinal; // member count at last transmission time
    fReceiverBandwidthFraction: Double;
    fRemoteProfile:             TIdRTPProfile;
    fMissedReportTolerance:     Cardinal;
    fSenderBandwidthFraction:   Double;
    fSentOctetCount:            Cardinal;
    fSentPacketCount:           Cardinal;
    fSessionBandwidth:          Cardinal;
    fTimer:                     TIdTimerQueue;
    Listeners:                  TIdNotificationList;
    MemberLock:                 TCriticalSection;
    Members:                    TIdRTPMemberTable;
    NextTransmissionTime:       TDateTime;
    NoDataSent:                 Boolean;
    PreviousTransmissionTime:   TDateTime;
    SequenceNo:                 TIdRTPSequenceNo;
    TransmissionLock:           TCriticalSection;

    function  AddAppropriateReportTo(Packet: TIdCompoundRTCPPacket): TIdRTCPReceiverReport;
    procedure AddControlSource(ID: Cardinal; Binding: TIdConnectionBindings);
    procedure AddControlSources(RTCP: TIdRTCPMultiSSRCPacket;
                                Binding: TIdConnectionBindings);
    procedure AddReports(Packet: TIdCompoundRTCPPacket);
    procedure AddSourceDesc(Packet: TIdCompoundRTCPPacket);
    procedure AdjustAvgRTCPSize(Control: TIdRTCPPacket);
    procedure AdjustTransmissionTime(Members: TIdRTPMemberTable);
    function  DefaultAssumedMTU: Cardinal;
    function  DefaultMinimumRTCPSendInterval: TDateTime;
    function  DefaultMissedReportTolerance: Cardinal;
    function  DefaultNoControlSentAvgRTCPSize: Cardinal;
    function  DefaultReceiverBandwidthFraction: Double;
    function  DefaultSenderBandwidthFraction: Double;
    procedure IncSentOctetCount(N: Cardinal);
    procedure IncSentPacketCount;
    procedure NotifyListenersOfData(Data: TIdRTPPayload;
                                    Binding: TIdConnectionBindings);
    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnectionBindings);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnectionBindings);
    procedure RemoveSources(Bye: TIdRTCPBye);
    procedure ResetSentOctetCount;
    procedure ResetSentPacketCount;
    procedure ScheduleReport(Milliseconds: Cardinal);
    procedure SetSyncSrcId(Value: Cardinal);
  public
    constructor Create(Agent: IIdAbstractRTPPeer);
    destructor  Destroy; override;

    function  AcceptableSSRC(SSRC: Cardinal): Boolean;
    procedure AddListener(const Listener: IIdRTPDataListener);
    function  AddMember(SSRC: Cardinal): TIdRTPMember;
    function  AddReceiver(Host: String; Port: Cardinal): TIdRTPMember;
    function  AddSender(SSRC: Cardinal): TIdRTPMember;
    function  CreateNextReport: TIdCompoundRTCPPacket;
    function  DeterministicSendInterval(ForSender: Boolean;
                                        Table: TIdRTPMemberTable): TDateTime;
    procedure Initialize;
    function  IsMember(SSRC: Cardinal): Boolean; overload;
    function  IsMember(const Host: String;
                       Port: Cardinal): Boolean; overload;
    function  IsSender: Boolean; overload;
    function  IsSender(SSRC: Cardinal): Boolean; overload;
    procedure JoinSession;
    procedure LeaveSession(const Reason: String = '');
    function  LockMembers: TIdRTPMemberTable;
    function  Member(SSRC: Cardinal): TIdRTPMember; overload;
    function  Member(const Host: String;
                     Port: Cardinal): TIdRTPMember; overload;
    function  MemberCount: Cardinal;
    function  NewSSRC: Cardinal;
    function  NextSequenceNo: TIdRTPSequenceNo;
    function  NothingSent: Boolean;
    procedure ReceiveControl(RTCP: TIdRTCPPacket;
                             Binding: TIdConnectionBindings);
    procedure ReceiveData(RTP: TIdRTPPacket;
                          Binding: TIdConnectionBindings);
    function  ReceiverCount: Cardinal;
    procedure RemoveListener(const Listener: IIdRTPDataListener);
    procedure RemoveTimedOutMembers;
    procedure RemoveTimedOutSenders;
    procedure ResolveSSRCCollision;
    procedure SendControl(Packet: TIdRTCPPacket);
    procedure SendData(Data: TIdRTPPayload);
    function  Sender(SSRC: Cardinal): TIdRTPMember;
    function  SenderCount: Cardinal;
    procedure SendReport;
    function  TimeOffsetFromStart(WallclockTime: TDateTime): TDateTime;
    procedure TransmissionTimeExpire;
    procedure UnlockMembers;

    property AssumedMTU:                Cardinal      read fAssumedMTU write fAssumedMTU;
    property AvgRTCPSize:               Cardinal      read fAvgRTCPSize;
    property CanonicalName:             String        read fCanonicalName write fCanonicalName;
    property LocalProfile:              TIdRTPProfile read fLocalProfile write fLocalProfile;
    property NoControlSent:             Boolean       read fNoControlSent;
    property MaxRTCPBandwidth:          Cardinal      read fMaxRTCPBandwidth write fMaxRTCPBandwidth;
    property MinimumRTCPSendInterval:   TDateTime     read fMinimumRTCPSendInterval write fMinimumRTCPSendInterval;
    property PreviousMemberCount:       Cardinal      read fPreviousMemberCount;
    property MissedReportTolerance:     Cardinal      read fMissedReportTolerance write fMissedReportTolerance;
    property ReceiverBandwidthFraction: Double        read fReceiverBandwidthFraction write fReceiverBandwidthFraction;
    property RemoteProfile:             TIdRTPProfile read fRemoteProfile write fRemoteProfile;
    property SenderBandwidthFraction:   Double        read fSenderBandwidthFraction write fSenderBandwidthFraction;
    property SentOctetCount:            Cardinal      read fSentOctetCount;
    property SentPacketCount:           Cardinal      read fSentPacketCount;
    property SessionBandwith:           Cardinal      read fSessionBandwidth write fSessionBandwidth;
    property SyncSrcID:                 Cardinal      read fSyncSrcID;
    property Timer:                     TIdTimerQueue read fTimer write fTimer;
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

  TIdRTPMethod = class(TIdNotification)
  private
    fBinding: TIdConnectionBindings;
  public
    property Binding: TIdConnectionBindings read fBinding write fBinding;
  end;

  TIdRTPListenerReceiveRTCPMethod = class(TIdRTPMethod)
  private
    fPacket: TIdRTCPPacket;
  public
    procedure Run(const Subject: IInterface); override;

    property Packet: TIdRTCPPacket read fPacket write fPacket;
  end;

  TIdRTPListenerReceiveRTPMethod = class(TIdRTPMethod)
  private
    fPacket: TIdRTPPacket;
  public
    procedure Run(const Subject: IInterface); override;

    property Packet: TIdRTPPacket read fPacket write fPacket;
  end;

  TIdRTPSendListenerSendRTCPMethod = class(TIdRTPListenerReceiveRTCPMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdRTPSendListenerSendRTPMethod = class(TIdRTPListenerReceiveRTPMethod)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdRTPDataListenerNewDataMethod = class(TIdRTPMethod)
  private
    fData: TIdRTPPayload;
  public
    procedure Run(const Subject: IInterface); override;

    property Data: TIdRTPPayload read fData write fData;
  end;

  TIdRTPWait = class(TIdWait)
  private
    fSessionID: String;
  public
    property SessionID: String read fSessionID write fSessionID;
  end;

  // I represent the (possibly) deferred handling of an inbound packet.
  // Give me a COPY of the packet received; I'll free it myself.
  TIdRTPReceivePacketWait = class(TIdRTPWait)
  private
    fPacket:       TIdRTPBasePacket;
    fReceivedFrom: TIdConnectionBindings;

    procedure SetReceivedFrom(Value: TIdConnectionBindings);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Trigger; override;

    property Packet:       TIdRTPBasePacket      read fPacket write fPacket;
    property ReceivedFrom: TIdConnectionBindings read fReceivedFrom write SetReceivedFrom;
  end;

  TIdRTPSendDataWait = class(TIdRTPWait)
  private
    fData: TIdRTPPayload;
  public
    procedure Trigger; override;

    property Data: TIdRTPPayload read fData write fData;
  end;

  TIdRTPTransmissionTimeExpire = class(TIdRTPWait)
  public
    procedure Trigger; override;
  end;

  TIdRTPSenderReportWait = class(TIdRTPWait)
  public
    procedure Trigger; override;
  end;

  EBadEncodingName = class(Exception);
  ENoAgent = class(Exception);
  ENoPayloadTypeFound = class(Exception);
  EStreamTooShort = class(Exception);
  EUnknownSDES = class(Exception);

// Conversion functions  
function  DateTimeToNTPFractionsOfASecond(DT: TDateTime): Cardinal;
function  DateTimeToNTPSeconds(DT: TDateTime): Cardinal;
function  DateTimeToNTPTimestamp(DT: TDateTime): TIdNTPTimestamp;
function  DateTimeToRTPTimestamp(DT: TDateTime; ClockRate: Cardinal): TIdRTPTimestamp;
function  EncodeAsString(Value: Cardinal): String; overload;
function  EncodeAsString(Value: Word): String; overload;
function  HtoNL(Value: Cardinal): Cardinal;
function  HtoNS(Value: Word): Word;
function  NowAsNTP: TIdNTPTimestamp;
function  NtoHL(Value: Cardinal): Cardinal;
function  NtoHS(Value: Word): Word;

// Stream functions
function  Eof(Src: TStream): Boolean;
function  PeekByte(Src: TStream): Byte;
function  PeekWord(Src: TStream): Word;
function  ReadByte(Src: TStream): Byte;
function  ReadCardinal(Src: TStream): Cardinal;
procedure ReadNTPTimestamp(Src: TStream; var Timestamp: TIdNTPTimestamp);
function  ReadRemainderOfStream(Src: TStream): String;
function  ReadRemainderOfStreamAsWideString(Src: TStream): WIdeString;
function  ReadString(Src: TStream; Length: Cardinal): String;
function  ReadWord(Src: TStream): Word;
function  RoundUpToMultipleOfFour(Input: Cardinal): Cardinal;
function  TwosComplement(N: Int64): Int64;
procedure WriteByte(Dest: TStream; Value: Byte);
procedure WriteCardinal(Dest: TStream; Value: Cardinal);
procedure WriteNTPTimestamp(Dest: TStream; Value: TIdNTPTimestamp);
procedure WriteString(Dest: TStream; Value: String);
procedure WriteWideString(Dest: TStream; Value: WideString);
procedure WriteWord(Dest: TStream; Value: Word);

// From RFC 1521
const
  ApplicationMediaType = 'application';
  AudioMediaType       = 'audio';
  ImageMediaType       = 'image';
  MessageMediaType     = 'message';
  MultipartMediaType   = 'multipart';
  TextMediaType        = 'text';
  VideoMediaType       = 'video';

// From RFC 3550 and 3551
const
  RFC3550Version     = 2;
  AudioVisualProfile = 'RTP/AVP';

  CelBEncoding      = 'CelB';
  CNEncoding        = 'CN';
  DVI4Encoding      = 'DVI4';
  H263_1998Encoding = 'H263-1998';
  G722Encoding      = 'G722';
  G723Encoding      = 'G723';
  G726_16Encoding   = 'G726-16';
  G726_24Encoding   = 'G726-24';
  G726_32Encoding   = 'G726-32';
  G726_40Encoding   = 'G726-40';
  G728Encoding      = 'G728';
  G729Encoding      = 'G729';
  G729DEncoding     = 'G729D';
  G729EEncoding     = 'G729E';
  GSMEncoding       = 'GSM';
  GSM_EFREncoding   = 'GSM-EFR';
  H261Encoding      = 'H261';
  H263Encoding      = 'H263';
  JPEGEncoding      = 'JPEG';
  L8Encoding        = 'L8';
  L16Encoding       = 'L16';
  LPCEncoding       = 'LPC';
  MP2TEncoding      = 'MP2T';
  MPAEncoding       = 'MPA';
  MPVEncoding       = 'MPV';
  NVEncoding        = 'nv';
  PCMMuLawEncoding  = 'PCMU';
  PCMALawEncoding   = 'PCMA';
  QCELPEncoding     = 'QCELP';
  VDVIEncoding      = 'VDVI';

  CelBClockRate      = 90000;
  CNClockRate        = 8000;
  G722ClockRate      = 8000; // http://www.openh323.org/pipermail/openh323/2001-December/050691.html
  G723ClockRate      = 8000;
  G726_16ClockRate   = 8000;
  G726_24ClockRate   = 8000;
  G726_32ClockRate   = 8000;
  G726_40ClockRate   = 8000;
  G728ClockRate      = 8000;
  G729ClockRate      = 8000;
  G729DClockRate     = 8000;
  G729EClockRate     = 8000;
  GSMClockRate       = 8000;
  GSM_EFRClockRate   = 8000;
  H261ClockRate      = 90000;
  H263_1998ClockRate = 90000;
  JPEGClockRate      = 90000;
  L16ClockRate       = 44100;
  LPCClockRate       = 8000;
  MPAClockRate       = 90000;
  MPV2TClockRate     = 90000;
  MPVClockRate       = 90000;
  NVClockRate        = 90000;
  PCMALawClockRate   = 8000;
  PCMMuLawClockRate  = 8000;
  QCELPClockRate     = 8000;

  G722SamplingRate     = 16000;
  PCMMuLawSamplingRate = PCMMuLawClockRate;
  PCMALawSamplingRate  = PCMALawClockRate;

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

// From RFCs 4102, 4103
const
  InterleavedT140ClockRate    = 8000;
  RedundancyEncodingName      = 'red'; // RFC 4102
  RedundancyEncodingParameter = RedundancyEncodingName;
  T140ClockRate               = 1000;
  T140EncodingName            = 't140'; // RFC 4103
  T140LostChar                = #$ff#$fd;
  InterleavedT140MimeType     = AudioMediaType + '/' + T140EncodingName;
  RedundantT140MimeType       = TextMediaType + '/' + RedundancyEncodingName;
  T140MimeType                = TextMediaType + '/' + T140EncodingName;
  T140RecommendedBufferTime   = 300;
  T140ByteOrderMark           = WideChar($feff);
  T140Escape                  = WideChar($001b);
  T140StartOfString           = WideChar($0098);
  T140StringTerminator        = WideChar($009C);

  RedundancyEncoding          = RedundancyEncodingName + '/1000';
  T140Encoding                = T140EncodingName + '/1000';

// From RFC 2833
const
  DTMF0                   = 0;
  DTMF1                   = 1;
  DTMF2                   = 2;
  DTMF3                   = 3;
  DTMF4                   = 4;
  DTMF5                   = 5;
  DTMF6                   = 6;
  DTMF7                   = 7;
  DTMF8                   = 8;
  DTMF9                   = 9;
  DTMFStar                = 10;
  DTMFHash                = 11;
  DTMFA                   = 12;
  DTMFB                   = 13;
  DTMFC                   = 14;
  DTMFD                   = 15;
  DTMFFlash               = 16;
  TelephoneEventEncodingName = 'telephone-event';
  TelephoneEventMimeType  = AudioMediaType + '/' + TelephoneEventEncodingName;
  TelephoneEventClockRate = 8000;
  TelephoneEventEncoding = TelephoneEventEncodingName + '/8000';

const
  NullEncodingName     = 'null';
  ReservedEncodingName = 'reserved';

const
  ItemNotFoundIndex = -1;

implementation

uses
  DateUtils, IdHash, IdHashMessageDigest, IdRandom, IdRegisteredObject,
  IdSimpleParser, IdSystem, IdUnicode, Math, RuntimeSafety;

const
  JanOne1900           = 2;
  NTPNegativeTimeError = 'DT < 1900/01/01';
  RTPNegativeTimeError = 'DateTimeToRTPTimestamp doesn''t support negative timestamps';
  RTPLoopDetected      = 'RTP loop detected';

//******************************************************************************
//* Unit public functions & procedures                                         *
//******************************************************************************

function DateTimeToNTPFractionsOfASecond(DT: TDateTime): Cardinal;
var
  Divisor:         Int64;
  Fraction:        Double;
  FractionBit:     Cardinal;
  PartOfOneSecond: Double;
begin
  // NTP zero time = 1900/01/01 00:00:00. Since we're working with fractions
  // of a second though we don't care that DT might be before this time.

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
  if (DT < JanOne1900) then
    raise EConvertError.Create(NTPNegativeTimeError);

  Days := Trunc(DT) - JanOne1900;

  Result := MultiplyCardinal(Days, SecsPerDay) + SecondOfTheDay(DT);
end;

// Caveat Programmer: TDateTime has the floating point nature. Don't expect
// enormous precision.
// TODO: Maybe we can find a platform-independent way of getting an accurate
// timestamp?
function DateTimeToNTPTimestamp(DT: TDateTime): TIdNTPTimestamp;
begin
  if (DT < JanOne1900) then
    raise EConvertError.Create(NTPNegativeTimeError);

  Result.IntegerPart    := DateTimeToNTPSeconds(DT);
  Result.FractionalPart := DateTimeToNTPFractionsOfASecond(DT);
end;

function DateTimeToRTPTimestamp(DT: TDateTime; ClockRate: Cardinal): TIdRTPTimestamp;
var
  Temp: Int64;
begin
  if (DT < 0) then
    raise EConvertError.Create(RTPNegativeTimeError);

  if (ClockRate > 0) then begin
    Temp := Round(ClockRate / OneSecond * DT);
    Result := Temp and $ffffffff;
  end
  else
    Result := 0;
end;

function EncodeAsString(Value: Cardinal): String;
begin
  Result := Chr((Value and $ff000000) shr 24)
          + Chr((Value and $00ff0000) shr 16)
          + Chr((Value and $0000ff00) shr 8)
          + Chr( Value and $000000ff);
end;

function EncodeAsString(Value: Word): String;
begin
  Result := Chr((Value and $ff00) shr 8)
          + Chr( Value and $00ff);
end;

function HtoNL(Value: Cardinal): Cardinal;
begin
  Result := ((Value and $ff000000) shr 24)
         or ((Value and $00ff0000) shr 8)
         or ((Value and $0000ff00) shl 8)
         or ((Value and $000000ff) shl 24);
end;

function HtoNS(Value: Word): Word;
begin
  Result := ((Value and $00ff) shl 8) or ((Value  and $ff00) shr 8);
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

function NtoHS(Value: Word): Word;
begin
  Result := HtoNS(Value);
end;

function Eof(Src: TStream): Boolean;
var
  B: Byte;
  N: Integer;
begin
  N := Src.Read(B, 1);
  Result := N = 0;
  Src.Seek(-1, soFromCurrent);
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

function ReadRemainderOfStreamAsWideString(Src: TStream): WIdeString;
begin
  Result := '';
  while not Eof(Src) do
    Result := Result + WideChar(ReadWord(Src));
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

function RoundUpToMultipleOfFour(Input: Cardinal): Cardinal;
begin
  if ((Input mod 4) <> 0) then
    Inc(Input, 4);

  Result := Input - (Input mod 4);
end;

function TwosComplement(N: Int64): Int64;
begin
  Result := (not N) + 1;
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

procedure WriteString(Dest: TStream; Value: String);
begin
  if (Value <> '') then
    Dest.Write(Value[1], Length(Value));
end;

procedure WriteWideString(Dest: TStream; Value: WideString);
var
  I: Integer;
begin
  for I := 1 to Length(Value) do
    WriteWord(Dest, Word(Value[I]));
end;

procedure WriteWord(Dest: TStream; Value: Word);
begin
  Value := HtoNS(Value);
  Dest.Write(Value, SizeOf(Value));
end;

//******************************************************************************
//* TIdRTPPayload                                                              *
//******************************************************************************
//* TIdRTPPayload Public methods ***********************************************

class function TIdRTPPayload.CreateFrom(Payload: TIdRTPPayload;
                                        Src: TStream): TIdRTPPayload;
begin
  Result := Payload.Copy;
  try
    Result.ReadFrom(Src);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

class function TIdRTPPayload.CreatePayload(Name: String): TIdRTPPayload;
var
  EncodingName: String;
  Clock:        String;
  ClockRate:    Cardinal;
  Parameters:   String;
begin
  // Name should be something like "L16/44100/2"
  Parameters   := Name;
  EncodingName := Fetch(Parameters, '/');

  Clock := Fetch(Parameters, '/');
  if (Clock = '') then
    raise EBadEncodingName.Create(Name);
  ClockRate := StrToInt(Clock);

  if (Lowercase(EncodingName) = Lowercase(T140EncodingName)) then
    Result := TIdRTPT140Payload.Create
  else if (Lowercase(EncodingName) = Lowercase(TelephoneEventEncodingName)) then
    Result := TIdRTPTelephoneEventPayload.Create
  else begin
    Result := TIdRTPRawPayload.Create;
    TIdRTPRawPayload(Result).SetName(EncodingName);
  end;

  Result.ClockRate  := ClockRate;
  Result.Parameters := Parameters;
  Result.StartTime  := Now;
end;

class function TIdRTPPayload.CreateNullPayload: TIdRTPPayload;
begin
  Result := Self.CreatePayload(NullEncodingName + '/0');
end;

class function TIdRTPPayload.EncodingName(const Name: String;
                                          ClockRate: Cardinal;
                                          const Parameters: String = ''): String;
begin
  Result := Name + '/' + IntToStr(ClockRate);

  if (Parameters <> '') then
    Result := Result + '/' + Parameters;
end;

constructor TIdRTPPayload.Create;
begin
  inherited Create;

  Self.SamplingRate := Self.DefaultSamplingRate;
  Self.StartTime := Now;
end;

procedure TIdRTPPayload.Assign(Src: TPersistent);
var
  Other: TIdRTPPayload;
  S:     TStream;
begin
  if not (Src is TIdRTPPayload) then
    inherited Assign(Src)
  else begin
    Other := Src as TIdRTPPayload;
    S := TMemoryStream.Create;
    try
      Other.PrintOn(S);
      S.Seek(0, soFromBeginning);
      Self.ReadFrom(S);
    finally
      S.Free;
    end;
    Self.StartTime := Other.StartTime;
  end;
end;

function TIdRTPPayload.Copy: TIdRTPPayload;
begin
  Result := TIdRTPPayload.CreatePayload(Self.EncodingName);
  Result.Assign(Self);
end;

function TIdRTPPayload.EncodingName: String;
begin
  Result := TIdRTPPayload.EncodingName(Self.Name,
                                       Self.ClockRate,
                                       Self.Parameters);
end;

function TIdRTPPayload.HasKnownLength: Boolean;
begin
  Result := false;
end;

function TIdRTPPayload.HasSameEncoding(Other: TIdRTPPayload): Boolean;
begin
  Result := Self.EncodingName = Other.EncodingName;
end;

function TIdRTPPayload.IsNull: Boolean;
begin
  Result := false;
end;

function TIdRTPPayload.IsReserved: Boolean;
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

//* TIdRTPPayload Protected methods ********************************************

function TIdRTPPayload.DefaultSamplingRate: Cardinal;
begin
  Result := 0;
end;

function TIdRTPPayload.GetName: String;
begin
  Result := '';
end;

function TIdRTPPayload.GetStartTime: TDateTime;
begin
  Result := fStartTime;
end;

procedure TIdRTPPayload.SetStartTime(Value: TDateTime);
begin
  fStartTime := Value;
end;

//******************************************************************************
//* TIdNullPayload                                                             *
//******************************************************************************
//* TIdNullPayload Public methods **********************************************

function TIdNullPayload.IsNull: Boolean;
begin
  Result := true;
end;

//* TIdNullPayload Protected methods *******************************************

function TIdNullPayload.GetName: String;
begin
  Result := NullEncodingName;
end;

function TIdNullPayload.GetStartTime: TDateTime;
begin
  Result := Now;
end;

procedure TIdNullPayload.SetStartTime(Value: TDateTime);
begin
end;

//******************************************************************************
//* TIdRTPReservedPayload                                                      *
//******************************************************************************
//* TIdRTPReservedPayload Public methods ***************************************

function TIdRTPReservedPayload.IsReserved: Boolean;
begin
  Result := true;
end;

//* TIdRTPReservedPayload Protected methods ************************************

function TIdRTPReservedPayload.GetName: String;
begin
  Result := ReservedEncodingName;
end;

function TIdRTPReservedPayload.GetStartTime: TDateTime;
begin
  Result := Now;
end;

procedure TIdRTPReservedPayload.SetStartTime(Value: TDateTime);
begin
end;

//******************************************************************************
//* TIdRTPRawPayload                                                           *
//******************************************************************************
//* TIdRTPRawPayload Public methods ********************************************

constructor TIdRTPRawPayload.Create;
begin
  inherited Create;

  Self.fName := EncodingName;
end;

function TIdRTPRawPayload.Length: Cardinal;
begin
  Result := System.Length(Self.Data);
end;

procedure TIdRTPRawPayload.ReadFrom(Src: TStream);
begin
  Self.Data := ReadRemainderOfStream(Src);
end;

procedure TIdRTPRawPayload.PrintOn(Dest: TStream);
begin
  WriteString(Dest, Self.Data);
end;

procedure TIdRTPRawPayload.SetName(const Value: String);
begin
  fName := Value;
end;

//* TIdRTPRawPayload Protected methods *****************************************

function TIdRTPRawPayload.GetName: String;
begin
  Result := fName;
end;

//******************************************************************************
//* TIdRTPT140Payload                                                          *
//******************************************************************************
//* TIdRTPT140Payload Public methods *******************************************

constructor TIdRTPT140Payload.Create;
begin
  inherited Create;

  Self.ClockRate := T140ClockRate;
end;

function TIdRTPT140Payload.HasKnownLength: Boolean;
begin
  Result := false;
end;

function TIdRTPT140Payload.Length: Cardinal;
begin
  Result := System.Length(Self.Block)*SizeOf(WideChar);
end;

procedure TIdRTPT140Payload.ReadFrom(Src: TStream);
begin
  Self.Block := UTF8ToUTF16LE(ReadRemainderOfStream(Src));
end;

procedure TIdRTPT140Payload.PrintOn(Dest: TStream);
begin
  WriteString(Dest, UTF16LEToUTF8(Self.Block));
end;

//* TIdRTPT140Payload Protected methods ****************************************

function TIdRTPT140Payload.GetName: String;
begin
  Result := T140EncodingName;
end;

//******************************************************************************
//* TIdRTPTelephoneEventPayload                                                *
//******************************************************************************
//* TIdRTPTelephoneEventPayload Public methods *********************************

function TIdRTPTelephoneEventPayload.NumberOfSamples: Cardinal;
begin
  Result := Self.Duration;
end;

procedure TIdRTPTelephoneEventPayload.ReadFrom(Src: TStream);
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

procedure TIdRTPTelephoneEventPayload.PrintOn(Dest: TStream);
var
  B: Byte;
begin
  WriteByte(Dest, Self.Event);

  // RFC 2833 says that senders MUST NOT set the reserved bit.
  B := Self.Volume;
  if Self.IsEnd then
    B := B or $80;
  WriteByte(Dest, B);

  WriteWord(Dest, Self.Duration);
end;

//* TIdRTPTelephoneEventPayload Protected methods ******************************

function TIdRTPTelephoneEventPayload.GetName: String;
begin
  Result := TelephoneEventEncodingName;
end;

//******************************************************************************
//* TIdRTPPCMMuLawPayload                                                      *
//******************************************************************************
//* TIdRTPPCMMuLawPayload Protected methods ************************************

function TIdRTPPCMMuLawPayload.DefaultSamplingRate: Cardinal;
begin
  Result := PCMMuLawSamplingRate;
end;

//******************************************************************************
//* TIdRTPProfile                                                              *
//******************************************************************************
//* TIdRTPProfile Public methods ***********************************************

constructor TIdRTPProfile.Create;
begin
  inherited Create;

  Self.NullEncoding     := TIdNullPayload.Create;
  Self.ReservedEncoding := TIdRTPReservedPayload.Create;
  Self.Initialize;
end;

destructor TIdRTPProfile.Destroy;
begin
  Self.Clear;

  Self.ReservedEncoding.Free;
  Self.NullEncoding.Free;

  inherited Destroy;
end;

procedure TIdRTPProfile.AddEncoding(Encoding: TIdRTPPayload;
                                    PayloadType: TIdRTPPayloadType);
begin
  if Encoding.IsNull then
    Self.RemoveEncoding(PayloadType)
  else
    Self.AddEncodingAsReference(Encoding.Copy, PayloadType);
end;

procedure TIdRTPProfile.AddEncoding(Name: String;
                                    ClockRate: Cardinal;
                                    Params: String;
                                    PayloadType: TIdRTPPayloadType);
var
  Enc: TIdRTPPayload;
begin
  Enc := TIdRTPPayload.CreatePayload(TIdRTPPayload.EncodingName(Name,
                                                                ClockRate,
                                                                Params));
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
  Encoding: TIdRTPPayload;
  I:        TIdRTPPayloadType;
begin
  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
    Encoding := Self.EncodingFor(I);
    if not Encoding.IsNull and not Encoding.IsReserved then
      Self.RemoveEncoding(I);
  end;
end;

function TIdRTPProfile.Count: Integer;
var
  I: TIdRTPPayloadType;
begin
  Result := 0;
  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do
    if not Self.EncodingFor(I).IsNull then Inc(Result);
end;

function TIdRTPProfile.CreatePacket(Src: TStream): TIdRTPBasePacket;
var
  PacketType: Byte;
begin
  PacketType := Self.StreamContainsPayloadType(Src);

  if Self.IsRTCPPayloadType(PacketType) then
    Result := TIdCompoundRTCPPacket.Create
  else
    Result := TIdRTPPacket.Create(Self);
end;

function TIdRTPProfile.EncodingFor(PayloadType: TIdRTPPayloadType): TIdRTPPayload;
begin
  Result := Self.Encodings[PayloadType];
end;

function TIdRTPProfile.EncodingFor(EncodingName: String): TIdRTPPayload;
var
  I: Integer;
begin
  // You must use a full EncodingName - T140/1000, or L16/44100/2, etc.
  Result := Self.NullEncoding;
  I := Low(Self.Encodings);
  while (I <= High(Self.Encodings)) and Result.IsNull do
    if (Self.Encodings[I].EncodingName = EncodingName) then
      Result := Self.Encodings[I]
    else
      Inc(I);
end;

function TIdRTPProfile.FirstFreePayloadType: TIdRTPPayloadType;
var
  I: Cardinal;
begin
  I := 0;
  while (I <= High(TIdRTPPayloadType)) and not Self.EncodingFor(I).IsNull do
    Inc(I);

  Result := I and $FF;
end;

function TIdRTPProfile.HasEncoding(Encoding: TIdRTPPayload): Boolean;
begin
  Result := not Encoding.IsNull and (Self.IndexOfEncoding(Encoding) <> ItemNotFoundIndex);
end;

function TIdRTPProfile.HasPayloadType(PayloadType: TIdRTPPayloadType): Boolean;
begin
  Result := not Self.EncodingFor(PayloadType).IsNull;
end;

function TIdRTPProfile.IsFull: Boolean;
var
  I: TIdRTPPayloadType;
begin
  Result := true;

  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do begin
    Result := Result and not Self.EncodingFor(I).IsNull;
    if not Result then Break;
  end;
end;

function TIdRTPProfile.IsRTCPPayloadType(PayloadType: Byte): Boolean;
begin
  Result := (PayloadType >= RTCPSenderReport)
        and (PayloadType <= RTCPApplicationDefined);
end;

function TIdRTPProfile.PayloadTypeFor(Encoding: TIdRTPPayload): TIdRTPPayloadType;
var
  Index: Integer;
begin
  Index := Self.IndexOfEncoding(Encoding);

  if (Index = ItemNotFoundIndex) then
    raise ENoPayloadTypeFound.Create(Encoding.EncodingName)
  else
    Result := TIdRTPPayloadType(Index);
end;

function TIdRTPProfile.PayloadTypeFor(const EncodingName: String): TIdRTPPayloadType;
var
  Index: Integer;
begin
  Index := Self.IndexOfEncoding(EncodingName);

  if (Index = ItemNotFoundIndex) then
    raise ENoPayloadTypeFound.Create(EncodingName)
  else
    Result := TIdRTPPayloadType(Index);
end;

function TIdRTPProfile.StreamContainsEncoding(Src: TStream): TIdRTPPayload;
begin
  Result := Self.EncodingFor(Self.StreamContainsPayloadType(Src));
end;

function TIdRTPProfile.StreamContainsPayloadType(Src: TStream): TIdRTPPayloadType;
var
  FirstWord: Word;
begin
  FirstWord := PeekWord(Src);
  Result := FirstWord and $00ff;
end;

function TIdRTPProfile.TransportDesc: String;
begin
  Result := '';
end;

//* TIdRTPProfile Protected methods ********************************************

procedure TIdRTPProfile.AddEncodingAsReference(Encoding: TIdRTPPayload;
                                               PayloadType: TIdRTPPayloadType);
begin
  if not Self.HasPayloadType(PayloadType) and not Self.HasEncoding(Encoding) then
    Self.Encodings[PayloadType] := Encoding;
end;

procedure TIdRTPProfile.ReservePayloadType(PayloadType: TIdRTPPayloadType);
begin
  if    (Self.Encodings[PayloadType] <> Self.NullEncoding)
    and (Self.Encodings[PayloadType] <> Self.ReservedEncoding) then
    Self.Encodings[PayloadType].Free;

  Self.Encodings[PayloadType] := Self.ReservedEncoding;
end;

//* TIdRTPProfile Private methods **********************************************

function TIdRTPProfile.IndexOfEncoding(Encoding: TIdRTPPayload): Integer;
begin
  Result := 0;

  while (Result <= High(TIdRTPPayloadType))
    and not Self.EncodingFor(Result).HasSameEncoding(Encoding) do
      Inc(Result);

  if (Result > High(TIdRTPPayloadType)) then
    Result := ItemNotFoundIndex;
end;

function TIdRTPProfile.IndexOfEncoding(const EncodingName: String): Integer;
begin
  Result := 0;

  while (Result <= High(TIdRTPPayloadType))
    and not (Self.EncodingFor(Result).EncodingName = EncodingName) do
      Inc(Result);

  if (Result > High(TIdRTPPayloadType)) then
    Result := ItemNotFoundIndex;
end;

procedure TIdRTPProfile.Initialize;
var
  I: TIdRTPPayloadType;
begin
  for I := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do
    Self.Encodings[I] := Self.NullEncoding;
end;

procedure TIdRTPProfile.RemoveEncoding(PayloadType: TIdRTPPayloadType);
begin
  if    (Self.Encodings[PayloadType] <> Self.NullEncoding)
    and (Self.Encodings[PayloadType] <> Self.ReservedEncoding) then begin
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

  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(PCMMuLawEncoding + '/8000'),     0);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(GSMEncoding      + '/8000'),     3);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(G723Encoding     + '/8000'),     4);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(DVI4Encoding     + '/8000'),     5);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(DVI4Encoding     + '/16000'),    6);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(LPCEncoding      + '/8000'),     7);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(PCMALawEncoding  + '/8000'),     8);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(G722Encoding     + '/8000'),     9);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(L16Encoding      + '/44100/2'), 10);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(L16Encoding      + '/44100/1'), 11);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(QCELPEncoding    + '/8000'),    12);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(CNEncoding       + '/8000'),    13);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(MPAEncoding      + '/90000'),   14);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(G728Encoding     + '/8000'),    15);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(DVI4Encoding     + '/11025'),   16);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(DVI4Encoding     + '/22050'),   17);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(G729Encoding     + '/8000'),    18);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(CelBEncoding     + '/90000'),   25);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(JPEGEncoding     + '/90000'),   26);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(NVEncoding       + '/90000'),   28);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(H261Encoding     + '/90000'),   31);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(MPVEncoding      + '/90000'),   32);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(MP2TEncoding     + '/90000'),   33);
  Self.AddEncodingAsReference(TIdRTPPayload.CreatePayload(H263Encoding     + '/90000'),   34);

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

procedure TIdRTPHeaderExtension.SetData(Index: Word;
                                        Value: Cardinal);
begin
  fData[Index] := Value;
end;

procedure TIdRTPHeaderExtension.SetLength(Value: Word);
begin
  System.SetLength(fData, Value);
end;

//******************************************************************************
//* TIdRTCPReportBlock                                                         *
//******************************************************************************
//* TIdRTCPReportBlock Public methods ******************************************

procedure TIdRTCPReportBlock.GatherStatistics(Member: TIdRTPMember);
begin
  Self.CumulativeLoss     := Member.PacketLossCount;
  Self.DelaySinceLastSR   := Member.DelaySinceLastSenderReport;
  Self.FractionLost       := Member.PacketLossFraction;
  Self.HighestSeqNo       := Member.HighestSeqNo;
  Self.InterArrivalJitter := Member.Jitter;
  Self.LastSenderReport   := Member.LastSenderReport;
  Self.SyncSrcID          := Member.SyncSrcID;
end;

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

constructor TIdRTPBasePacket.Create;
begin
  inherited Create;
  
  Self.Version := RFC3550Version;
  Self.LengthSetManually := false;
end;

procedure TIdRTPBasePacket.Assign(Src: TPersistent);
var
  S: TStringStream;
begin
  if Src.ClassType <> Self.ClassType then
    inherited Assign(Src)
  else begin
    if Src is TIdRTPBasePacket then begin
      S := TStringStream.Create('');
      try
        TIdRTPBasePacket(Src).PrintOn(S);
        S.Seek(0, soFromBeginning);
        Self.ReadFrom(S);
      finally
        S.Free;
      end;
    end;
  end;
end;

function TIdRTPBasePacket.Copy: TIdRTPBasePacket;
begin
  Result := nil;
  RaiseAbstractError(Self.ClassName, 'Copy');
end;

function TIdRTPBasePacket.IsRTCP: Boolean;
begin
  Result := false;
end;

function TIdRTPBasePacket.IsRTP: Boolean;
begin
  Result := false;
end;

function TIdRTPBasePacket.IsValid: Boolean;
begin
  Result := false;
end;

procedure TIdRTPBasePacket.PrepareForTransmission(Session: TIdRTPSession);
begin
  Self.SyncSrcID := Session.SyncSrcID;
end;

procedure TIdRTPBasePacket.PrintOn(Dest: TStream);
begin
  RaiseAbstractError(Self.ClassName, 'PrintOn');
end;

procedure TIdRTPBasePacket.ReadFrom(Src: TStream);
begin
  RaiseAbstractError(Self.ClassName, 'ReadFrom');
end;

function TIdRTPBasePacket.RealLength: Word;
begin
  Result := 0;
  RaiseAbstractError(Self.ClassName, 'RealLength');
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
  WriteByte(Dest, PadLength);
end;

procedure TIdRTPBasePacket.SetSyncSrcID(Value: Cardinal);
begin
  fSyncSrcID := Value;
end;

//* TIdRTPBasePacket Private methods *******************************************

function TIdRTPBasePacket.GetLength: Word;
begin
  if Self.LengthSetManually then
    Result := Self.fLength
  else begin
    Result := RoundUpToMultipleOfFour(Self.RealLength) div 4;
    Dec(Result);
  end;
end;

procedure TIdRTPBasePacket.SetLength(const Value: Word);
begin
  Self.LengthSetManually := true;

  Self.fLength := Value;
end;

//******************************************************************************
//* TIdRTPPacket                                                               *
//******************************************************************************
//* TIdRTPPacket Public methods ************************************************

constructor TIdRTPPacket.Create(Profile: TIdRTPProfile);
begin
  inherited Create;

  fHeaderExtension := TIdRTPHeaderExtension.Create;
  fPayload         := TIdRTPPayload.CreateNullPayload;

  Self.Profile := Profile;
  Self.Version := Self.DefaultVersion;
end;

destructor TIdRTPPacket.Destroy;
begin
  fHeaderExtension.Free;

  Self.Payload.Free;

  inherited Destroy;
end;

function TIdRTPPacket.Copy: TIdRTPBasePacket;
begin
  Result := TIdRTPPacket.Create(Self.Profile);
  Result.Assign(Self);
end;

function TIdRTPPacket.CollidesWith(SSRC: Cardinal): Boolean;
var
  I: Integer;
begin
  Result := Self.SyncSrcID = SSRC;
  for I := 0 to Self.CsrcCount - 1 do
    Result := Result or (Self.CsrcIDs[I] = SSRC);
end;

function TIdRTPPacket.GetAllSrcIDs: TCardinalDynArray;
var
  I: Integer;
begin
  // Return an array with all the source identifiers mentioned in this packet.
  // This boils down to the SSRC of the sender + the set of CSRCs
  System.SetLength(Result, Self.CsrcCount + 1);
  Result[0] := Self.SyncSrcID;

  for I := 0 to Self.CsrcCount - 1 do
    Result[I + 1] := Self.CsrcIDs[I];
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

procedure TIdRTPPacket.PrepareForTransmission(Session: TIdRTPSession);
var
  RelativeTime: TDateTime;
begin
  inherited PrepareForTransmission(Session);

  Self.SequenceNo := Session.NextSequenceNo;

  RelativeTime := Session.TimeOffsetFromStart(Self.Payload.StartTime);

  if (RelativeTime < 0) then
    RelativeTime := 0;

//  Assert(RelativeTime >= 0,
//         'You can''t send a packet with time less than the session start time');

  Self.Timestamp := DateTimeToRTPTimestamp(RelativeTime,
                                           Self.Payload.ClockRate);
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
  WriteByte(Dest, B);

  B := Self.PayloadType;
  if Self.IsMarker then B := B or $80;
  WriteByte(Dest, B);

  WriteWord(Dest, Self.SequenceNo);

  WriteCardinal(Dest, Self.Timestamp);
  WriteCardinal(Dest, Self.SyncSrcID);

  for I := 0 to Self.CsrcCount - 1 do
    WriteCardinal(Dest, Self.CsrcIDs[I]);

  if Self.HasExtension then
    Self.HeaderExtension.PrintOn(Dest);

  Self.Payload.PrintOn(Dest);

  if Self.HasPadding then
    Self.PrintPadding(Dest);
end;

procedure TIdRTPPacket.ReadFrom(Src: TStream);
var
  B: Byte;
  I: TIdRTPCsrcCount;
begin
  // Populate Self's properties etc from the RTP packet in Src.
  // Self.Profile knows what packet types map to what encodings,
  // so we can read in the entire RTP packet (payload and all) as
  // long as the packet type has been mapped to an encoding.
  // Should the raw payload be used, we treat the entire stream as
  // the payload. This seems the only reasonable course of action.

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
    Self.ReadPayload(Src, Self.Profile);
end;

procedure TIdRTPPacket.ReadPayload(Src: TStream;
                                   Profile: TIdRTPProfile);
begin
  // This looks weird. What's happening is that the SetPayload recreates Self.
  // Payload with a TIdRTPPayload of the appropriate type, and then Self.Payload
  // will know how to read itself from Src.
  Self.Payload := Profile.EncodingFor(Self.PayloadType);

  Self.Payload.ReadFrom(Src);
end;

procedure TIdRTPPacket.ReadPayload(Src: String;
                                   Profile: TIdRTPProfile);
var
  S: TStringStream;
begin
  S := TStringStream.Create(Src);
  try
    Self.ReadPayload(S, Profile);
  finally
    S.Free;
  end;
end;

procedure TIdRTPPacket.ReadPayload(Data: TIdRTPPayload);
begin
  Self.PayloadType := Self.Profile.PayloadTypeFor(Data.EncodingName);
  Self.Payload := Data;
end;

function TIdRTPPacket.RealLength: Word;
const
  MandatoryHeaderSize = 12;
var
  OctetCount: Cardinal;
begin
  OctetCount := MandatoryHeaderSize
              + Self.CsrcCount*4
              + Self.Payload.Length;

  if Self.HasExtension then
    OctetCount := OctetCount + Self.HeaderExtension.OctetCount;

  Result := OctetCount and $FFFF;
end;

//* TIdRTPPacket Private methods ***********************************************

function TIdRTPPacket.DefaultVersion: TIdRTPVersion;
begin
  Result := 2;
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
    Self.ReadPayload(PayloadStream, Self.Profile);
  finally
    PayloadStream.Free;
  end;
end;

procedure TIdRTPPacket.SetCsrcCount(Value: TIdRTPCsrcCount);
begin
  fCsrcCount := Value;

  System.SetLength(fCsrcIDs, Value);
end;

procedure TIdRTPPacket.SetCsrcID(Index: TIdRTPCsrcCount;
                                 Value: Cardinal);
begin
  fCsrcIDs[Index] := Value;
end;

procedure TIdRTPPacket.SetPayload(Value: TIdRTPPayload);
begin
  // Make sure you set Self.PayloadType! Otherwise you'll run into problems
  // because the payload type doesn't match the type of the payload!
  fPayload.Free;

  fPayload := Value.Copy;
end;

//******************************************************************************
//* TIdRTCPPacket                                                              *
//******************************************************************************
//* TIdRTCPPacket Public methods ***********************************************

class function TIdRTCPPacket.RTCPType(PacketType: Byte): TIdRTCPPacketClass;
begin
  case PacketType of
    RTCPSenderReport:       Result := TIdRTCPSenderReport;
    RTCPReceiverReport:     Result := TIdRTCPReceiverReport;
    RTCPSourceDescription:  Result := TIdRTCPSourceDescription;
    RTCPGoodbye:            Result := TIdRTCPBye;
    RTCPApplicationDefined: Result := TIdRTCPApplicationDefined;
  else
    // Maybe we should have an TIdRTCPUnknown?
    Result := nil;
  end;
end;

constructor TIdRTCPPacket.Create;
begin
  inherited Create;
end;

function TIdRTCPPacket.Copy: TIdRTPBasePacket;
begin
  Result := TIdRTCPPacket.RTCPType(Self.PacketType).Create;
  Result.Assign(Self);
end;

function TIdRTCPPacket.IsBye: Boolean;
begin
  Result := false;
end;

function TIdRTCPPacket.IsReceiverReport: Boolean;
begin
  Result := false;
end;

function TIdRTCPPacket.IsRTCP: Boolean;
begin
  Result := true;
end;

function TIdRTCPPacket.IsSenderReport: Boolean;
begin
  Result := false;
end;

function TIdRTCPPacket.IsSourceDescription: Boolean;
begin
  Result := false;
end;

function TIdRTCPPacket.IsValid: Boolean;
begin
  Result := Self.Version = RFC3550Version;
end;

//* TIdRTCPPacket Protected methods ********************************************

procedure TIdRTCPPacket.AssertPacketType(PT: Byte);
begin
  Assert(PT = Self.GetPacketType,
         Self.ClassName + ' packet type');
end;

function TIdRTCPPacket.GetPacketType: Byte;
begin
  Result := 0;
  RaiseAbstractError(Self.ClassName, 'GetPacketType');
end;

//******************************************************************************
//* TIdRTCPMultiSSRCPacket                                                     *
//******************************************************************************
//* TIdRTCPMultiSSRCPacket Public methods **************************************

function TIdRTCPMultiSSRCPacket.GetAllSrcIDs: TCardinalDynArray;
begin
  Result := nil;
  RaiseAbstractError(Self.ClassName, 'GetAllSrcIDs');
end;

//******************************************************************************
//* TIdRTCPReceiverReport                                                      *
//******************************************************************************
//* TIdRTCPReceiverReport Public methods ***************************************

function TIdRTCPReceiverReport.GetAllSrcIDs: TCardinalDynArray;
var
  I, J: Integer;
begin
  System.SetLength(Result, Self.ReceptionReportCount + 1);
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

function TIdRTCPReceiverReport.GetPacketType: Byte;
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
  WriteByte(Dest, B);

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

procedure TIdRTCPReceiverReport.SetReceptionReportCount(Value: TIdRTCPReceptionCount);
begin
  Self.ClearReportBlocks;
  System.SetLength(fReceptionReports, Value);
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
  // common behaviour, rather than inheritance.
  Result := false;
end;

function TIdRTCPSenderReport.IsSenderReport: Boolean;
begin
  Result := true;
end;

procedure TIdRTCPSenderReport.PrepareForTransmission(Session: TIdRTPSession);
begin
  inherited PrepareForTransmission(Session);
  Self.NTPTimestamp := NowAsNTP;
end;

//* TIdRTCPSenderReport Protected methods **************************************

function TIdRTCPSenderReport.FixedHeaderByteLength: Word;
begin
  Result := 7*4;
end;

function TIdRTCPSenderReport.GetPacketType: Byte;
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

function TIdSrcDescChunkItem.ID: Byte;
begin
  Result := 0;
  RaiseAbstractError(Self.ClassName, 'ID');
end;

function TIdSrcDescChunkItem.Length: Byte;
begin
  Result := System.Length(Self.Data);
end;

procedure TIdSrcDescChunkItem.PrintOn(Dest: TStream);
begin
  WriteByte(Dest, Self.ID);
  WriteByte(Dest, Self.Length);
  WriteString(Dest, Self.Data);
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

  WriteString(Dest, Self.Prefix);
  WriteString(Dest, Self.Data);
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
    if (Self.ItemList.IndexOf(Result) <> ItemNotFoundIndex) then
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
  if Self.TooManyChunks then begin
    Result := nil;
    Exit;
  end;

  Result := TIdRTCPSrcDescChunk.Create;
  try
    Self.ChunkList.Add(Result);
  except
    if (Self.ChunkList.IndexOf(Result) <> ItemNotFoundIndex) then
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
  System.SetLength(Result, Self.ChunkCount);

  J := Low(Result);
  for I := 0 to Self.ChunkCount - 1 do begin
    Result[J] := Self.Chunks[I].SyncSrcID;
    Inc(J);
  end;
end;

function TIdRTCPSourceDescription.IsSourceDescription: Boolean;
begin
  Result := true;
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
  WriteByte(Dest, B);

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

function TIdRTCPSourceDescription.GetPacketType: Byte;
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

procedure TIdRTCPSourceDescription.SetSyncSrcID(Value: Cardinal);
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

function TIdRTCPSourceDescription.TooManyChunks: Boolean;
begin
  Result := Self.ChunkCount = High(Self.ChunkCount);
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

function TIdRTCPBye.GetAllSrcIDs: TCardinalDynArray;
var
  I, J: Integer;
begin
  System.SetLength(Result, Self.SourceCount);

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

procedure TIdRTCPBye.PrintOn(Dest: TStream);
var
  B: Byte;
  I: Integer;
begin
  B := Self.Version shl 6;
  B := B or Self.SourceCount;
  if Self.HasPadding then B := B or $20;
  WriteByte(Dest, B);
  WriteByte(Dest, Self.GetPacketType);
  WriteWord(Dest, Self.Length);

  for I := 0 to Self.SourceCount - 1 do
    WriteCardinal(Dest, Self.Sources[I]);

  if (Self.ReasonLength > 0) then
    Self.PrintReason(Dest);

  if Self.HasPadding then
    Self.PrintPadding(Dest);
end;

procedure TIdRTCPBye.ReadFrom(Src: TStream);
var
  B: Byte;
  I: Integer;
begin
  B := ReadByte(Src);
  Self.Version     := (B and $C0) shr 6;
  Self.HasPadding  := (B and $20) <> 0;
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

function TIdRTCPBye.GetPacketType: Byte;
begin
  Result := RTCPGoodbye;
end;

function TIdRTCPBye.GetSyncSrcID: Cardinal;
begin
  Result := Self.Sources[0];
end;

procedure TIdRTCPBye.SetSyncSrcID(Value: Cardinal);
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

procedure TIdRTCPBye.PrintReason(Dest: TStream);
var
  I: Integer;
begin
  WriteByte(Dest, Self.ReasonLength);
  WriteString(Dest, Self.Reason);

  for I := Self.ReasonLength to RoundUpToMultipleOfFour(Self.ReasonLength) do
    WriteByte(Dest, 0);
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
                               Value: Cardinal);
begin
  Self.SourceCount := Max(Self.SourceCount, Index + 1);
  fSources[Index] := Value;
end;

procedure TIdRTCPBye.SetSourceCount(Value: TIdRTCPSourceCount);
begin
  System.SetLength(fSources, Value);
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
  WriteByte(Dest, B);
  WriteByte(Dest, Self.GetPacketType);
  WriteWord(Dest, Self.Length);

  WriteCardinal(Dest, Self.SyncSrcID);

  WriteString(Dest, Self.Name);
  for I := 1 to 4 - System.Length(Name) do
    WriteByte(Dest, 0);

  WriteString(Dest, Self.Data);

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
  Self.Version    := (B and $C0) shr 6;
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

function TIdRTCPApplicationDefined.GetPacketType: Byte;
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
    fName := System.Copy(Value, 1, 4)
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

procedure TIdCompoundRTCPPacket.Assign(Src: TPersistent);
var
  I:      Integer;
  Other:  TIdCompoundRTCPPacket;
  Report: TIdRTCPPacket;
begin
  if (Src is TIdCompoundRTCPPacket) then begin
    Other := Src as TIdCompoundRTCPPacket;

    // This doesn't look very pleasant!
    for I := 0 to Other.PacketCount - 1 do begin
      Report := Other.PacketAt(I);
      Self.Add(TIdRTCPPacketClass(Report.ClassType)).Assign(Report);
    end;
  end
  else
    inherited Assign(Src);
end;

function TIdCompoundRTCPPacket.Copy: TIdRTPBasePacket;
begin
  Result := TIdCompoundRTCPPacket.Create;
  Result.Assign(Self);
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

function TIdCompoundRTCPPacket.HasReceiverReport: Boolean;
var
  I: Cardinal;
begin
  Result := false;

  I := 0;
  while (I < Self.PacketCount) and not Result do begin
    if Self.PacketAt(I).IsReceiverReport then
      Result := true;
    Inc(I);
  end;
end;  

function TIdCompoundRTCPPacket.HasSourceDescription: Boolean;
var
  I: Cardinal;
begin
  Result := false;

  I := 0;
  while (I < Self.PacketCount) and not Result do begin
    if Self.PacketAt(I).IsSourceDescription then
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
var
  FirstReport: TIdRTCPPacket;
begin
  FirstReport := Self.FirstPacket;

  Result := inherited IsValid
        and (Self.PacketCount > 0)
        and (FirstReport.IsSenderReport or FirstReport.IsReceiverReport)
        and not FirstReport.HasPadding;
end;

function TIdCompoundRTCPPacket.PacketAt(Index: Cardinal): TIdRTCPPacket;
begin
  Result := Self.Packets[Index] as TIdRTCPPacket;
end;

function TIdCompoundRTCPPacket.PacketCount: Cardinal;
begin
  Result := Self.Packets.Count;
end;

procedure TIdCompoundRTCPPacket.PrepareForTransmission(Session: TIdRTPSession);
var
  I: Cardinal;
begin
  if (Self.PacketCount > 0) then
    for I := 0 to Self.PacketCount - 1 do
      Self.PacketAt(I).PrepareForTransmission(Session);
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

function TIdCompoundRTCPPacket.GetPacketType: Byte;
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
    if (Self.Packets.IndexOf(Result) <> ItemNotFoundIndex) then
      Self.Packets.Remove(Result)
    else
      Result.Free;
    raise;
  end;
end;

//******************************************************************************
//* TIdRTPMember                                                               *
//******************************************************************************
//* TIdRTPMember Public methods ************************************************

constructor TIdRTPMember.Create;
begin
  inherited Create;

  Self.ControlAddress           := '';
  Self.ControlPort              := 0;
  Self.HasLeftSession           := false;
  Self.HasSyncSrcID             := false;
  Self.IsSender                 := false;
  Self.LocalAddress             := false;
  Self.MaxDropout               := Self.DefaultMaxDropout;
  Self.MaxMisOrder              := Self.DefaultMaxMisOrder;
  Self.MinimumSequentialPackets := Self.DefaultMinimumSequentialPackets;
  Self.Probation                := Self.MinimumSequentialPackets;
  Self.SentData                 := false;
  Self.SentControl              := false;
  Self.SourceAddress            := '';
  Self.SourcePort               := 0;
end;

function TIdRTPMember.DelaySinceLastSenderReport: Cardinal;
begin
  // Return the length of time, expressed in units of 1/65536 seconds,
  // since we last received a Sender Report from the source.

  if (Self.LastSenderReportReceiptTime = 0) then
    Result := 0
  else
    Result := Trunc(SecondSpan(Now, Self.LastSenderReportReceiptTime)*65536);
end;

procedure TIdRTPMember.InitSequence(Data: TIdRTPPacket);
begin
  Assert(Self.SyncSrcID = Data.SyncSrcID,
         'Member received an RTP packet not meant for it');

  Self.BaseSeqNo    := Data.SequenceNo;
  Self.BadSeqNo     := Self.SequenceNumberRange + 1;
  Self.HighestSeqNo := Data.SequenceNo;

  // Data is the very first packet from the newly-validated source
  Self.ReceivedPackets := 1;
end;

function TIdRTPMember.IsInSequence(Data: TIdRTPPacket): Boolean;
begin
  Result := Data.SequenceNo = Self.HighestSeqNo + 1;
end;

function TIdRTPMember.IsUnderProbation: Boolean;
begin
  Result := Self.Probation > 0;
end;

function TIdRTPMember.LastSenderReport: Cardinal;
var
  NTP: TIdNTPTimestamp;
begin
  // This returns the middle 32 bits out of an NTP timestamp of the last
  // time we received a Sender Report.

  if (Self.LastSenderReportReceiptTime = 0) then begin
    Result := 0;
    Exit;
  end;

  NTP := DateTimeToNTPTimestamp(Self.LastSenderReportReceiptTime);

  Result := ((NTP.IntegerPart and $0000ffff) shl 16)
        or ((NTP.FractionalPart and $ffff0000) shr 16);
end;

procedure TIdRTPMember.MarkAsSender(SSRC: Cardinal);
begin
  Self.HasSyncSrcID := true;
  Self.IsSender     := true;
  Self.SyncSrcID    := SSRC;
end;

function TIdRTPMember.PacketLossCount: Cardinal;
const
  MaxPacketLoss = $7fffff;
  MinPacketLoss = -$800000;
var
  Count: Int64;
begin
  // Return a 24-bit signed value of the number of packets lost in the last
  // report interval.

  Count := Int64(Self.ExpectedPacketCount) - Self.ReceivedPackets;

  if (Count > MaxPacketLoss) then
    Count := MaxPacketLoss
  else if (Count < MinPacketLoss) then
    Count := MinPacketLoss;

  // Translate into 24-bit signed value
  if (Count < 0) then
    Count := TwosComplement(Count);

  Result := Count and $ffffff;
end;

function TIdRTPMember.PacketLossFraction: Byte;
var
  ExpectedInterval: Int64;
  LostInterval:     Int64;
  ReceivedInterval: Int64;
begin
  // Return an 8-bit fixed-point fraction of the number of packets lost
  // in the last reporting interval.

  ExpectedInterval := Self.ExpectedPacketCount - Self.ExpectedPrior;
  ReceivedInterval := Self.ReceivedPackets - Self.ReceivedPrior;

  LostInterval := ExpectedInterval - ReceivedInterval;

  if (ExpectedInterval = 0) or (LostInterval < 0) then
    Result := 0
  else
    Result := ((LostInterval shl 8) div ExpectedInterval) and $ff;
end;

function TIdRTPMember.SequenceNumberRange: Cardinal;
begin
  Result := High(Self.BaseSeqNo) + 1; // 1 shl 16
end;

procedure TIdRTPMember.SetControlBinding(Binding: TIdConnectionBindings);
begin
  if not Self.SentControl then begin
    Self.ControlAddress := Binding.PeerIP;
    Self.ControlPort    := Binding.PeerPort;
    Self.SentControl    := true;
  end;
end;

procedure TIdRTPMember.SetDataBinding(Binding: TIdConnectionBindings);
begin
  Self.IsSender := true;

  if not Self.SentData then begin
    Self.SourceAddress := Binding.PeerIP;
    Self.SourcePort    := Binding.PeerPort;
    Self.SentData      := true;
  end;
end;

function TIdRTPMember.UpdateStatistics(Data: TIdRTPPacket;
                                       CurrentTime: TIdRTPTimestamp): Boolean;
begin
  Self.LastRTPReceiptTime := Now;
  
  Result := Self.UpdateSequenceNo(Data);
  Self.UpdateJitter(Data, CurrentTime);
  Self.UpdatePrior;
end;

procedure TIdRTPMember.UpdateStatistics(Stats: TIdRTCPPacket);
begin
  Self.LastRTCPReceiptTime := Now;

  if Stats.IsSenderReport then
    Self.LastSenderReportReceiptTime := Self.LastRTCPReceiptTime;
end;

//* TIdRTPMember Private methods ***********************************************

function TIdRTPMember.DefaultMaxDropout: Word;
begin
  // This tells us how big a gap in the sequence numbers we will
  // accept before invalidating the source.
  Result := 3000;
end;

function TIdRTPMember.DefaultMaxMisOrder: Word;
begin
  // We accept packets as valid if their sequence numbers are no more
  // than MaxMisOrder behind HighestSeqNo.
  Result := 100;
end;

function TIdRTPMember.DefaultMinimumSequentialPackets: Word;
begin
  // This tells us how many packets we must receive, _in_order_, before
  // we validate a source.
  Result := 2;
end;

function TIdRTPMember.ExpectedPacketCount: Cardinal;
var
  RealMax: Cardinal;
begin
  // How many packets do we think we should have received?
  
  RealMax := (Self.Cycles * Self.SequenceNumberRange)
           + Self.HighestSeqNo;

  Result := RealMax - Self.BaseSeqNo + 1;
end;

procedure TIdRTPMember.UpdateJitter(Data: TIdRTPPacket; CurrentTime: Cardinal);
var
  Delta:   Int64;
  Transit: Int64;
begin
  Transit := Int64(CurrentTime) - Data.Timestamp;
  Delta := Abs(Transit - Self.PreviousPacketTransit);
  Self.PreviousPacketTransit := Transit;

  Self.Jitter := (Int64(Self.Jitter) + Delta - ((Self.Jitter + 8) shr 4))
                 and $ffffffff;
end;

procedure TIdRTPMember.UpdatePrior;
begin
  Self.ExpectedPrior := Self.ExpectedPacketCount;
  Self.ReceivedPrior := Self.ReceivedPackets;
end;

function TIdRTPMember.UpdateSequenceNo(Data: TIdRTPPacket): Boolean;
var
  Delta: Cardinal;
begin
  // cf RFC 3550, appendix A.1
  if (Data.SequenceNo < Self.HighestSeqNo) then
    Delta := High(Data.SequenceNo) - (Self.HighestSeqNo - Data.SequenceNo)
  else
    Delta := Data.SequenceNo - Self.HighestSeqNo;

  if Self.IsUnderProbation then begin
    if Self.IsInSequence(Data) then begin
      Self.Probation := Self.Probation - 1;
      Self.HighestSeqNo  := Data.SequenceNo;

      if not Self.IsUnderProbation then begin
        Self.InitSequence(Data);
        Result := true;
        Exit;
      end;
    end
    else begin
      // First received packet - one down, Self.MinimumSequentialPackets - 1 to go
      Self.Probation := Self.MinimumSequentialPackets - 1;
      Self.HighestSeqNo  := Data.SequenceNo;
    end;
    Result := false;
    Exit;
  end
  else if (Delta < Self.MaxDropout) then begin
    // In order, with permissible gap
    if (Data.SequenceNo < Self.HighestSeqNo) then begin
      // Sequence number wrapped - count another 64k cycle
      Self.Cycles := Self.Cycles + 1;//Self.SequenceNumberRange;
    end;
    Self.HighestSeqNo := Data.SequenceNo;
  end
  else if (Delta < Self.SequenceNumberRange - Self.MaxMisOrder) then begin
    // The sequence made a very large jump
    if (Data.SequenceNo = Self.BadSeqNo) then begin
      // Two sequential packets - assume the other side restarted without
      // telling us so just re-sync (i.e., pretend this was the first
      // packet).
      Self.InitSequence(Data);
    end
    else begin
      Self.BadSeqNo := (Data.SequenceNo + 1) and (Self.SequenceNumberRange - 1);
      Result := false;
      Exit;
    end;
  end
  else begin
    // duplicate or re-ordered packet
  end;
  Self.ReceivedPackets := Self.ReceivedPackets + 1;
  Result := true;
end;

//******************************************************************************
//* TIdRTPMemberTable                                                          *
//******************************************************************************
//* TIdRTPMemberTable Public methods *******************************************

constructor TIdRTPMemberTable.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdRTPMemberTable.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

function TIdRTPMemberTable.Add(SSRC: Cardinal): TIdRTPMember;
begin
  if Self.Contains(SSRC) then begin
    Result := Self.Find(SSRC);
    Exit;
  end;

  Result := TIdRTPMember.Create;
  try
    Self.List.Add(Result);

    Result.HasSyncSrcID := true;
    Result.SyncSrcID    := SSRC;
  except
    if (Self.List.IndexOf(Result) <> ItemNotFoundIndex) then
      Self.List.Remove(Result)
    else
      FreeAndNil(Result);

    raise;
  end;
end;

function TIdRTPMemberTable.AddReceiver(Host: String; Port: Cardinal): TIdRTPMember;
begin
  // Sometimes we know we want to send data to someone even though they've not
  // sent us data first. For instance, when you set up a SIP session, you know
  // very well the session contains someone else (the remote end) but you don't
  // know their SSRC because you're sending data first. (Either that, or the
  // remote end faces the same problem - sending us data when they don't know
  // our SSRC).

  if Self.FindReceiver(Host, Port) <> nil then begin
    Result := Self.FindReceiver(Host, Port);
    Exit;
  end;

  Result := TIdRTPMember.Create;
  try
    Self.List.Add(Result);

    Result.SourceAddress  := Host;
    Result.SourcePort     := Port;

    Result.LastRTCPReceiptTime := Now;
    Result.LastRTPReceiptTime  := Now;
  except
    if (Self.List.IndexOf(Result) <> ItemNotFoundIndex) then
      Self.List.Remove(Result)
    else
      FreeAndNil(Result);

    raise;
  end;
end;

function TIdRTPMemberTable.AddSender(SSRC: Cardinal): TIdRTPMember;
begin
  Result := Self.Add(SSRC);
  Result.IsSender := true;
end;

procedure TIdRTPMemberTable.AdjustTransmissionTime(PreviousMemberCount: Cardinal;
                                                   var NextTransmissionTime: TDateTime;
                                                   var PreviousTransmissionTime: TDateTime);
var
  Timestamp: TDateTime;
begin
  Timestamp := Now;

  // PreviousMemberCount should NEVER be zero (since the session owning Self
  // has to at least have itself as a member. In the interests of safety, though,
  // we make sure we can't divide by zero. The methods calling this method should
  // also Assert that PreviousMemberCount > 0.
  if (PreviousMemberCount = 0) then PreviousMemberCount := 1;

  NextTransmissionTime := Timestamp
           + (Self.Count/PreviousMemberCount) * (NextTransmissionTime - Timestamp);

  PreviousTransmissionTime := Timestamp
           - (Self.Count/PreviousMemberCount) * (Timestamp - PreviousTransmissionTime);
end;

function TIdRTPMemberTable.Contains(SSRC: Cardinal): Boolean;
begin
  Result := Assigned(Self.Find(SSRC));
end;

function TIdRTPMemberTable.ContainsReceiver(Host: String; Port: Cardinal): Boolean;
begin
  Result := Assigned(Self.FindReceiver(Host, Port));
end;

function TIdRTPMemberTable.Count: Cardinal;
begin
  Result := Self.List.Count;
end;

function TIdRTPMemberTable.DeterministicSendInterval(ForSender: Boolean;
                                                     Session: TIdRTPSession): TDateTime;
begin
  Result := Session.DeterministicSendInterval(ForSender, Self);
end;

function TIdRTPMemberTable.Find(SSRC: Cardinal): TIdRTPMember;
var
  I:      Cardinal;
  Member: TIdRTPMember;
begin
  Result := nil;
  I := 0;

  while (I < Self.Count) and not Assigned(Result) do begin
    Member := Self.MemberAt(I);
    if Member.HasSyncSrcID and (Member.SyncSrcID = SSRC) then
      Result := Member
    else
      Inc(I);
  end;
end;

function TIdRTPMemberTable.FindReceiver(Host: String; Port: Cardinal): TIdRTPMember;
var
  I:      Cardinal;
  Member: TIdRTPMember;
begin
  Result := nil;
  I := 0;

  while (I < Self.Count) and not Assigned(Result) do begin
    Member := Self.MemberAt(I);
    if (Member.SourceAddress = Host)
      and (Member.SourcePort = Port) then
      Result := Member
    else
      Inc(I);
  end;
end;

function TIdRTPMemberTable.MemberAt(Index: Cardinal): TIdRTPMember;
begin
  // TODO: This will blow up when Index > High(Integer)
  Result := Self.List[Index] as TIdRTPMember;
end;

function TIdRTPMemberTable.MemberTimeout(Session: TIdRTPSession): TDateTime;
begin
  Result := Now
          - Session.MissedReportTolerance * Self.DeterministicSendInterval(false, Session);
end;

function TIdRTPMemberTable.ReceiverCount: Cardinal;
begin
  Result := Self.Count - Self.SenderCount;
end;

procedure TIdRTPMemberTable.Remove(SSRC: Cardinal);
begin
  // Note that if you try to remove an SSRC that is not in this session, nothing
  // happens. If you're looping over the members to remove some of them, use
  // Remove(Member: TIdRTPMember) instead.
  //
  // POSTCONDITION: if a member with the SSRC is in this session,
  // Self.List.Count decrements.

  Self.List.Remove(Self.Find(SSRC));
end;

procedure TIdRTPMemberTable.Remove(Member: TIdRTPMember);
begin
  Self.List.Remove(Member);
end;

procedure TIdRTPMemberTable.RemoveAll;
begin
  Self.List.Clear;
end;

procedure TIdRTPMemberTable.RemoveSources(Bye: TIdRTCPBye);
var
  I:   Cardinal;
  IDs: TCardinalDynArray;
begin
  IDs := Bye.GetAllSrcIDs;

  for I := Low(IDs) to High(IDs) do
    Self.Remove(IDs[I]);
end;

procedure TIdRTPMemberTable.RemoveTimedOutMembersExceptFor(CutoffTime: TDateTime;
                                                           SessionSSRC: Cardinal);
var
  I:      Cardinal;
  Member: TIdRTPMember;
begin
  I := 0;
  while (I < Self.Count) do begin
    Member := Self.MemberAt(I);
    if (Member.SyncSrcID <> SessionSSRC)
      and (Member.LastRTCPReceiptTime < CutoffTime) then
      Self.Remove(Member)
    else
      Inc(I)
  end;
end;

procedure TIdRTPMemberTable.RemoveTimedOutSendersExceptFor(CutoffTime: TDateTime;
                                                           SessionSSRC: Cardinal);
var
  I:      Cardinal;
  Member: TIdRTPMember;
begin
  I := 0;
  while (I < Self.Count) do begin
    Member := Self.MemberAt(I);
    if Member.IsSender
      and (Member.SyncSrcID <> SessionSSRC)
      and (Member.LastRTCPReceiptTime < CutoffTime) then
      Self.Remove(Member)
    else
      Inc(I);
  end;
end;

procedure TIdRTPMemberTable.SendControl(Packet: TIdRTCPPacket;
                                        Agent: IIdAbstractRTPPeer);
var
  I:      Integer;
  Member: TIdRTPMember;
begin
  for I := 0 to Self.Count - 1 do begin
    Member := Self.MemberAt(I);
    if (Member.SyncSrcID <> Packet.SyncSrcID) then
      Agent.SendPacket(Member.ControlAddress,
                       Member.ControlPort,
                       Packet);
  end;
end;

procedure TIdRTPMemberTable.SendData(Packet: TIdRTPPacket;
                                     Agent: IIdAbstractRTPPeer);
var
  I:      Integer;
  Member: TIdRTPMember;
begin
  for I := 0 to Self.Count - 1 do begin
    Member := Self.MemberAt(I);
    if (Member.SyncSrcID <> Packet.SyncSrcID) then
      Agent.SendPacket(Member.SourceAddress,
                       Member.SourcePort,
                       Packet);
  end;
end;

function TIdRTPMemberTable.SenderCount: Cardinal;
var
  I: Cardinal;
begin
  Result := 0;
  if (Self.Count > 0) then
    for I := 0 to Self.Count - 1 do
      if Self.MemberAt(I).IsSender then
        Inc(Result);

end;

function TIdRTPMemberTable.SenderTimeout(Session: TIdRTPSession): TDateTime;
begin
  Result := Now - 2*Self.SendInterval(Session);
end;

function TIdRTPMemberTable.SendInterval(Session: TIdRTPSession): TDateTime;
begin
  // Return the number of milliseconds until we must send the next RTCP
  // (i.e., SR/RR) packet to the members of this session.

  Result := Self.DeterministicSendInterval(Session.IsSender, Session)
          * Self.RandomTimeFactor
          / Self.CompensationFactor;
end;

procedure TIdRTPMemberTable.SetControlBinding(SSRC: Cardinal;
                                              Binding: TIdConnectionBindings);
var
  ID: TCardinalDynArray;
begin
  SetLength(ID, 1);
  ID[0] := SSRC;
  Self.SetControlBindings(ID, Binding);
end;

procedure TIdRTPMemberTable.SetControlBindings(SSRCs: TCardinalDynArray;
                                               Binding: TIdConnectionBindings);
var
  I:      Cardinal;
  Member: TIdRTPMember;
begin
  for I := Low(SSRCs) to High(SSRCs) do begin
    Member := Self.Find(SSRCs[I]);

    if not Assigned(Member) then
      Member := Self.Add(SSRCs[I]);

    Member.SetControlBinding(Binding);
  end;
end;

function TIdRTPMemberTable.SetDataBinding(SSRC: Cardinal;
                                          Binding: TIdConnectionBindings): TIdRTPMember;
begin
  Result := Self.Find(SSRC);

  if not Assigned(Result) then
    Result := Self.AddSender(SSRC);

  Result.SetDataBinding(Binding);
end;

procedure TIdRTPMemberTable.UpdateStatistics(CurrentRTPTime: Cardinal;
                                             RTP: TIdRTPPacket;
                                             Binding: TIdConnectionBindings);
var
  I:     Integer;
  SSRC:  TIdRTPMember;
begin
  SSRC := Self.Find(RTP.SyncSrcID);

  if not Assigned(SSRC) then begin
    // We don't know the sender by his SSRC.

    if Self.ContainsReceiver(Binding.PeerIP, Binding.PeerPort) then begin
      // We don't know the sender by his SSRC, but we are expecting data from
      // this Binding.

      SSRC := Self.FindReceiver(Binding.PeerIP, Binding.PeerPort);
      SSRC.MarkAsSender(RTP.SyncSrcID);
    end
    else begin
      // We don't know the sender by his SSRC, nor were we told to expect data
      // from this Binding. In the interests of being nice, we add this new,
      // unexpected, friend to our party.

      SSRC := Self.AddSender(RTP.SyncSrcID);
      SSRC.SetDataBinding(Binding);
    end;
  end;

  for I := 0 to RTP.CsrcCount - 1 do
    Self.SetDataBinding(RTP.CsrcIDs[I], Binding);

  Assert(Assigned(SSRC), 'SSRC is not known and was not added');

  SSRC.UpdateStatistics(RTP, CurrentRTPTime);
end;

//* TIdRTPMemberTable Private methods ******************************************

function TIdRTPMemberTable.CompensationFactor: Double;
begin
  // cf RFC 3550, section 6.3.1:
  // We divide the resulting value of T by e-3/2=1.21828 to compensate
  // for the fact that the timer reconsideration algorithm converges to
  // a value of the RTCP bandwidth below the intended average.

  Result := Exp(1) - 1.5;
end;

function TIdRTPMemberTable.RandomTimeFactor: Double;
begin
  // We want a factor in the range [0.5, 1.5]
  Result := GRandomNumber.NextDouble + 0.5;
end;

//******************************************************************************
//* TIdRTPSenderTable                                                          *
//******************************************************************************
//* TIdRTPSenderTable Public methods *******************************************

constructor TIdRTPSenderTable.Create(MemberTable: TIdRTPMemberTable);
begin
  inherited Create;

  Self.Members := MemberTable;
end;

function  TIdRTPSenderTable.Add(SSRC: Cardinal): TIdRTPMember;
begin
  Result := Self.Members.AddSender(SSRC);
end;

function TIdRTPSenderTable.Contains(SSRC: Cardinal): Boolean;
begin
  Result := Self.Members.Contains(SSRC);

  if Result then
    Result := Result and Self.Members.Find(SSRC).IsSender;
end;

function TIdRTPSenderTable.Count: Cardinal;
var
  I: Cardinal;
begin
  Result := 0;

  if (Self.Members.Count > 0) then
    for I := 0 to Self.Members.Count - 1 do
      if Self.Members.MemberAt(I).IsSender then
        Inc(Result);
end;

function TIdRTPSenderTable.Find(SSRC: Cardinal): TIdRTPMember;
var
  I:      Cardinal;
  Member: TIdRTPMember;
begin
  Result := nil;

  if (Self.Members.Count > 0) then begin
    for I := 0 to Self.Members.Count - 1 do begin
      Member := Self.Members.MemberAt(I);
      if Member.IsSender
        and (Member.SyncSrcID = SSRC) then begin
        Result := Member;
        Break;
      end;
    end;
  end;
end;

function TIdRTPSenderTable.SenderAt(Index: Cardinal): TIdRTPMember;
var
  I:           Cardinal;
  MemberIndex: Cardinal;
begin
  Result := nil;

  if (Self.Members.Count > 0) then begin
    MemberIndex := 0;
    I           := 0;

    // Loop invariant: MemberIndex <= I
    while (I < Self.Members.Count) and not Assigned(Result) do begin
      if Self.Members.MemberAt(I).IsSender then begin
        if (MemberIndex = Index) then
          Result := Self.Members.MemberAt(I)
        else
          Inc(MemberIndex);
      end;

      Inc(I);
    end;
  end;
end;

procedure TIdRTPSenderTable.Remove(SSRC: Cardinal);
begin
  Self.Members.Remove(SSRC);
end;

procedure TIdRTPSenderTable.Remove(Sender: TIdRTPMember);
begin
  Self.Members.Remove(Sender);
end;

procedure TIdRTPSenderTable.RemoveAll;
var
  I: Cardinal;
begin
  I := 0;
  if (Self.Members.Count > 0) then
    while (I < Self.Members.Count) do
     if Self.Members.MemberAt(I).IsSender then
       Self.Members.Remove(Self.Members.MemberAt(I))
     else
       Inc(I);
end;

//******************************************************************************
//* TIdBaseRTPAbstractPeer                                                     *
//******************************************************************************
//* TIdBaseRTPAbstractPeer Public methods **************************************

constructor TIdBaseRTPAbstractPeer.Create;
begin
  inherited Create;

  Self.Listeners := TIdNotificationList.Create;
  Self.SendListeners := TIdNotificationList.Create;

  Self.fSession := TIdRTPSession.Create(Self);
  Self.AddListener(Self.Session);
end;

destructor TIdBaseRTPAbstractPeer.Destroy;
begin
  Self.Session.Free;
  Self.SendListeners.Free;
  Self.Listeners.Free;

  inherited Destroy;
end;

procedure TIdBaseRTPAbstractPeer.AddListener(const Listener: IIdRTPListener);
begin
  Self.Listeners.AddListener(Listener);
end;

procedure TIdBaseRTPAbstractPeer.AddSendListener(const Listener: IIdRTPSendListener);
begin
  Self.SendListeners.AddListener(Listener);
end;

procedure TIdBaseRTPAbstractPeer.ReceivePacket(Packet: TIdRTPBasePacket;
                                               Binding: TIdConnectionBindings);
begin
  if Packet.IsRTCP then
    Self.NotifyListenersOfRTCP(Packet as TIdRTCPPacket, Binding)
  else
    Self.NotifyListenersOfRTP(Packet as TIdRTPPacket, Binding);
end;

procedure TIdBaseRTPAbstractPeer.RemoveListener(const Listener: IIdRTPListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdBaseRTPAbstractPeer.RemoveSendListener(const Listener: IIdRTPSendListener);
begin
  Self.SendListeners.RemoveListener(Listener);
end;

procedure TIdBaseRTPAbstractPeer.SendPacket(const Host: String;
                                            Port: Cardinal;
                                            Packet: TIdRTPBasePacket);
begin
end;

//* TIdBaseRTPAbstractPeer Protected methods ***********************************

function TIdBaseRTPAbstractPeer.GetActive: Boolean;
begin
  Result := false;
end;

function TIdBaseRTPAbstractPeer.GetAddress: String;
begin
  Result := '';
end;

function TIdBaseRTPAbstractPeer.GetDefaultPort: Integer;
begin
  Result := 0;
end;

function TIdBaseRTPAbstractPeer.GetRTCPPort: Cardinal;
begin
  Result := 0;
end;

function TIdBaseRTPAbstractPeer.GetRTPPort: Cardinal;
begin
  Result := 0;
end;

function TIdBaseRTPAbstractPeer.GetSession: TIdRTPSession;
begin
  Result := Self.fSession;
end;

function TIdBaseRTPAbstractPeer.GetTimer: TIdTimerQueue;
begin
  Result := Self.Session.Timer;
end;

procedure TIdBaseRTPAbstractPeer.NotifyListenersOfSentRTCP(Packet: TIdRTCPPacket;
                                                           Binding: TIdConnectionBindings);
var
  C: TIdCompoundRTCPPacket;
  I: Integer;
begin
  if Packet is TIdCompoundRTCPPacket then begin
    C := Packet as TIdCompoundRTCPPacket;
    for I := 0 to C.PacketCount - 1 do
      Self.NotifyListenersOfSingleSentRTCP(C.PacketAt(I), Binding);
  end
  else
    Self.NotifyListenersOfSingleSentRTCP(Packet, Binding);
end;

procedure TIdBaseRTPAbstractPeer.NotifyListenersOfSentRTP(Packet: TIdRTPPacket;
                                                          Binding: TIdConnectionBindings);
var
  Notification: TIdRTPSendListenerSendRTPMethod;
begin
  Notification := TIdRTPSendListenerSendRTPMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Packet  := Packet;

    Self.SendListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdBaseRTPAbstractPeer.NotifyListenersOfRTCP(Packet: TIdRTCPPacket;
                                                       Binding: TIdConnectionBindings);
var
  C: TIdCompoundRTCPPacket;
  I: Integer;
begin
  if Packet is TIdCompoundRTCPPacket then begin
    C := Packet as TIdCompoundRTCPPacket;
    for I := 0 to C.PacketCount - 1 do
      Self.NotifyListenersOfSingleRTCP(C.PacketAt(I), Binding);
  end
  else
    Self.NotifyListenersOfSingleRTCP(Packet, Binding);
end;

procedure TIdBaseRTPAbstractPeer.NotifyListenersOfRTP(Packet: TIdRTPPacket;
                                                      Binding: TIdConnectionBindings);
var
  Notification: TIdRTPListenerReceiveRTPMethod;
begin
  Notification := TIdRTPListenerReceiveRTPMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Packet  := Packet;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdBaseRTPAbstractPeer.SetActive(Value: Boolean);
begin
end;

procedure TIdBaseRTPAbstractPeer.SetAddress(Value: String);
begin
end;

procedure TIdBaseRTPAbstractPeer.SetDefaultPort(Value: Integer);
begin
end;

procedure TIdBaseRTPAbstractPeer.SetLocalProfile(Value: TIdRTPProfile);
begin
  Self.fLocalProfile        := Value;
  Self.Session.LocalProfile := Value;
end;

procedure TIdBaseRTPAbstractPeer.SetRemoteProfile(Value: TIdRTPProfile);
begin
  Self.fRemoteProfile        := Value;
  Self.Session.RemoteProfile := Value;
end;

procedure TIdBaseRTPAbstractPeer.SetRTCPPort(Value: Cardinal);
begin
end;

procedure TIdBaseRTPAbstractPeer.SetRTPPort(Value: Cardinal);
begin
end;

procedure TIdBaseRTPAbstractPeer.SetTimer(Value: TIdTimerQueue);
begin
  Self.Session.Timer := Value;
end;

//* TIdBaseRTPAbstractPeer Private methods *************************************

procedure TIdBaseRTPAbstractPeer.NotifyListenersOfSingleSentRTCP(Packet: TIdRTCPPacket;
                                                                 Binding: TIdConnectionBindings);
var
  Notification: TIdRTPSendListenerSendRTCPMethod;
begin
  Notification := TIdRTPSendListenerSendRTCPMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Packet  := Packet;

    Self.SendListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdBaseRTPAbstractPeer.NotifyListenersOfSingleRTCP(Packet: TIdRTCPPacket;
                                                             Binding: TIdConnectionBindings);
var
  Notification: TIdRTPListenerReceiveRTCPMethod;
begin
  Notification := TIdRTPListenerReceiveRTCPMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Packet  := Packet;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

//******************************************************************************
//* TIdRTPPeerRegistry                                                         *
//******************************************************************************
//* TIdRTPPeerRegistry Public methods ******************************************

class function TIdRTPPeerRegistry.FindServer(const ServerID: String): TIdBaseRTPAbstractPeer;
var
  O: TObject;
begin
  O := TIdObjectRegistry.FindObject(ServerID);

  if not Assigned(O) then
    raise ERegistry.Create(ServerID + ' does not point to a registered object');

  if not (O is TIdBaseRTPAbstractPeer) then
    raise ERegistry.Create(ServerID + ' points to a ' + O.ClassName + ', not a ' + TIdBaseRTPAbstractPeer.ClassName);

  Result := O as TIdBaseRTPAbstractPeer;
end;

class function TIdRTPPeerRegistry.ServerOn(Host: String; Port: Cardinal): TIdBaseRTPAbstractPeer;
var
  I:      Integer;
  L:      TStrings;
  Server: TIdBaseRTPAbstractPeer;
begin
  // Return the (first) server that's running on Host:Port, whether active or
  // not.
  Result := nil;

  L := Self.GetAllServers;
  try
    for I := 0 to L.Count - 1 do begin
      Server := L.Objects[I] as TIdBaseRTPAbstractPeer;
      if (Server.Address = Host) and (Server.RTPPort = Port) then begin
        Result := Server;
        Break;
      end;
    end;
  finally
    L.Free;
  end;
end;

class function TIdRTPPeerRegistry.ServerRunningOn(Host: String; Port: Cardinal): Boolean;
var
  I:      Integer;
  L:      TStrings;
  Server: TIdBaseRTPAbstractPeer;
begin
  // Return true if a TIdBaseRTPAbstractPeer is running on Host:Port.
  Result := false;

  L := Self.GetAllServers;
  try
    for I := 0 to L.Count - 1 do begin
      Server := L.Objects[I] as TIdBaseRTPAbstractPeer;

    if (Server.Address = Host) and (Server.RTPPort = Port) and Server.Active then begin
      Result := true;
      Break;
    end;
    end;
  finally
    L.Free;
  end;
end;

//* TIdRTPPeerRegistry Private methods *****************************************

class function TIdRTPPeerRegistry.GetAllServers: TStrings;
begin
  Result := TStringList.Create;

  TIdObjectRegistry.CollectAllObjectsOfClass(TIdBaseRTPAbstractPeer, Result);
end;

//******************************************************************************
//* TIdRTPSession                                                              *
//******************************************************************************
//* TIdRTPSession Public methods ***********************************************

constructor TIdRTPSession.Create(Agent: IIdAbstractRTPPeer);
begin
  inherited Create;

  Self.Listeners := TIdNotificationList.Create;

  Self.Agent          := Agent;
  Self.fNoControlSent := true;
  Self.MinimumRTCPSendInterval := Self.DefaultMinimumRTCPSendInterval;
  Self.NoDataSent     := true;

  Self.MemberLock := TCriticalSection.Create;
  Self.Members    := TIdRTPMemberTable.Create;

  Self.TransmissionLock := TCriticalSection.Create;

  Self.AssumedMTU            := Self.DefaultAssumedMTU;
  Self.MissedReportTolerance := Self.DefaultMissedReportTolerance;

  Self.Initialize;
end;

destructor TIdRTPSession.Destroy;
begin
  Self.TransmissionLock.Free;

  Self.MemberLock.Acquire;
  try
    Self.Members.Free;
  finally
    Self.MemberLock.Release;
  end;
  Self.MemberLock.Free;
  Self.Agent := nil;

  Self.Listeners.Free;

  inherited Destroy;
end;

function TIdRTPSession.AcceptableSSRC(SSRC: Cardinal): Boolean;
begin
  Result := (SSRC <> 0) and not Self.IsMember(SSRC);
end;

procedure TIdRTPSession.AddListener(const Listener: IIdRTPDataListener);
begin
  Self.Listeners.AddListener(Listener);
end;

function TIdRTPSession.AddMember(SSRC: Cardinal): TIdRTPMember;
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    Result := MemberTable.Add(SSRC);
  finally
    Self.UnlockMembers
  end;
end;

function TIdRTPSession.AddReceiver(Host: String; Port: Cardinal): TIdRTPMember;
var
  MemberTable: TIdRTPMemberTable;
begin
  // When we start up a session with the intention of sending data to someone
  // (as opposed to solely listening), we need to send data to that person
  // knowing only their address. Since they've not sent US any data, we can't
  // know their SSRC. Therefore we add them as a receiver, and note the first
  // SSRC we get from that address. From then on we act as normal.

  MemberTable := Self.LockMembers;
  try
    Result := MemberTable.AddReceiver(Host, Port);
  finally
    Self.UnlockMembers;
  end;
end;

function TIdRTPSession.AddSender(SSRC: Cardinal): TIdRTPMember;
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    Result := MemberTable.AddSender(SSRC);
  finally
    Self.UnlockMembers;
  end;
end;

function TIdRTPSession.CreateNextReport: TIdCompoundRTCPPacket;
begin
  // Either an RR or SR, plus an SDES
  Result := TIdCompoundRTCPPacket.Create;
  try
    Result.SyncSrcID := Self.SyncSrcID;
    Self.AddReports(Result);
    Self.AddSourceDesc(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TIdRTPSession.DeterministicSendInterval(ForSender: Boolean;
                                                 Table: TIdRTPMemberTable): TDateTime;
var
  MinInterval:         TDateTime;
  N:                   Cardinal;
  NewMaxRTCPBandwidth: Cardinal;
begin
  MinInterval := Self.MinimumRTCPSendInterval;

  if (Self.NoControlSent) then
    MinInterval := MinInterval / 2;

  NewMaxRTCPBandwidth := Self.MaxRTCPBandwidth;
  N := Table.Count;
  if (Table.SenderCount <= Round(Table.Count * Self.SenderBandwidthFraction)) then begin
    if ForSender then begin
      NewMaxRTCPBandwidth := Round(NewMaxRTCPBandwidth * Self.SenderBandwidthFraction);
      N := Table.SenderCount;
    end
    else begin
      NewMaxRTCPBandwidth := Round(NewMaxRTCPBandwidth * Self.ReceiverBandwidthFraction);
      N := Table.ReceiverCount;
    end;
  end;

  if (NewMaxRTCPBandwidth > 0) then begin
    Result := OneSecond * Self.AvgRTCPSize * N / NewMaxRTCPBandwidth;

    if (Result < MinInterval) then
      Result := MinInterval;
  end
  else
    Result := MinInterval;

  Self.MaxRTCPBandwidth := NewMaxRTCPBandwidth;
end;

procedure TIdRTPSession.Initialize;
begin
  Self.SequenceNo    := GRandomNumber.NextWord;
  Self.BaseTimestamp := GRandomNumber.NextCardinal;
  Self.BaseTime      := Now;

  Self.Members.RemoveAll;

  Self.SetSyncSrcId(Self.NewSSRC);
  Self.AddMember(Self.SyncSrcID).LocalAddress := true;
  Self.SenderBandwidthFraction   := Self.DefaultSenderBandwidthFraction;
  Self.ReceiverBandwidthFraction := Self.DefaultReceiverBandwidthFraction;

  Self.fPreviousMemberCount     := 1;
  Self.PreviousTransmissionTime := 0;
  Self.fAvgRTCPSize             := Self.DefaultNoControlSentAvgRTCPSize;
end;

function TIdRTPSession.IsMember(SSRC: Cardinal): Boolean;
begin
  Self.MemberLock.Acquire;
  try
    Result := Self.Members.Contains(SSRC);
  finally
    Self.MemberLock.Release;
  end;
end;

function TIdRTPSession.IsMember(const Host: String;
                                Port: Cardinal): Boolean;
begin
  Result := Assigned(Self.Member(Host, Port));
end;

function TIdRTPSession.IsSender: Boolean;
begin
  Result := Self.IsSender(Self.SyncSrcID);
end;

function TIdRTPSession.IsSender(SSRC: Cardinal): Boolean;
var
  Member: TIdRTPMember;
  Table:  TIdRTPMemberTable;
begin
  Table := Self.LockMembers;
  try
    Member := Table.Find(SSRC);
    Result := Assigned(Member) and Member.IsSender;
  finally
    Self.UnlockMembers;
  end;
end;

procedure TIdRTPSession.JoinSession;
begin
  Self.ScheduleReport(MilliSecondOfTheDay(Members.SendInterval(Self)));
end;

procedure TIdRTPSession.LeaveSession(const Reason: String = '');
var
  Bye: TIdRTCPBye;
begin
  // TODO: RFC 3550 section 6.3.7 - if a session contains more than 50 members
  // then there's a low-bandwidth algorithm for sending BYEs that prevents a
  // "storm".
  //
  // We don't need to add our own SSRC - PrepareTransmission will do that.
  Bye := TIdRTCPBye.Create;
  try
    Bye.Reason := Reason;

    Self.SendControl(Bye);
  finally
    Bye.Free;
  end;
end;

function TIdRTPSession.LockMembers: TIdRTPMemberTable;
begin
  Self.MemberLock.Acquire;
  Result := Self.Members;
end;

function TIdRTPSession.Member(SSRC: Cardinal): TIdRTPMember;
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    Result := MemberTable.Find(SSRC);
  finally
    Self.UnlockMembers;
  end;
end;

function TIdRTPSession.Member(const Host: String;
                              Port: Cardinal): TIdRTPMember;
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    Result := MemberTable.FindReceiver(Host, Port);
  finally
    Self.UnlockMembers;
  end;
end;

function TIdRTPSession.MemberCount: Cardinal;
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    Result := MemberTable.Count;
  finally
    Self.UnlockMembers;
  end;
end;

function TIdRTPSession.NewSSRC: Cardinal;
var
  Hash:   T4x4LongWordRecord;
  Hasher: TIdHash128;
  I:      Integer;
begin
  Result := 0;
  // This implementation's largely stolen from Appendix A.6 of RFC 3550.
  Hasher := TIdHashMessageDigest5.Create;
  try
    while not Self.AcceptableSSRC(Result) do begin
      // TODO: We should add more stuff here. RFC 3550's sample implementation
      // uses: pid, uid, gid and hostid (but hostid is deprecated according to
      // FreeBSD's gethostid(3) manpage).
      Hash := Hasher.HashValue(DateTimeToStr(Now)
                             + GetHostName
                             + IntToHex(GetCurrentProcessId, 8)
                             + IntToHex(GRandomNumber.NextCardinal, 8));

      Result := 0;
      for I := Low(Hash) to High(Hash) do
        Result := Result xor Hash[I];
    end;
  finally
    Hasher.Free;
  end;
end;

function TIdRTPSession.NextSequenceNo: TIdRTPSequenceNo;
begin
  Result := SequenceNo;
  SequenceNo := AddModuloWord(SequenceNo, 1);
end;

function TIdRTPSession.NothingSent: Boolean;
begin
  Result := Self.NoDataSent and Self.NoControlSent;
end;

procedure TIdRTPSession.ReceiveControl(RTCP: TIdRTCPPacket;
                                       Binding: TIdConnectionBindings);
begin
  if RTCP.IsBye then begin
    Self.RemoveSources(RTCP as TIdRTCPBye);
  end
  else begin
    Self.AdjustAvgRTCPSize(RTCP);

    if RTCP is TIdRTCPMultiSSRCPacket then
      Self.AddControlSources(RTCP as TIdRTCPMultiSSRCPacket, Binding)
    else
      Self.AddControlSource(RTCP.SyncSrcID, Binding);

    Self.Member(RTCP.SyncSrcID).UpdateStatistics(RTCP);
  end;
end;

procedure TIdRTPSession.ReceiveData(RTP: TIdRTPPacket;
                                    Binding: TIdConnectionBindings);
var
  CurrentRTPTime: Cardinal;
  MemberTable:    TIdRTPMemberTable;
begin
  if RTP.CollidesWith(Self.SyncSrcID) then
    Self.ResolveSSRCCollision;

  CurrentRTPTime := DateTimeToRTPTimestamp(Self.TimeOffsetFromStart(Now),
                                           RTP.Payload.ClockRate);

  MemberTable := Self.LockMembers;
  try
    MemberTable.UpdateStatistics(CurrentRTPTime, RTP, Binding);
  finally
    Self.UnlockMembers;
  end;

  // We send RTP (not necessarily valid, in-sequence RTP) up the stack
  Self.NotifyListenersOfData(RTP.Payload, Binding);
end;

function TIdRTPSession.ReceiverCount: Cardinal;
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    Result := MemberTable.ReceiverCount;
  finally
    Members.Free;
  end;
end;

procedure TIdRTPSession.RemoveListener(const Listener: IIdRTPDataListener);
begin
  Self.Listeners.RemoveListener(Listener);
end;

procedure TIdRTPSession.RemoveTimedOutMembers;
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    MemberTable.RemoveTimedOutMembersExceptFor(MemberTable.MemberTimeout(Self),
                                               Self.SyncSrcID);
    Assert(MemberTable.Count > 0, 'At the least, an RTP session should include itself as a member');
  finally
    Self.UnlockMembers;
  end;
end;

procedure TIdRTPSession.RemoveTimedOutSenders;
var
  MemberTable: TIdRTPMemberTable;
begin
  // Self can itself be timed out as a sender. That's fine.
  MemberTable := Self.LockMembers;
  try
    MemberTable.RemoveTimedOutSendersExceptFor(MemberTable.SenderTimeout(Self),
                                               Self.SyncSrcID);

    Assert(MemberTable.Count > 0, 'At the least, an RTP session should include itself as a member');
  finally
    Self.UnlockMembers;
  end;
end;

procedure TIdRTPSession.ResolveSSRCCollision;
begin
  // 1. Issue a BYE to all members
  // 2. Calculate a new, unused SSRC
  // 3. Rejoin
  Self.LeaveSession(RTPLoopDetected);
  Self.SetSyncSrcId(Self.NewSSRC);
  Self.JoinSession;
end;

procedure TIdRTPSession.SendControl(Packet: TIdRTCPPacket);
var
  MemberTable: TIdRTPMemberTable;
begin
  Packet.PrepareForTransmission(Self);

  if Packet.IsRTCP then
    Self.fNoControlSent := false;

  MemberTable := Self.LockMembers;
  try
    MemberTable.SendControl(Packet, Self.Agent);
  finally
    Self.UnlockMembers;
  end;
end;

procedure TIdRTPSession.SendData(Data: TIdRTPPayload);
var
  MemberTable: TIdRTPMemberTable;
  Packet:      TIdRTPPacket;
begin
  Packet := TIdRTPPacket.Create(Self.LocalProfile);
  try
    Packet.ReadPayload(Data);

    if not Self.IsSender(Self.SyncSrcID) then
      Self.AddSender(Self.SyncSrcID);

    Packet.PrepareForTransmission(Self);

    Self.IncSentOctetCount(Data.Length);
    Self.IncSentPacketCount;

    MemberTable := Self.LockMembers;
    try
      MemberTable.SendData(Packet, Self.Agent);
    finally
      Self.UnlockMembers;
    end;
  finally
    Packet.Free;
  end;
end;

function TIdRTPSession.Sender(SSRC: Cardinal): TIdRTPMember;
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    Result := MemberTable.Find(SSRC);

    if not Result.IsSender then
      Result := nil;
  finally
    Self.UnlockMembers;
  end;
end;

function TIdRTPSession.SenderCount: Cardinal;
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    Result := MemberTable.SenderCount;
  finally
    Self.UnlockMembers;
  end;
end;

procedure TIdRTPSession.SendReport;
var
  Report: TIdRTCPPacket;
begin
  Report := Self.CreateNextReport;
  try
    Self.SendControl(Report);
  finally
    Report.Free;
  end;
end;

function TIdRTPSession.TimeOffsetFromStart(WallclockTime: TDateTime): TDateTime;
begin
  Result := WallclockTime - Self.BaseTime;
end;

procedure TIdRTPSession.TransmissionTimeExpire;
var
  Table:                        TIdRTPMemberTable;
  PresumedNextTransmissionTime: TDateTime;
begin
  // It's time to send an RTCP SR/RR.

  Self.TransmissionLock.Acquire;
  try
    Table := Self.LockMembers;
    try
      Table.RemoveTimedOutSendersExceptFor(Table.SenderTimeout(Self),
                                           Self.SyncSrcID);
      Table.RemoveTimedOutMembersExceptFor(Table.MemberTimeout(Self),
                                           Self.SyncSrcID);
      Self.AdjustTransmissionTime(Table);

      PresumedNextTransmissionTime := Self.PreviousTransmissionTime
                                    + OneMillisecond*Members.SendInterval(Self);

      if (PresumedNextTransmissionTime < Now) then begin
        Self.SendReport;

        Self.ScheduleReport(MilliSecondOfTheDay(Now - PresumedNextTransmissionTime));
      end
      else begin
        // cf RFC 3550 Appendix A.7
        // We must redraw the interval.  Don't reuse the
        // one computed above, since it's not actually
        // distributed the same, as we are conditioned
        // on it being small enough to cause a packet to
        // be sent.
        Self.ScheduleReport(MilliSecondOfTheDay(Table.SendInterval(Self)));
      end;
    finally
      Self.UnlockMembers;
    end;
  finally
    Self.TransmissionLock.Release
  end;
end;


procedure TIdRTPSession.UnlockMembers;
begin
  Self.MemberLock.Release;
end;

//* TIdRTPSession Private methods **********************************************

function TIdRTPSession.AddAppropriateReportTo(Packet: TIdCompoundRTCPPacket): TIdRTCPReceiverReport;
begin
  if Self.IsSender then
    Result := Packet.AddSenderReport
  else
    Result := Packet.AddReceiverReport;
end;

procedure TIdRTPSession.AddControlSource(ID: Cardinal; Binding: TIdConnectionBindings);
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    MemberTable.SetControlBinding(ID, Binding);
  finally
    Self.UnlockMembers;
  end;
end;

procedure TIdRTPSession.AddControlSources(RTCP: TIdRTCPMultiSSRCPacket;
                                          Binding: TIdConnectionBindings);
var
  IDs:   TCardinalDynArray;
  MemberTable: TIdRTPMemberTable;
begin
  IDs := RTCP.GetAllSrcIDs;

  MemberTable := Self.LockMembers;
  try
    MemberTable.SetControlBindings(IDs, Binding);
  finally
    Self.UnlockMembers;
  end;
end;

procedure TIdRTPSession.AddReports(Packet: TIdCompoundRTCPPacket);
var
  I, J:    Cardinal;
  Table:   TIdRTPMemberTable;
  NumSrcs: Cardinal;
  Report:  TIdRTCPReceiverReport;
  Senders: TIdRTPSenderTable;
begin
  // Sender Reports and Receiver Reports can hold at most 31 report blocks.
  // Sessions can have many more members. Therefore we pack at most
  // 31 report blocks into one report, and just keep adding reports to
  // the compound packet. For instance, if this session has 51 members,
  // we'd create a compound packet looking like this:
  // [RR with 31 members][RR with 20 members].

  // TODO: for sessions with a very large number of members (where the
  // size of the packet exceeds the probable MTU of the underlying
  // transport) we should send several sets of RRs/SRs covering (more or
  // less disjoint) subsets of the session members.

  Table := Self.LockMembers;
  try
    Senders := TIdRTPSenderTable.Create(Table);
    try
      I      := 0;
      Report := Self.AddAppropriateReportTo(Packet);

      while (I < Senders.Count) do begin

        J := 0;
        NumSrcs := Min(High(TIdRTCPReceptionCount), Senders.Count - I);
        Report.ReceptionReportCount := NumSrcs;
        while (J < NumSrcs) do begin
          Report.Reports[J].GatherStatistics(Senders.SenderAt(I));
          Inc(J);
          Inc(I);
        end;

        if (I < Senders.Count) then
          Report := Self.AddAppropriateReportTo(Packet);
      end;
    finally
      Senders.Free;
    end;
  finally
    Self.UnlockMembers;
  end;
end;

procedure TIdRTPSession.AddSourceDesc(Packet: TIdCompoundRTCPPacket);
var
  Chunk: TIdRTCPSrcDescChunk;
  SDES:  TIdRTCPSourceDescription;
begin
  SDES := Packet.AddSourceDescription;
  Chunk := SDES.AddChunk;
  Chunk.SyncSrcID := Self.SyncSrcID;
  Chunk.AddCanonicalName(Self.CanonicalName);
end;

procedure TIdRTPSession.AdjustAvgRTCPSize(Control: TIdRTCPPacket);
begin
  Self.fAvgRTCPSize := Control.RealLength div 16
                     + (15*Self.AvgRTCPSize) div 16;
end;

procedure TIdRTPSession.AdjustTransmissionTime(Members: TIdRTPMemberTable);
var
  NextTransmissionTime:     TDateTime;
  PreviousTransmissionTime: TDateTime;
begin
  Members.AdjustTransmissionTime(Self.PreviousMemberCount,
                                 NextTransmissionTime,
                                 PreviousTransmissionTime);

  Self.NextTransmissionTime     := NextTransmissionTime;
  Self.PreviousTransmissionTime := PreviousTransmissionTime;
  Self.fPreviousMemberCount     := Members.Count;
end;

function TIdRTPSession.DefaultAssumedMTU: Cardinal;
begin
  Result := 1500;
end;

function TIdRTPSession.DefaultMinimumRTCPSendInterval: TDateTime;
begin
  Result := 5*OneSecond;
end;

function TIdRTPSession.DefaultMissedReportTolerance: Cardinal;
begin
  Result := 5;
end;

function TIdRTPSession.DefaultNoControlSentAvgRTCPSize: Cardinal;
begin
  Result := 20; // a small Source DEScription
end;

function TIdRTPSession.DefaultReceiverBandwidthFraction: Double;
begin
  Result := 1 - Self.DefaultSenderBandwidthFraction;
end;

function TIdRTPSession.DefaultSenderBandwidthFraction: Double;
begin
  Result := 0.25;
end;

procedure TIdRTPSession.IncSentOctetCount(N: Cardinal);
begin
  Inc(Self.fSentOctetCount, N);
end;

procedure TIdRTPSession.IncSentPacketCount;
begin
  Inc(Self.fSentPacketCount);
end;

procedure TIdRTPSession.NotifyListenersOfData(Data: TIdRTPPayload;
                                              Binding: TIdConnectionBindings);
var
  Notification: TIdRTPDataListenerNewDataMethod;
begin
  Notification := TIdRTPDataListenerNewDataMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Data    := Data;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdRTPSession.OnRTCP(Packet: TIdRTCPPacket;
                               Binding: TIdConnectionBindings);
begin
  Self.ReceiveControl(Packet, Binding);
end;

procedure TIdRTPSession.OnRTP(Packet: TIdRTPPacket;
                              Binding: TIdConnectionBindings);
begin
  Self.ReceiveData(Packet, Binding);
end;

procedure TIdRTPSession.RemoveSources(Bye: TIdRTCPBye);
var
  MemberTable: TIdRTPMemberTable;
begin
  MemberTable := Self.LockMembers;
  try
    MemberTable.RemoveSources(Bye);
    Self.AdjustTransmissionTime(MemberTable);
  finally
    Self.UnlockMembers;
  end;
end;

procedure TIdRTPSession.ResetSentOctetCount;
begin
  Self.fSentOctetCount := 0;
end;

procedure TIdRTPSession.ResetSentPacketCount;
begin
  Self.fSentPacketCount := 0;
end;

procedure TIdRTPSession.ScheduleReport(Milliseconds: Cardinal);
var
  Wait: TIdRTPTransmissionTimeExpire;
begin
  // Schedule the next RTCP send time.
  Wait := TIdRTPTransmissionTimeExpire.Create;
  Wait.SessionID := Self.ID;
  Self.Timer.AddEvent(Milliseconds, Wait);
end;

procedure TIdRTPSession.SetSyncSrcId(Value: Cardinal);
begin
  Self.fSyncSrcID := Value;
  Self.ResetSentOctetCount;
  Self.ResetSentPacketCount;
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

//******************************************************************************
//* TIdRTPListenerReceiveRTCPMethod                                            *
//******************************************************************************
//* TIdRTPListenerReceiveRTCPMethod Public methods *****************************

procedure TIdRTPListenerReceiveRTCPMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdRTPListener).OnRTCP(Self.Packet, Self.Binding);
end;

//******************************************************************************
//* TIdRTPListenerReceiveRTPMethod                                             *
//******************************************************************************
//* TIdRTPListenerReceiveRTPMethod Public methods ******************************

procedure TIdRTPListenerReceiveRTPMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdRTPListener).OnRTP(Self.Packet, Self.Binding);
end;

//******************************************************************************
//* TIdRTPSendListenerSendRTCPMethod                                           *
//******************************************************************************
//* TIdRTPSendListenerSendRTCPMethod Public methods ****************************

procedure TIdRTPSendListenerSendRTCPMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdRTPSendListener).OnSendRTCP(Self.Packet, Self.Binding);
end;

//******************************************************************************
//* TIdRTPSendListenerSendRTPMethod                                            *
//******************************************************************************
//* TIdRTPSendListenerSendRTPMethod Public methods *****************************

procedure TIdRTPSendListenerSendRTPMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdRTPSendListener).OnSendRTP(Self.Packet, Self.Binding);
end;

//******************************************************************************
//* TIdRTPDataListenerNewDataMethod                                            *
//******************************************************************************
//* TIdRTPDataListenerNewDataMethod Public methods *****************************

procedure TIdRTPDataListenerNewDataMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdRTPDataListener).OnNewData(Self.Data, Self.Binding);
end;

//******************************************************************************
//* TIdRTPReceivePacketWait                                                    *
//******************************************************************************
//* TIdRTPReceivePacketWait Public methods *************************************

constructor TIdRTPReceivePacketWait.Create;
begin
  inherited Create;

  Self.fReceivedFrom := TIdConnectionBindings.Create;
end;

destructor TIdRTPReceivePacketWait.Destroy;
begin
  Self.fReceivedFrom.Free;
  Self.Packet.Free;

  inherited Destroy;
end;

procedure TIdRTPReceivePacketWait.Trigger;
var
  S:       TObject;
  Session: TIdRTPSession;
begin
  S := TIdObjectRegistry.FindObject(Self.SessionID);

  if Assigned(S) and (S is TIdRTPSession) then begin
    Session := S as TIdRTPSession;

    if Self.Packet.IsRTCP then
      Session.ReceiveControl(Self.Packet as TIdRTCPPacket, Self.ReceivedFrom)
    else
      Session.ReceiveData(Self.Packet as TIdRTPPacket, Self.ReceivedFrom)
  end;
end;

//* TIdRTPReceivePacketWait Private methods ************************************

procedure TIdRTPReceivePacketWait.SetReceivedFrom(Value: TIdConnectionBindings);
begin
  Self.fReceivedFrom.Assign(Value);
end;

//******************************************************************************
//* TIdRTPSendDataWait                                                         *
//******************************************************************************
//* TIdRTPSendDataWait Public methods ******************************************

procedure TIdRTPSendDataWait.Trigger;
var
  Session: TObject;
begin
  Session := TIdObjectRegistry.FindObject(Self.SessionID);

  if Assigned(Session) and (Session is TIdRTPSession) then
    (Session as TIdRTPSession).SendData(Self.Data);
end;

//******************************************************************************
//* TIdRTPTransmissionTimeExpire                                               *
//******************************************************************************
//* TIdRTPTransmissionTimeExpire Public methods ********************************

procedure TIdRTPTransmissionTimeExpire.Trigger;
var
  Session: TObject;
begin
  Session := TIdObjectRegistry.FindObject(Self.SessionID);

  if Assigned(Session) and (Session is TIdRTPSession) then
    (Session as TIdRTPSession).TransmissionTimeExpire;
end;

//******************************************************************************
//* TIdRTPSenderReportWait                                                     *
//******************************************************************************
//* TIdRTPSenderReportWait Public methods **************************************

procedure TIdRTPSenderReportWait.Trigger;
var
  Session: TObject;
begin
  Session := TIdObjectRegistry.FindObject(Self.SessionID);

  if Assigned(Session) and (Session is TIdRTPSession) then
    (Session as TIdRTPSession).SendReport;
end;

end.
