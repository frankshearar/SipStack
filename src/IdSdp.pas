{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSdp;

interface

// Typically you don't manually instantiate the SDP objects (media
// descriptions, connections, attributes, etc). Usually you'll just have a
// string containing an SDP description, and you'll use a TIdSdpParser on it.

uses
  Classes, Contnrs, IdBaseThread, IdEmailAddress, IdConnectionBindings,
  IdInterfacedObject, IdNotification, IdRegisteredObject, IdRTP, IdRTPServer,
  IdSimpleParser, IdTcpServer, IdThreadableTcpClient, IdTimerQueue, SyncObjs,
  SysUtils;

type
  TIdNtpTimestamp      = Int64;
  TIdSdpBandwidthType  = (btConferenceTotal, btApplicationSpecific, btRS, btRR, btUnknown);
  TIdSdpConnectionType = (ctExisting, ctNew, ctUnknown); // RFC 4145
  TIdSdpDirection      = (sdInactive, sdRecvOnly, sdSendOnly, sdSendRecv, sdUnknown);
  TIdSdpKeyType        = (ktClear, ktBase64, ktURI, ktPrompt, ktUnknown);
  // Technically, Text doesn't exist. However, it will once
  // draft-ietf-sip-callee-caps gets an RFC number.
  TIdSdpMediaType      = (mtAudio, mtVideo, mtApplication, mtData, mtControl,
                          mtText, mtUnknown);
  TIdSdpSetupType      = (stActive, stActPass, stHoldConn, stPassive, stUnknown); // RFC 4145

  TIdPrintable = class(TPersistent)
  public
    constructor Create; virtual;

    function  AsString: String;
    procedure PrintOn(Dest: TStream); virtual;
  end;

  TIdPrintableClass = class of TIdPrintable;

  TIdSdpAttribute = class(TIdPrintable)
  private
    fName:  String;
    fValue: String;

  protected
    function  GetName: String; virtual;
    function  GetValue: String; virtual;
    procedure SetValue(const Value: String); virtual;
  public
    class function CreateAttribute(Value: String): TIdSdpAttribute;

    constructor Create; override;

    procedure Assign(Src: TPersistent); override;
    function  Copy: TIdSdpAttribute; virtual;
    function  Equals(Other: TIdSdpAttribute): Boolean;
    function  IsRTPMap: Boolean; virtual;
    procedure PrintOn(Dest: TStream); override;

    property Name:  String read GetName write fName;
    property Value: String read GetValue write SetValue;
  end;

  TIdSdpAttributeClass = class of TIdSdpAttribute;

  TIdSdpRTPMapAttribute = class(TIdSdpAttribute)
  private
    fPayloadType: String;
    fEncoding:    TIdRTPPayload;

    procedure SetEncoding(Value: TIdRTPPayload);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function IsRTPMap: Boolean; override;

    property Format:   String        read fPayloadType write fPayloadType;
    property Encoding: TIdRTPPayload read fEncoding;
  end;

  // Usually you need only look at my BandwidthType property. However, if you
  // receive an SDP with some unknown bandwidth type, my BandwidthType will be
  // btUnknown, and then you can look at my BandwidthName property to see what
  // kind of bandwidth constraint I represent.
  TIdSdpBandwidth = class(TIdPrintable)
  private
    fBandwidth:     Cardinal;
    fBandwidthName: String;
    fBandwidthType: TIdSdpBandwidthType;
  public
    procedure Assign(Src: TPersistent); override;
    procedure PrintOn(Dest: TStream); override;

    property Bandwidth:     Cardinal            read fBandwidth write fBandwidth;
    property BandwidthName: String              read fBandwidthName write fBandwidthName;
    property BandwidthType: TIdSdpBandwidthType read fBandwidthType write fBandwidthType;
  end;

  TIdSdpConnection = class(TIdPrintable)
  private
    fAddress:           String;
    fAddressType:       TIdIPVersion;
    fRoutableAddress:   String; // If you're behind a NAT, this will hold the NAT's external IP.
    fNetType:           String;
    fNumberOfAddresses: Cardinal;
    fTTL:               Byte;

    procedure SetAddress(Value: String);
    procedure SetRoutableAddress(Value: String);
  public
    procedure Assign(Src: TPersistent); override;
    function  Copy: TIdSdpConnection;
    procedure PrintOn(Dest: TStream); override;

    property AddressType:       TIdIPVersion read fAddressType write fAddressType;
    property Address:           String       read fAddress write SetAddress;
    property RoutableAddress:   String       read fRoutableAddress write SetRoutableAddress;
    property NetType:           String       read fNetType write fNetType;
    property NumberOfAddresses: Cardinal     read fNumberOfAddresses write fNumberOfAddresses;
    property TTL:               Byte         read fTTL write fTTL;
  end;

  TIdSdpKey = class(TIdPrintable)
  private
    fKeyName: String;
    fKeyType: TIdSdpKeyType;
    fValue:   String;
  public
    procedure Assign(Src: TPersistent); override;
    procedure PrintOn(Dest: TStream); override;

    property KeyName: String        read fKeyName write fKeyName;
    property KeyType: TIdSdpKeyType read fKeyType write fKeyType;
    property Value:   String        read fValue write fValue;
  end;

  TIdSdpAttributes = class;
  TIdSdpBandwidths = class;
  TIdSdpConnections = class;
  TIdSdpRTPMapAttributes = class;

  TIdSdpMediaDescription = class(TIdPrintable)
  private
    fAttributes:       TIdSdpAttributes;
    fBandwidths:       TIdSdpBandwidths;
    fConnections:      TIdSdpConnections;
    fInfo:             String;
    fKey:              TIdSdpKey;
    fMediaName:        String; // If MediaType is mtUnknown, MediaName holds the unrecognised media type.
    fMediaType:        TIdSdpMediaType;
    fRTPMapAttributes: TIdSdpRTPMapAttributes;
    FormatList:        TStrings;
    fPort:             TPortNum;
    fPortCount:        Cardinal;
    fProtocol:         String;

    procedure ClearKey;
    function  GetAttributes: TIdSdpAttributes;
    function  GetRTPMapAttributes: TIdSdpRTPMapAttributes;
    function  GetBandwidths: TIdSdpBandwidths;
    function  GetConnections: TIdSdpConnections;
    function  GetFormats(Index: Integer): String;
    function  GetKey: TIdSdpKey;
    procedure PrintInfoField(Dest: TStream);
    procedure PrintMediaField(Dest: TStream);

  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddAttribute(const Name, Value: String);
    procedure AddRTPMapAttribute(const EncodingName: String;
                                 PayloadType: TIdRTPPayloadType);
    procedure Assign(Src: TPersistent); override;
    procedure AddFormat(const Fmt: String);
    procedure ClearAttributes;
    procedure ClearFormats;
    function  Equals(Other: TIdSdpMediaDescription): Boolean;
    function  FormatCount: Integer;
    function  HasAttribute(Att: TIdSdpAttribute): Boolean;
    function  HasConnection: Boolean;
    function  HasFormat(Fmt: String): Boolean;
    function  HasKey: Boolean;
    function  IsRefusedStream: Boolean;
    function  IsText: Boolean;
    function  IsValidFormat(Token: String): Boolean;
    function  MediaTypeAsString: String;
    procedure PrintOn(Dest: TStream); override;
    procedure RefuseStream;
    function  UsesBinding(Binding: TIdConnectionBindings): Boolean;
    function  UsesRtpProtocol: Boolean;

    property Attributes:              TIdSdpAttributes       read GetAttributes;
    property Bandwidths:              TIdSdpBandwidths       read GetBandwidths;
    property Connections:             TIdSdpConnections      read GetConnections;
    property Formats[Index: Integer]: String                 read GetFormats;
    property Info:                    String                 read fInfo write fInfo;
    property Key:                     TIdSdpKey              read GetKey;
    property MediaName:               String                 read fMediaName write fMediaName;
    property MediaType:               TIdSdpMediaType        read fMediaType write fMediaType;
    property Port:                    TPortNum               read fPort write fPort;
    property PortCount:               Cardinal               read fPortCount write fPortCount;
    property RTPMapAttributes:        TIdSdpRTPMapAttributes read GetRTPMapAttributes;
    property Protocol:                String                 read fProtocol write fProtocol;
  end;

  TIdSdpOrigin = class(TIdPrintable)
  private
    fAddress:        String;
    fAddressType:    TIdIPVersion;
    fNetType:        String;
    fSessionID:      String;
    fSessionVersion: String;
    fUsername:       String;
  public
    procedure Assign(Src: TPersistent); override;
    procedure PrintOn(Dest: TStream); override;
    function  UsernameEncode(Name: String): String;

    property Address:        String       read fAddress write fAddress;
    property AddressType:    TIdIPVersion read fAddressType write fAddressType;
    property NetType:        String       read fNetType write fNetType;
    property SessionID:      String       read fSessionID write fSessionID;
    property SessionVersion: String       read fSessionVersion write fSessionVersion;
    property Username:       String       read fUsername write fUsername;
  end;

  TIdSdpRepeat = class(TIdPrintable)
  private
    fValue: String;
  public
    procedure Assign(Src: TPersistent); override;
    procedure PrintOn(Dest: TStream); override;

    property Value: String read fValue write fValue;
  end;

  TIdSdpZoneAdjustment = class(TIdPrintable)
  private
    fValue: String;
  public
    procedure Assign(Src: TPersistent); override;
    procedure PrintOn(Dest: TStream); override;

    property Value: String read fValue write fValue;
  end;

  TIdSdpRepeats = class;
  TIdSdpZoneAdjustments = class;

  TIdSdpTime = class(TIdPrintable)
  private
    fEndTime:         TIdNtpTimestamp;
    fStartTime:       TIdNtpTimestamp;
    fRepeats:         TIdSdpRepeats;
    fZoneAdjustments: TIdSdpZoneAdjustments;

    function GetRepeats: TIdSdpRepeats;
    function GetZoneAdjustments: TIdSdpZoneAdjustments;
  public
    destructor Destroy; override;

    procedure Assign(Src: TPersistent); override;
    procedure PrintOn(Dest: TStream); override;

    property EndTime:         TIdNtpTimestamp       read fEndTime write fEndTime;
    property Repeats:         TIdSdpRepeats         read GetRepeats;
    property StartTime:       TIdNtpTimestamp       read fStartTime write fStartTime;
    property ZoneAdjustments: TIdSdpZoneAdjustments read GetZoneAdjustments;
  end;

  TIdSdpList = class(TIdPrintable)
  protected
    List: TObjectList;

    function AddItem: TIdPrintable; overload;
    function AddItem(ToBeCopied: TIdPrintable): TIdPrintable; overload;
    function ItemType: TIdPrintableClass; virtual;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;
    procedure Clear;
    function  Count: Integer;
    function  Contains(O: TObject): Boolean;
    function  Equals(Other: TIdSdpList): Boolean;
    function  ItemAt(Index: Integer): TIdPrintable;
    procedure PrintOn(Dest: TStream); override;
    procedure Remove(O: TObject);
  end;

  TIdSdpAttributes = class(TIdSdpList)
  private
    function  GetDirection: TIdSdpDirection;
    function  GetItems(Index: Integer): TIdSdpAttribute;
    function  GetSetupType: TIdSdpSetupType;
    function  IndexOfAttributeNamed(Name: String): Integer;
    procedure SetDirection(Value: TIdSdpDirection);
    procedure SetSetupType(Value: TIdSdpSetupType);
  protected
    function ItemType: TIdPrintableClass; override;
  public
    function  Add: TIdSdpAttribute; overload;
    function  Add(Att: TIdSdpAttribute): TIdSdpAttribute; overload;
    procedure Add(A: TIdSdpAttributes); overload;
    procedure Add(const NameAndValue: String); overload;
    function  HasAttribute(Att: TIdSdpAttribute): Boolean;
    function  HasAttributeNamed(Name: String): Boolean;
    function  ValueFor(AttributeName: String): String;

    property Direction:             TIdSdpDirection read GetDirection write SetDirection;
    property Items[Index: Integer]: TIdSdpAttribute read GetItems; default;
    property SetupType:             TIdSdpSetupType read GetSetupType write SetSetupType;
  end;

  TIdSdpRTPMapAttributes = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpRTPMapAttribute;
  protected
    function ItemType: TIdPrintableClass; override;
  public
    function  Add: TIdSdpRTPMapAttribute; overload;
    function  Add(Att: TIdSdpRTPMapAttribute): TIdSdpRTPMapAttribute; overload;
    procedure Add(A: TIdSdpRTPMapAttributes); overload;
    function  Add(const Value: String): TIdSdpRTPMapAttribute; overload;
    function  EncodingFor(Format: String): String;
    function  FormatFor(EncodingName: String): String;
    function  HasAttribute(Att: TIdSdpAttribute): Boolean;

    property Items[Index: Integer]: TIdSdpRTPMapAttribute read GetItems; default;
  end;

  TIdSdpBandwidths = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpBandwidth;
  protected
    function ItemType: TIdPrintableClass; override;
  public
    function  Add: TIdSdpBandwidth; overload;
    function  Add(BW: TIdSdpBandwidth): TIdSdpBandwidth; overload;
    procedure Add(B: TIdSdpBandwidths); overload;

    property Items[Index: Integer]: TIdSdpBandwidth read GetItems; default;
  end;

  TIdSdpConnections = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpConnection;
  protected
    function ItemType: TIdPrintableClass; override;
  public
    function  Add: TIdSdpConnection; overload;
    function  Add(C: TIdSdpConnection): TIdSdpConnection; overload;
    procedure Add(C: TIdSdpConnections); overload;
    procedure AddConnection(NetType: String;
                            AddrType: TIdIPVersion;
                            Addr: String;
                            TTL: Byte);

    property Items[Index: Integer]: TIdSdpConnection read GetItems; default;
  end;

  TIdSdpMediaDescriptions = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpMediaDescription;
  protected
    function ItemType: TIdPrintableClass; override;
  public
    function Add: TIdSdpMediaDescription; overload;
    function Add(Desc: TIdSdpMediaDescription): TIdSdpMediaDescription; overload;
    function AllDescriptionsHaveConnections: Boolean;

    property Items[Index: Integer]: TIdSdpMediaDescription read GetItems; default;
  end;

  TIdSdpRepeats = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpRepeat;
  protected
    function ItemType: TIdPrintableClass; override;
  public
    function Add: TIdSdpRepeat; overload;
    function Add(R: TIdSdpRepeat): TIdSdpRepeat; overload;

    property Items[Index: Integer]: TIdSdpRepeat read GetItems; default;
  end;

  TIdSdpTimes = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpTime;
  protected
    function ItemType: TIdPrintableClass; override;
  public
    function Add: TIdSdpTime; overload;
    function Add(T: TIdSdpTime): TIdSdpTime; overload;

    property Items[Index: Integer]: TIdSdpTime read GetItems; default;
  end;

  TIdSdpZoneAdjustments = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpZoneAdjustment;
  protected
    function ItemType: TIdPrintableClass; override;
  public
    function Add: TIdSdpZoneAdjustment; overload;
    function Add(Adj: TIdSdpZoneAdjustment): TIdSdpZoneAdjustment; overload;

    property Items[Index: Integer]: TIdSdpZoneAdjustment read GetItems; default;
  end;

  TIdSdpPayload = class(TObject)
  private
    fAttributes:        TIdSdpAttributes;
    fBandwidths:        TIdSdpBandwidths;
    fConnections:       TIdSdpConnections;
    fEmailAddress:      TIdEmailAddressItem;
    fInfo:              String;
    fKey:               TIdSdpKey;
    fMediaDescriptions: TIdSdpMediaDescriptions;
    fOrigin:            TIdSdpOrigin;
    fPhoneNumber:       String;
    fRTPMapAttributes:  TIdSdpRTPMapAttributes;
    fSessionName:       String;
    fTimes:             TIdSdpTimes;
    fURI:               String;
    fVersion:           Cardinal;

    function  GetAttributes: TIdSdpAttributes;
    function  GetBandwidths: TIdSdpBandwidths;
    function  GetConnections: TIdSdpConnections;
    function  GetEmailAddress: TIdEmailAddressItem;
    function  GetKey: TIdSdpKey;
    function  GetMediaDescriptions: TIdSdpMediaDescriptions;
    function  GetOrigin: TIdSdpOrigin;
    function  GetTimes: TIdSdpTimes;
    function  MakeRTPMapAttributes: TIdSdpRTPMapAttributes;
    procedure PrintEmailAddressField(Dest: TStream);
    procedure PrintInfo(Dest: TStream);
    procedure PrintPhoneNumber(Dest: TStream);
    procedure PrintSessionNameField(Dest: TStream);
    procedure PrintUriField(Dest: TStream);
    procedure PrintVersionField(Dest: TStream);

    property Connections:       TIdSdpConnections       read GetConnections;
    property MediaDescriptions: TIdSdpMediaDescriptions read GetMediaDescriptions;
  public
    class function CreateFrom(Src: TStream): TIdSdpPayload; overload;
    class function CreateFrom(Src: String): TIdSdpPayload; overload;

    destructor Destroy; override;

    function  AddConnection: TIdSdpConnection; overload;
    function  AddMediaDescription(Desc: TIdSdpMediaDescription): TIdSdpMediaDescription; overload;
    function  AddMediaDescription: TIdSdpMediaDescription; overload;
    function  AllDescriptionsHaveConnections: Boolean;
    function  AsString: String;
    function  ConnectionAt(Index: Integer): TIdSdpConnection;
    function  ConnectionCount: Integer;
    function  Equals(Other: TIdSdpPayload; IgnoreTimestamps: Boolean = false): Boolean; overload;
    function  Equals(Other: String; IgnoreTimestamps: Boolean = false): Boolean; overload;
    procedure GetRtpMapAttributes(Atts: TIdSdpRTPMapAttributes);
    function  HasAttribute(Att: TIdSdpAttribute): Boolean;
    function  HasKey: Boolean;
    procedure InitializeProfile(Profile: TIdRTPProfile);
    function  MediaDescriptionAt(Index: Integer): TIdSdpMediaDescription;
    function  MediaDescriptionCount: Integer;
    function  MimeType: String;
    procedure PrintOn(Dest: TStream);
    procedure ReadFrom(Src: TStream); overload;
    procedure ReadFrom(Src: String); overload;
    procedure RemoveLastMediaDescription;
    procedure SetConnectionAddress(NewAddress: String);

    property Attributes:       TIdSdpAttributes       read GetAttributes;
    property Bandwidths:       TIdSdpBandwidths       read GetBandwidths;
    property EmailAddress:     TIdEMailAddressItem    read GetEmailAddress;
    property Info:             String                 read fInfo write fInfo;
    property Key:              TIdSdpKey              read GetKey;
    property Origin:           TIdSdpOrigin           read GetOrigin;
    property PhoneNumber:      String                 read fPhoneNumber write fPhoneNumber;
    property RTPMapAttributes: TIdSdpRTPMapAttributes read MakeRTPMapAttributes;
    property SessionName:      String                 read fSessionName write fSessionName;
    property Times:            TIdSdpTimes            read GetTimes;
    property URI:              String                 read fUri write fUri;
    property Version:          Cardinal               read fVersion write fVersion;
  end;

  // I implement RFCs 2327 and 3266.
  // I canonicalise header information in the following way:
  // * If session-level connections or attributes exist, I copy these into
  //   each media description.
  // * If a connection contains multiple (multicast) addresses, then I add
  //   multiple connection headers, one for each of the multicast addresses;
  //   e.g., I convert the address 224.0.0.1/127/2 into two addresses,
  //   viz., 224.0.0.1/127 and 224.0.0.2/127.
  TIdSdpParser = class(TIdSimpleParser)
  private
    LastMediaHeader:       Char;
    LastSessionHeader:     Char;
    ParsingSessionHeaders: Boolean;

    class function ContainsOnly(Token: String; AllowedChars: TCharSet): Boolean;
    class function ContainsNoneOf(Token: String; DisallowedChars: TCharSet): Boolean;
    procedure AssertHeaderOrder;
    function  GetAndCheckInfo: String;
    procedure ParseAttribute(Attributes: TIdSdpAttributes);
    procedure ParseBandwidth(Bandwidths: TIdSdpBandwidths);
    procedure ParseConnection(Connections: TIdSdpConnections);
    procedure ParseEmail(Payload: TIdSdpPayload);
    procedure ParseHeader(var Name, Value: String);
    procedure ParseInfo(MediaDescription: TIdSdpMediaDescription); overload;
    procedure ParseInfo(Payload: TIdSdpPayload); overload;
    procedure ParseKey(Key: TIdSdpKey);
    procedure ParseMediaDescription(Payload: TIdSdpPayload);
    procedure ParseMediaOptionalHeaders(MediaDescription: TIdSdpMediaDescription);
    procedure ParseOrigin(Payload: TIdSdpPayload);
    procedure ParsePhone(Payload: TIdSdpPayload);
    procedure ParseRepeat(Time: TIdSdpTime);
    procedure ParseRTPMapAttribute(RTPMapAttributes: TIdSdpRTPMapAttributes);
    procedure ParseSessionHeaders(Payload: TIdSdpPayload);
    procedure ParseSessionOptionalHeaders(Payload: TIdSdpPayload);
    procedure ParseSessionName(Payload: TIdSdpPayload);
    procedure ParseTime(Payload: TIdSdpPayload);
    procedure ParseZoneAdjustment(Time: TIdSdpTime);
    procedure ParseURI(Payload: TIdSdpPayload);
    procedure ParseVersion(Payload: TIdSdpPayload);
  public
    class function IsAddressType(const Token: String): Boolean;
    class function IsBandwidthType(const Token: String): Boolean;
    class function IsByteString(const Token: String): Boolean;
    class function IsDirection(const Token: String): Boolean;
    class function IsKeyData(const Token: String): Boolean;
    class function IsKeyType(const Token: String): Boolean;
    class function IsMediaType(const Token: String): Boolean;
    class function IsMimeType(const Token: String): Boolean;
    class function IsMulticastAddress(IpVersion: TIdIPVersion;
                                      const Token: String): Boolean;
    class function IsNetType(const Token: String): Boolean;
    class function IsPhone(const Token: String): Boolean;
    class function IsPhoneNumber(const Header: String): Boolean;
    class function IsPort(const Token: String): Boolean;
    class function IsProtocol(const Token: String): Boolean;
    class function IsText(const Token: String): Boolean;
    class function IsTime(const Token: String): Boolean;
    class function IsToken(const S: String): Boolean;

    procedure Parse(Payload: TIdSdpPayload);
  end;

  TIdSdpBaseMediaStream = class;

  IIdSdpMediaListener = interface
    ['{5BD6E9C8-45BA-49AD-AD99-D655CD6FFDA6}']
    procedure OnData(Stream: TIdSdpBaseMediaStream;
                     Chunk: TStream;
                     Format: String;
                     Binding: TIdConnectionBindings);
  end;

  IIdSdpMediaSendListener = interface
    ['{B4982215-66A8-49C4-85DD-E3D6144AAFF8}']
    procedure OnSentData(Stream: TIdSdpBaseMediaStream;
                         Chunk: TStream;
                         Format: String;
                         LayerID: Integer);
  end;

  TIdSdpMediaStreamFactory = class;

  TIdSdpBaseMediaStream = class(TIdInterfacedObject)
  private
    DataListeners:       TIdNotificationList;
    DataSendListeners:   TIdNotificationList;
    fHighestAllowedPort: TPortNum;
    fIsOffer:            Boolean;
    fOnHold:             Boolean;
    fRemoteDescription:  TIdSdpMediaDescription;
    fLocalDescription:   TIdSdpMediaDescription;
    fLowestAllowedPort:  TPortNum;
    fTimer:              TIdTimerQueue;
    PreHoldDirection:    TIdSdpDirection;

    function  GetDirection: TIdSdpDirection;
    function  GetPortCount: Cardinal;
    procedure SetDirection(Value: TIdSdpDirection);
    procedure SetLocalDescription(Value: TIdSdpMediaDescription);
    procedure SetRemoteDescription(const Value: TIdSdpMediaDescription);
    function  SufficientPortsFree(Port: TPortNum): Boolean;
  protected
    Factory: TIdSdpMediaStreamFactory;

    procedure AfterSetLocalDescription(Value: TIdSdpMediaDescription); virtual;
    procedure AfterSetRemoteDescription(Value: TIdSdpMediaDescription); virtual;
    procedure BeforeSetLocalDescription(Value: TIdSdpMediaDescription); virtual;
    procedure BeforeSetRemoteDescription(Value: TIdSdpMediaDescription); virtual;
    procedure BindListeningStreams;
    function  GetPort(Index: Integer): TPortNum; virtual;
    procedure InitializeLocalServers; virtual;
    procedure InternalCreate; virtual;
    function  NextPort(PreviousPort: TPortNum): TPortNum; virtual;
    procedure NotifyOfData(Binding: TIdConnectionBindings; Data: TStream; Format: String);
    procedure NotifyOfSentData(Data: TStream; Format: String; LayerID: Integer);
    procedure ReallySendData(Data: TStream; Format: String; LayerID: Integer = 0); virtual;
    function  RequiredPorts: Cardinal; virtual;
    procedure SetTimer(Value: TIdTimerQueue); virtual;
    procedure StartServers; virtual;
  public
    constructor Create(Factory: TIdSdpMediaStreamFactory); virtual;
    destructor  Destroy; override;

    procedure AddDataListener(const Listener: IIdSdpMediaListener);
    procedure AddDataSendListener(const Listener: IIdSdpMediaSendListener);
    function  AllowedPort(Port: TPortNum): Boolean;
    function  IsListening: Boolean; virtual;
    function  IsNull: Boolean; virtual;
    function  IsReceiver: Boolean;
    function  IsRefusedStream: Boolean;
    function  IsSender: Boolean;
    function  IsText: Boolean;
    procedure JoinSession; virtual;
    procedure PutOnHold;
    procedure RemoveDataListener(const Listener: IIdSdpMediaListener);
    procedure RemoveDataSendListener(const Listener: IIdSdpMediaSendListener);
    procedure SendData(Data: TStream; Format: String; LayerID: Integer = 0);
    procedure StartListening; virtual;
    procedure StopListening; virtual;
    procedure TakeOffHold;
    function  UsesBinding(Binding: TIdConnectionBindings): Boolean;

    property Direction:             TIdSdpDirection        read GetDirection write SetDirection;
    property HighestAllowedPort:    TPortNum               read fHighestAllowedPort write fHighestAllowedPort;
    property IsOffer:               Boolean                read fIsOffer write fIsOffer;
    property LocalDescription:      TIdSdpMediaDescription read fLocalDescription write SetLocalDescription;
    property LowestAllowedPort:     TPortNum               read fLowestAllowedPort write fLowestAllowedPort;
    property OnHold:                Boolean                read fOnHold;
    property PortCount:             Cardinal               read GetPortCount;
    property Ports[Index: Integer]: TPortNum               read GetPort;
    property RemoteDescription:     TIdSdpMediaDescription read fRemoteDescription write SetRemoteDescription;
    property Timer:                 TIdTimerQueue          read fTimer write SetTimer;
  end;

  // I manage the sending and receiving of one media stream, as set out by an
  // offer and answer (RFC 3264) of SDP payloads (RFC 2327).
  //
  // My support for hierarchically encoded streams is minimal. You have to
  // figure out what layer a packet belongs to (by using the Binding property
  // of the Notification), and you have to specify what layer to use when
  // sending data by setting the LayerID parameter to the port that layer uses.
  // Each layer runs on its own port (obviously), using its own RTPAbstractPeer
  // (stored in Servers).
  TIdSDPMediaStream = class(TIdSdpBaseMediaStream,
                            IIdRTPDataListener,
                            IIdRTPListener,
                            IIdRTPSendListener)
  private
    fLocalProfile:    TIdRTPProfile;
    fRemoteProfile:   TIdRTPProfile;
    RTPListeners:     TIdNotificationList;
    RTPSendListeners: TIdNotificationList;
    Servers:          TObjectList;

    function  CreateServer: TIdBaseRTPAbstractPeer;
    function  FindServer(LayerID: Cardinal): TIdBaseRTPAbstractPeer;
    procedure InitializeRemoteServers;
    procedure OnNewData(Data: TIdRTPPayload;
                        Binding: TIdConnectionBindings);
    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdConnectionBindings);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdConnectionBindings);
    procedure OnSendRTCP(Packet: TIdRTCPPacket;
                         Binding: TIdConnectionBindings);
    procedure OnSendRTP(Packet: TIdRTPPacket;
                        Binding: TIdConnectionBindings);
    procedure RecreateServers(NumberOfServers: Cardinal);
    procedure RegisterEncodingMaps(Profile: TIdRTPProfile;
                                   Maps: TIdSdpRTPMapAttributes);
    function  ServerAt(Index: Integer): TIdBaseRTPAbstractPeer;
    procedure SetLocalProfile(Value: TIdRTPProfile);
    procedure SetRemoteProfile(Value: TIdRTPProfile);
    procedure UnregisterEncodingMaps(Profile: TIdRTPProfile;
                                     Maps: TIdSdpRTPMapAttributes);
  protected
    procedure AfterSetLocalDescription(Value: TIdSdpMediaDescription); override;
    procedure AfterSetRemoteDescription(Value: TIdSdpMediaDescription); override;
    procedure BeforeSetLocalDescription(Value: TIdSdpMediaDescription); override;
    procedure BeforeSetRemoteDescription(Value: TIdSdpMediaDescription); override;
    function  GetPort(Index: Integer): TPortNum; override;
    procedure InitializeLocalServers; override;
    procedure InternalCreate; override;
    function  NextPort(PreviousPort: TPortNum): TPortNum; override;
    procedure ReallySendData(Data: TStream; Format: String; LayerID: Integer = 0); override;
    function  RequiredPorts: Cardinal; override;
    procedure SetTimer(Value: TIdTimerQueue); override;
    procedure StartServers; override;
  public
    destructor Destroy; override;

    procedure AddRTPListener(const Listener: IIdRTPListener);
    procedure AddRTPSendListener(const Listener: IIdRTPSendListener);
    function  IsListening: Boolean; override;
    procedure JoinSession; override;
    function  MatchPort(Port: TPortNum): Boolean;

    procedure RemoveRTPListener(const Listener: IIdRTPListener);
    procedure RemoveRTPSendListener(const Listener: IIdRTPSendListener);
    procedure StartListening; override;
    procedure StopListening; override;

    property LocalProfile:  TIdRTPProfile read fLocalProfile write SetLocalProfile;
    property RemoteProfile: TIdRTPProfile read fRemoteProfile write SetRemoteProfile;
  end;

  // I represent a stream that's being refused because, for instance, the media
  // description refers to an unknown protocol.
  TIdSdpNullMediaStream = class(TIdSdpBaseMediaStream)
  protected
    procedure AfterSetLocalDescription(Value: TIdSdpMediaDescription); override;
  public
    function IsNull: Boolean; override;
  end;

  TIdSdpBaseTcpConnection = class;
  TIdSdpBaseTcpConnectionClass = class of TIdSdpBaseTcpConnection;

  // OnConnect tells you that a client has connected to this (server)
  // connection, or that this (client) connection has successfully connected to
  // a server connection.
  // OnDisconnect tells you that the remote end terminated the connection, or
  // the network did (pulled-out cable, say).
  IIdSdpTcpConnectionListener = interface
    ['{3CF32F0D-557E-4590-B467-CE99A65C480F}']
    procedure OnConnect(Connection: TIdSdpBaseTcpConnection);
    procedure OnData(Connection: TIdSdpBaseTcpConnection;
                     Data: TStream);
    procedure OnDisconnect(Connection: TIdSdpBaseTcpConnection);
    procedure OnException(Connection: TIdSdpBaseTcpConnection;
                          ExceptionType: ExceptClass;
                          ExceptionMessage: String);
  end;

  // I represent a TCP connection. I have both client and server protocols
  // (ConnectTo and ListenOn, respectively) because, other than how the
  // connection's set up, there's no difference between the two.
  //
  // Server connections only accept one client. You can tell when a client has
  // connected by inspecting the IsConnected property, or by listening for the
  // OnConnected notification in IIdSdpTcpConnectionListener.
  //
  // The Timeout property is measured in milliseconds. Timeout = 0 means "don't
  // wait" and Timeout = -1 means "wait forever".
  TIdSdpBaseTcpConnection = class(TIdRegisteredObject)
  private
    DataListeners: TIdNotificationList;
    fTimeout:      Integer;
    fTimer:        TIdTimerQueue;
  protected
    function  GetAddress: String; virtual;
    function  GetPeerAddress: String; virtual;
    function  GetPeerPort: TPortNum; virtual;
    function  GetPort: TPortNum; virtual;
    procedure NotifyOfConnection;
    procedure NotifyOfDisconnection;
    procedure NotifyOfException(ExceptionType: ExceptClass; ExceptionMessage: String);
    procedure RaiseSocketError(Msg: String);
    procedure ScheduleNotification(WaitTime: Cardinal; Wait: TIdWait);
    procedure SetTimer(Value: TIdTimerQueue); virtual;
  public
    class function ClientConnectionType: TIdSdpBaseTcpConnectionClass; virtual;
    class function NullConnectionType: TIdSdpBaseTcpConnectionClass; virtual;
    class function ServerConnectionType: TIdSdpBaseTcpConnectionClass; virtual;

    constructor Create; override;
    destructor  Destroy; override;

    procedure AddDataListener(Listener: IIdSdpTcpConnectionListener);
    procedure ConnectTo(PeerAddress: String; PeerPort: TPortNum); virtual;
    procedure Disconnect; virtual;
    function  IsActive: Boolean; virtual;
    function  IsConnected: Boolean; virtual;
    function  IsServer: Boolean; virtual;
    procedure ListenOn(Address: String; Port: TPortNum); virtual;
    procedure ReceiveData(Data: TStream; ReceivedOn: TIdConnectionBindings); virtual;
    procedure RemoveDataListener(Listener: IIdSdpTcpConnectionListener);
    procedure SendData(Data: TStream); virtual;

    property Address:     String        read GetAddress;
    property Port:        TPortNum      read GetPort;
    property PeerAddress: String        read GetPeerAddress;
    property PeerPort:    TPortNum      read GetPeerPort;
    property Timeout:     Integer       read fTimeout write fTimeout;
    property Timer:       TIdTimerQueue read fTimer write SetTimer;
  end;

  // I provide a way for tests to reference BaseTcpConnections without exposing
  // the peers directly in owning classes. For instance, TIdSdpTcpMediaStream
  // uses BaseTcpConnections, but we don't want normal code to reference those
  // objects except through TIdSdpTcpMediaStream's methods, but tests must ALSO
  // access the objects in order to, for instance, simulate the receipt of a
  // packet.
  //
  // Please note that I am _NOT_ thread-safe. Using FindConnection will likely
  // cause time-of-use/time-of-check problems in a multithreaded environments.
  TIdSdpTcpConnectionRegistry = class(TObject)
  private
    function GetAllConnections: TStrings;
  public
    class function Singleton: TIdSdpTcpConnectionRegistry;

    function ClientConnectedTo(Host: String; Port: TPortNum): TIdSdpBaseTcpConnection;
    function FindConnection(const ServerID: TRegisteredObjectID): TIdSdpBaseTcpConnection;
    function ServerOn(Host: String; Port: TPortNum): TIdSdpBaseTcpConnection;
    function ServerRunningOn(Host: String; Port: TPortNum): Boolean;
  end;

  // I represent a real TCP connection, but one that's a "holdconn" stream - a
  // placeholder for a future connection.
  TIdSdpTcpNullConnection = class(TIdSdpBaseTcpConnection)
  protected
    function  GetAddress: String; override;
    function  GetPeerAddress: String; override;
    function  GetPeerPort: TPortNum; override;
    function  GetPort: TPortNum; override;
  public
    procedure ConnectTo(PeerAddress: String; PeerPort: TPortNum); override;
    procedure SendData(Data: TStream); override;
    procedure Disconnect; override;
    function  IsActive: Boolean; override;
    function  IsConnected: Boolean; override;
    procedure ListenOn(Address: String; Port: TPortNum); override;
    procedure ReceiveData(Data: TStream; ReceivedOn: TIdConnectionBindings); override;
  end;

  TIdNewDataEvent = procedure(Data: TStream; ReceivedOn: TIdConnectionBindings) of object;

  TIdSdpTcpClientConnection = class;

  TIdSdpDisconnectionProc = procedure(Sender: TObject; ConnectionID: TRegisteredObjectID) of object;

  TIdSdpTcpClient = class(TIdThreadableTcpClient)
  private
    BufferSize:       Integer;
    fConnectionID:    TRegisteredObjectID;
    fOnDisconnection: TIdSdpDisconnectionProc;

    procedure NotifyOfDisconnection(Sender: TObject; ConnectionID: TRegisteredObjectID);
  protected
    function  GetTimer: TIdTimerQueue; override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ReceiveMessages; override;

    property ConnectionID:    TRegisteredObjectID     read fConnectionID write fConnectionID;
    property OnDisconnection: TIdSdpDisconnectionProc read fOnDisconnection write fOnDisconnection;
  end;

  TIdSdpThreadedTcpClient = class(TIdThreadedTcpClient)
  private
    ConnectionLock: TCriticalSection;

    procedure DisconnectClient(C: TIdThreadableTcpClient;
                               L: TCriticalSection);
    procedure FreeClient(C: TIdThreadableTcpClient;
                         L: TCriticalSection);
  protected
    function  GetTimer: TIdTimerQueue; override;
    procedure Run; override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    constructor Create(Connection: TIdThreadableTcpClient;
                       ConnectionLock: TCriticalSection); reintroduce;
  end;

  TIdSdpTcpClientConnection = class(TIdSdpBaseTcpConnection)
  private
    Client:       TIdSdpTcpClient;
    ClientThread: TIdSdpThreadedTcpClient;
    ThreadLock:   TCriticalSection;

    procedure ClientConnected(Client: TObject);
    procedure ClientDisconnected(Client: TObject);
    procedure ClientDisconnection(Sender: TObject; ConnectionID: TRegisteredObjectID);
    function  CreateClient: TIdSdpTcpClient;
    procedure ReceiveMessage(Sender: TObject; Msg: String; ReceivedOn: TIdConnectionBindings);
  protected
    function  GetAddress: String; override;
    function  GetPeerAddress: String; override;
    function  GetPeerPort: TPortNum; override;
    function  GetPort: TPortNum; override;
    procedure SetTimer(Value: TIdTimerQueue); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure ConnectTo(PeerAddress: String; PeerPort: TPortNum); override;
    procedure Disconnect; override;
    function  IsActive: Boolean; override;
    function  IsConnected: Boolean; override;
    procedure ListenOn(Address: String; Port: TPortNum); override;
    procedure SendData(Data: TStream); override;
  end;

  // I represent a connection initiated by a remote party. I notify my listeners
  // of connections, disconnections and received data in the context of my
  // TimerQueue.
  TIdSdpTcpServerConnection = class(TIdSdpBaseTcpConnection)
  private
    Connection: TIdTcpServer;

    procedure ClientConnected(Thread: TIdPeerThread);
    procedure ClientDisconnected(Thread: TIdPeerThread);
    procedure NotifyOfConnectionInTimerContext;
    procedure NotifyOfDisconnectionInTimerContext;
    procedure ReadData(Thread: TIdPeerThread);
  protected
    function  GetAddress: String; override;
    function  GetPeerAddress: String; override;
    function  GetPeerPort: TPortNum; override;
    function  GetPort: TPortNum; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure ConnectTo(PeerAddress: String; PeerPort: TPortNum); override;
    procedure Disconnect; override;
    function  IsActive: Boolean; override;
    function  IsConnected: Boolean; override;
    function  IsServer: Boolean; override;
    procedure ListenOn(Address: String; Port: TPortNum); override;
    procedure SendData(Data: TStream); override;
  end;

  TIdSdpMockTcpConnection = class(TIdSdpBaseTcpConnection)
  private
    fAddress:          String;
    fConnectToCalled:  Boolean;
    fIsActive:         Boolean;
    fIsConnected:      Boolean;
    fIsServer:         Boolean;
    fListenOnCalled:   Boolean;
    fPeerAddress:      String;
    fPeerPort:         TPortNum;
    fPort:             TPortNum;
    fReceivedData:     String;
    fSentData:         String;
  protected
    function GetAddress: String; override;
    function GetPeerAddress: String; override;
    function GetPeerPort: TPortNum; override;
    function GetPort: TPortNum; override;
  public
    class function ClientConnectionType: TIdSdpBaseTcpConnectionClass; override;
    class function NullConnectionType: TIdSdpBaseTcpConnectionClass; override;
    class function ServerConnectionType: TIdSdpBaseTcpConnectionClass; override;

    constructor Create; override;

    procedure Disconnect; override;
    procedure ForceDisconnect; virtual;
    function  IsActive: Boolean; override;
    function  IsConnected: Boolean; override;
    function  IsServer: Boolean; override;
    procedure RemotePartyAccepts; virtual;
    procedure ReceiveData(Data: TStream; ReceivedOn: TIdConnectionBindings); override;
    procedure SendData(Data: TStream); override;

    property ConnectToCalled: Boolean  read fConnectToCalled;
    property ListenOnCalled:  Boolean  read fListenOnCalled;
    property ReceivedData:    String   read fReceivedData;
    property SentData:        String   read fSentData;
  end;

  TIdSdpMockTcpNullConnection = class(TIdSdpMockTcpConnection)
  public
    procedure ConnectTo(PeerAddress: String; PeerPort: TPortNum); override;
    procedure ForceDisconnect; override;
    procedure ListenOn(Address: String; Port: TPortNum); override;
    procedure ReceiveData(Data: TStream; ReceivedOn: TIdConnectionBindings); override;
    procedure RemotePartyAccepts; override;
  end;

  TIdSdpMockTcpClientConnection = class(TIdSdpMockTcpConnection)
  private
    function EphemeralPort: TPortNum;
  public
    procedure ConnectTo(PeerAddress: String; PeerPort: TPortNum); override;
    function  IsServer: Boolean; override;
    procedure ListenOn(Address: String; Port: TPortNum); override;
  end;

  TIdSdpMockTcpServerConnection = class(TIdSdpMockTcpConnection)
  public
    procedure ConnectTo(PeerAddress: String; PeerPort: TPortNum); override;
    function  IsServer: Boolean; override;
    procedure ListenOn(Address: String; Port: TPortNum); override;
  end;

  TConnectionTypeArray = array[stActive..stUnknown, stActive..stUnknown] of TIdSdpBaseTcpConnectionClass;

  // I implement the streams defined by RFC 4145 "TCP-Based Media Transport in
  // the Session Description Protocol (SDP)".
  //
  // Note that if this stream uses multiple formats, the data notification will
  // notify having received data of the FIRST format ONLY. Listeners will have
  // to reassemble the stream and demultiplex the stream themselves.
  TIdSdpTcpMediaStream = class(TIdSdpBaseMediaStream,
                               IIdSdpTcpConnectionListener)
  private
    AnswerConnectionType:  TConnectionTypeArray;
    OfferConnectionType:   TConnectionTypeArray;
    HaveLocalDescription:  Boolean;
    HaveRemoteDescription: Boolean;
    Servers:               TObjectList;

    function  CreateServer(OfferSetupType, AnswerSetupType: TIdSdpSetupType): TIdSdpBaseTcpConnection;
    function  DefaultSetupType(ForOffer: Boolean): TIdSdpSetupType;
    function  FindServer(LayerID: Cardinal): TIdSdpBaseTcpConnection;
    function  HaveCompleteDescription: Boolean;
    function  HaveCorrectConnections(NumberOfServers: Integer): Boolean;
    procedure InitializeConnectionTypeTable(BaseServerType: TIdSdpBaseTcpConnectionClass);
    procedure InitializeRemoteServers;
    procedure OnConnect(Connection: TIdSdpBaseTcpConnection);
    procedure OnData(Connection: TIdSdpBaseTcpConnection; Data: TStream);
    procedure OnDisconnect(Connection: TIdSdpBaseTcpConnection);
    procedure OnException(Connection: TIdSdpBaseTcpConnection;
                          ExceptionType: ExceptClass;
                          ExceptionMessage: String);
    procedure RecreateServers(NumberOfServers: Cardinal);
    procedure ResetHaveDescriptionFlags;
    function  ServerAt(Index: Integer): TIdSdpBaseTcpConnection;
    function  LocalSetupType: TIdSdpSetupType;
    procedure PossiblyConnect(HaveCompleteSessionDescription: Boolean);
    function  RemoteSetupType: TIdSdpSetupType;
    function  ServerType(IsOffer: Boolean; OfferSetupType, AnswerSetupType: TIdSdpSetupType): TIdSdpBaseTcpConnectionClass;
    procedure StartConnectingStreams;
    function  UsesServerSockets: Boolean;
  protected
    procedure AfterSetLocalDescription(Value: TIdSdpMediaDescription); override;
    procedure AfterSetRemoteDescription(Value: TIdSdpMediaDescription); override;
    function  GetPort(Index: Integer): TPortNum; override;
    procedure InitializeLocalServers; override;
    procedure InternalCreate; override;
    procedure ReallySendData(Data: TStream; Format: String; LayerID: Integer = 0); override;
    procedure SetTimer(Value: TIdTimerQueue); override;
    procedure StartServers; override;
  public
    destructor Destroy; override;

    function  IsActiveConnection: Boolean;
    function  IsListening: Boolean; override;
    procedure StartListening; override;
    procedure StopListening; override;
  end;

  TIdSdpMediaStreamFactory = class(TObject)
  protected
    fTcpServerType: TIdSdpBaseTcpConnectionClass;
    fRtpServerType: TIdBaseRTPAbstractPeerClass;
  public
    constructor Create; virtual;

    function CreateStream(Protocol: String): TIdSdpBaseMediaStream; virtual;

    property TcpServerType: TIdSdpBaseTcpConnectionClass read fTcpServerType;
    property RtpServerType: TIdBaseRTPAbstractPeerClass  read fRtpServerType;
  end;

  // I process SDP (RFC 2327) payloads. This means that I instantiate (RTP)
  // servers on appropriate ports based on a local session description.
  // You can give me a remote session description too, which allows you to
  // use me to send (RTP) data to the remote peer.
  TIdSDPMultimediaSession = class(TObject)
  private
    fHighestAllowedPort:  TPortNum;
    Factory:              TIdSdpMediaStreamFactory;
    FirstLocalSessDesc:   Boolean;
    fIsOffer:             Boolean;
    fLocalMachineName:    String;
    fLowestAllowedPort:   TPortNum;
    fLocalSessionID:      String;
    fLocalSessionName:    String;
    fLocalSessionVersion: Int64;
    fOnHold:              Boolean;
    fStreams:             TObjectList;
    fUsername:            String;
    StreamLock:           TCriticalSection;
    TimeHeader:           TIdSdpTime;
    Timer:                TIdTimerQueue;

    procedure ClearStreams;
    function  CreateStream(Protocol: String): TIdSDPBaseMediaStream;
    procedure InternalCreate(Profile: TIdRTPProfile);
    procedure SetIsOffer(Value: Boolean);
    function  GetStreams(Index: Integer): TIdSdpBaseMediaStream;
    procedure RecreateStreams(LocalDescription: TIdSdpPayload);
    procedure SetHighestAllowedPort(Value: TPortNum);
    procedure SetLocalMachineName(Value: String);
    procedure SetLowestAllowedPort(Value: TPortNum);
    procedure UpdateSessionVersion;
  public
    constructor Create(Profile: TIdRTPProfile; Factory: TIdSdpMediaStreamFactory; ExecutionContext: TIdTimerQueue);
    destructor  Destroy; override;

    function  AddressTypeFor(Address: String): TIdIPVersion;
    function  IndexOfStream(S: TIdSdpBaseMediaStream): Integer;
    function  IsListening: Boolean;
    procedure JoinSession;
    function  LocalSessionDescription: String;
    function  LocalSessionVersion: Int64;
    function  MimeType: String;
    function  NetTypeFor(Address: String): String;
    procedure PutOnHold;
    procedure SetRemoteDescription(RemoteSessionDesc: String); overload;
    procedure SetRemoteDescription(RemoteSessionDesc: TIdSdpPayload); overload;
    function  StartListening(LocalSessionDesc: String): String; overload;
    function  StartListening(LocalSessionDesc: TIdSdpPayload): String; overload;
    procedure StopListening;
    function  StreamCount: Integer;
    procedure TakeOffHold;

    property HighestAllowedPort:      TPortNum              read fHighestAllowedPort write SetHighestAllowedPort;
    property IsOffer:                 Boolean               read fIsOffer write SetIsOffer;
    property LocalMachineName:        String                read fLocalMachineName write SetLocalMachineName;
    property LocalSessionID:          String                read fLocalSessionID write fLocalSessionID;
    property LowestAllowedPort:       TPortNum              read fLowestAllowedPort write SetLowestAllowedPort;
    property OnHold:                  Boolean               read fOnHold;
    property LocalSessionName:        String                read fLocalSessionName write fLocalSessionName;
    property Streams[Index: Integer]: TIdSdpBaseMediaStream read GetStreams;
    property Username:                String                read fUsername write fUsername;
  end;

  TIdSdpMediaListener = class(TIdNotification)
  private
    fChunk:   TStream;
    fFormat:  String;
    fStream:  TIdSdpBaseMediaStream;

    procedure SetChunk(Value: TStream);
  public
    constructor Create;
    destructor  Destroy; override;

    property Chunk:   TStream               read fChunk write SetChunk;
    property Format:  String                read fFormat write fFormat;
    property Stream:  TIdSdpBaseMediaStream read fStream write fStream;
  end;

  TIdSdpTcpConnectionWait = class(TIdWait)
  private
    fConnectionID: TRegisteredObjectID;

    procedure TriggerClosure(O: TObject);
  protected
    procedure ActOnTrigger(C: TIdSdpBaseTcpConnection); virtual;
  public
    procedure Trigger; override;
    property ConnectionID: TRegisteredObjectID read fConnectionID write fConnectionID;
  end;

  TIdSdpTcpConnectionConnectedWait = class(TIdSdpTcpConnectionWait)
  protected
    procedure ActOnTrigger(C: TIdSdpBaseTcpConnection); override;
  end;

  TIdSdpTcpConnectionDisconnectedWait = class(TIdSdpTcpConnectionWait)
  protected
    procedure ActOnTrigger(C: TIdSdpBaseTcpConnection); override;
  end;

  TIdSdpTcpConnectionExceptionWait = class(TIdSdpTcpConnectionWait)
  private
    fExceptionMessage: String;
    fExceptionType:    ExceptClass;
  protected
    procedure ActOnTrigger(C: TIdSdpBaseTcpConnection); override;
  public
    property ExceptionMessage: String      read fExceptionMessage write fExceptionMessage;
    property ExceptionType:    ExceptClass read fExceptionType write fExceptionType;
  end;

  TIdSdpTcpReceiveDataWait = class(TIdSdpTcpConnectionWait)
  private
    fData:       TStream;
    fReceivedOn: TIdConnectionBindings;

    procedure SetData(Value: TStream);
    procedure SetReceivedOn(Value: TIdConnectionBindings);
  protected
    procedure ActOnTrigger(C: TIdSdpBaseTcpConnection); override;
    procedure LogTrigger; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Data:       TStream               read fData write SetData;
    property ReceivedOn: TIdConnectionBindings read fReceivedOn write SetReceivedOn;
  end;

  TIdSdpTcpSendDataWait = class(TIdSdpTcpConnectionWait)
  private
    fData: TStream;

    procedure SetData(Value: TStream);
  protected
    procedure ActOnTrigger(C: TIdSdpBaseTcpConnection); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Data: TStream read fData write SetData;
  end;

  TIdSdpMediaListenerOnDataMethod = class(TIdSdpMediaListener)
  private
    fBinding: TIdConnectionBindings;

    procedure SetBinding(Value: TIdConnectionBindings);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Run(const Subject: IInterface); override;

    property Binding: TIdConnectionBindings read fBinding write SetBinding;
  end;

  TIdSdpMediaListenerOnSentDataMethod = class(TIdSdpMediaListener)
  private
    fLayerID: Integer;
  public
    procedure Run(const Subject: IInterface); override;

    property LayerID: Integer read fLayerID write fLayerID;
  end;

  TIdSdpTcpConnectionNotification = class(TIdNotification)
  private
    fConnection: TIdSdpBaseTcpConnection;
  public
    property Connection: TIdSdpBaseTcpConnection read fConnection write fConnection;
  end;

  TIdSdpTcpConnectionOnConnectMethod = class(TIdSdpTcpConnectionNotification)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSdpTcpConnectionOnDataMethod = class(TIdSdpTcpConnectionNotification)
  private
    fData: TStream;

    procedure SetData(Value: TStream);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Run(const Subject: IInterface); override;

    property Data: TStream read fData write SetData;
  end;

  TIdSdpTcpConnectionOnDisconnectMethod = class(TIdSdpTcpConnectionNotification)
  public
    procedure Run(const Subject: IInterface); override;
  end;

  TIdSdpTcpConnectionOnExceptionMethod = class(TIdSdpTcpConnectionNotification)
  private
    fExceptionMessage: String;
    fExceptionType:    ExceptClass;
  public
    procedure Run(const Subject: IInterface); override;

    property ExceptionMessage: String      read fExceptionMessage write fExceptionMessage;
    property ExceptionType:    ExceptClass read fExceptionType write fExceptionType;
  end;

  ESdpException = class(Exception);

const
  BadHeaderOrder        = 'Headers in the wrong order: found %s after %s';
  ConvertEnumErrorMsg   = 'Couldn''t convert a %s with Ord() = %d to type %s';
  ConvertStrErrorMsg    = 'Couldn''t convert ''%s'' to type %s';
  MissingConnection     = 'Missing connection-field';
  MissingOrigin         = 'Missing origin-field';
  MissingSessionName    = 'Missing session-name-field';
  MissingVersion        = 'Missing proto-version';
  TooManyHeaders        = 'Header ''%s'' occured multiple times';
  UnknownOptionalHeader = 'Unknown optional header: ''%s''';

// Grammar definitions. Don't localise them.
const
  AlphanumericChars = Alphabet + Digits;
  SafeChars = AlphanumericChars + ['''', '-', '.', '/', ':', '?', '#',
               '$', '&', '*', ';', '=', '@', '[', ']', '^', '_', '`', '{', '|',
               '}', '+', '~', '"'];
  EmailSafeChars = SafeChars + [' ', #9];
  IllegalByteStringChars = [#0, #10, #13];
  TimeTypes          = ['d', 'h', 'm', 's'];
//  TokenChars         = ['!', '#', '$', '%', '&', '''', '*', '+', '-', '.', '^',
//                        '_', '`', '{', '|', '}', '~'] + AlphanumericChars;
  TokenChars = [#$21, #$23..#$27, #$2a..#$2b, #$2d..#$2e, #$30..#$39, #$41..#$5a, #$5e..#$7e]; // RFC 4566
  AllTokenChars = '!#$%&''*+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ^_`abcdefhijklmnopqrstuvwxyz{|}~';

// MIME types etc
const
  PlainTextMimeType = 'text/plain';
  SdpMimeType       = 'application/sdp';

// for IdAssignedNumbers
const
  // IANA assigned bwtype
  Id_SDP_ConferenceTotal     = 'CT';
  Id_SDP_ApplicationSpecific = 'AS';
  Id_SDP_RS                  = 'RS';
  Id_SDP_RR                  = 'RR';
  Id_SDP_Unknown             = ''; // not IANA assigned!
  // IANA assigned nettype
  Id_SDP_IN = 'IN';
  // IANA assigned addrtype
  Id_SDP_IP4       = 'IP4';
  Id_SDP_IP6       = 'IP6';
  Id_SDP_IPUnknown = 'UNKNOWN_IP_VERSION'; // NOT IANA assigned!
  // IANA assigned keytype
  Id_SDP_Clear   = 'clear';
  Id_SDP_Base64  = 'base64';
  Id_SDP_URI     = 'uri';
  Id_SDP_Prompt  = 'prompt';
  // IANA assigned protos
  Id_SDP_RTPAVP  = 'RTP/AVP'; // RFC 3551
  Id_SDP_RTPSAVP = 'RTP/SAVP'; // RFC 3711
  Id_SDP_udp     = 'udp'; // RFC 2327
  Id_SDP_TCP     = 'TCP'; // RFC 4145

  RSSDPSetupActive        = 'active';   // RFC 4145
  RSSDPSetupActPass       = 'actpass';  // RFC 4145
  RSSDPConnectionExisting = 'existing'; // RFC 4145
  RSSDPSetupHoldConn      = 'holdconn'; // RFC 4145
  RSSDPConnectionNew      = 'new';      // RFC 4145
  RSSDPSetupPassive       = 'passive';  // RFC 4145

const
  // Attribute names
  ConnectionAttribute = 'connection'; // RFC 4145
  RTPMapAttribute     = 'rtpmap';
  SetupAttribute      = 'setup';      // RFC 4145

  // SDP header names
  RSSDPAttributeName         = 'a';
  RSSDPBandwidthName         = 'b';
  RSSDPConnectionName        = 'c';
  RSSDPEmailName             = 'e';
  RSSDPOriginName            = 'o';
  RSSDPInformationName       = 'i';
  RSSDPKeyName               = 'k';
  RSSDPMediaDescriptionName  = 'm';
  RSSDPPhoneName             = 'p';
  RSSDPRepeatName            = 'r';
  RSSDPSessionName           = 's';
  RSSDPTimeName              = 't';
  RSSDPUriName               = 'u';
  RSSDPVersionName           = 'v';
  RSSDPZoneAdjustmentName    = 'z';

  RSSDPMediaTypeAudio        = 'audio';
  RSSDPMediaTypeVideo        = 'video';
  RSSDPMediaTypeApplication  = 'application';
  RSSDPMediaTypeData         = 'data';
  RSSDPMediaTypeControl      = 'control';
  RSSDPMediaTypeText         = 'text';

  RSSDPDirectionInactive = 'inactive'; // RFC 3264
  RSSDPDirectionRecvOnly = 'recvonly'; // RFC 3264
  RSSDPDirectionSendOnly = 'sendonly'; // RFC 3264
  RSSDPDirectionSendRecv = 'sendrecv'; // RFC 3264

// Transport-specific constants, and similar.
const
  RefusedPort    = 0;
  TcpDiscardPort = 9;

const
  BlankSessionName    = '-';
  BlankUsername       = '-';
  FiveSeconds         = 5000;
  HighestPossiblePort = 65535;
  ItemNotFoundIndex   = -1;
  LowestPossiblePort  = 0;
  IPv4ZeroAddress     = '0.0.0.0';

function AddressTypeToStr(Version: TIdIPVersion): String;
function AppropriateSetupType(Offer: TIdSdpSetupType; EstablishStream, PreferActive: Boolean): TIdSdpSetupType;
function BandwidthTypeToStr(BwType: TIdSdpBandwidthType): String;
function ConnectionTypeToStr(ConnType: TIdSdpConnectionType): String;
function DirectionToStr(Direction: TIdSdpDirection): String;
function KeyTypeToStr(KeyType: TIdSdpKeyType): String;
function MediaTypeToStr(MediaType: TIdSdpMediaType): String;
function SetupTypeToStr(SetupType: TIdSdpSetupType): String;
function StrToAddressType(const S: String): TIdIPVersion;
function StrToBandwidthType(const S: String): TIdSdpBandwidthType;
function StrToConnectionType(const S: String): TIdSdpConnectionType;
function StrToDirection(const S: String): TIdSdpDirection;
function StrToKeyType(const S: String): TIdSDPKeyType;
function StrToMediaType(const S: String): TIdSDPMediaType;
function StrToSetupType(const S: String): TIdSDPSetupType;

implementation

uses
  IdException, IdIndyUtils, IdRandom, IdSocketHandle, IdTCPConnection,
  PluggableLogging, RuntimeSafety;

const
  SessionHeaderOrder = 'vosiuepcbtka';
  MediaHeaderOrder   = 'micbka';

const
  OneSecond = 1000; // milliseconds

var
  GSdpTcpConnectionRegistry: TIdSdpTcpConnectionRegistry;  

//******************************************************************************
//* Unit public functions and procedures                                       *
//******************************************************************************

function AddressTypeToStr(Version: TIdIPVersion): String;
begin
  case Version of
    Id_IPv4:      Result := Id_SDP_IP4;
    Id_IPv6:      Result := Id_SDP_IP6;
    Id_IPUnknown: Result := Id_SDP_IPUnknown;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdIPVersion',
                                       Ord(Version),
                                       'String']));
  end;
end;

function AppropriateSetupType(Offer: TIdSdpSetupType; EstablishStream, PreferActive: Boolean): TIdSdpSetupType;
begin
  // Given an Offer of a setup type, what would be the appropriate answering setup type?
  // * If we want to actually establish the stream, set EstablishStream to true.
  // * If the offer is "actpass" (in which case we get to decide), do we want to
  //   initiate the TCP connection? Set PreferActive to true to initiate the
  //   connection.

  if not EstablishStream then begin
    Result := stHoldConn;
    Exit;
  end;

  case Offer of
    stActive:  Result := stPassive;
    stPassive: Result := stActive;
    stActPass: if PreferActive then
                 Result := stActive
               else
                 Result := stPassive;
  else
    Result := stHoldConn;
  end;
end;

function DirectionToStr(Direction: TIdSdpDirection): String;
begin
  case Direction of
    sdInactive: Result := RSSDPDirectionInactive;
    sdRecvOnly: Result := RSSDPDirectionRecvOnly;
    sdSendOnly: Result := RSSDPDirectionSendOnly;
    sdSendRecv: Result := RSSDPDirectionSendRecv;
    sdUnknown:  Result := Id_SDP_Unknown;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdSdpDirection',
                                       Ord(Direction),
                                       'String']));
  end;
end;

function BandwidthTypeToStr(BwType: TIdSdpBandwidthType): String;
begin
  case BwType of
    btConferenceTotal:     Result := Id_SDP_ConferenceTotal;
    btApplicationSpecific: Result := Id_SDP_ApplicationSpecific;
    btRS:                  Result := Id_SDP_RS;
    btRR:                  Result := Id_SDP_RR;
    btUnknown:             Result := Id_SDP_Unknown;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdSdpBandwidthType',
                                       Ord(BwType),
                                       'String']));
  end;
end;

function ConnectionTypeToStr(ConnType: TIdSdpConnectionType): String;
begin
  case ConnType of
    ctExisting: Result := RSSDPConnectionExisting;
    ctNew:      Result := RSSDPConnectionNew;
    ctUnknown:  Result := Id_SDP_Unknown;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdSdpConnectionType',
                                       Ord(ConnType),
                                       'String']));
  end;
end;

function KeyTypeToStr(KeyType: TIdSdpKeyType): String;
begin
  case KeyType of
    ktClear:   Result := Id_SDP_Clear;
    ktBase64:  Result := Id_SDP_Base64;
    ktURI:     Result := Id_SDP_URI;
    ktPrompt:  Result := Id_SDP_Prompt;
    ktUnknown: Result := Id_SDP_Unknown;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdSdpKeyType',
                                       Ord(KeyType),
                                       'String']));
  end;
end;

function MediaTypeToStr(MediaType: TIdSdpMediaType): String;
begin
  case MediaType of
    mtAudio:       Result := RSSDPMediaTypeAudio;
    mtVideo:       Result := RSSDPMediaTypeVideo;
    mtApplication: Result := RSSDPMediaTypeApplication;
    mtData:        Result := RSSDPMediaTypeData;
    mtControl:     Result := RSSDPMediaTypeControl;
    mtText:        Result := RSSDPMediaTypeText;
    mtUnknown:     Result := Id_SDP_Unknown;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdSdpMediaType',
                                       Ord(MediaType),
                                       'String']));
  end;
end;

function SetupTypeToStr(SetupType: TIdSdpSetupType): String;
begin
  case SetupType of
    stActive:   Result := RSSDPSetupActive;
    stActPass:  Result := RSSDPSetupActPass;
    stHoldConn: Result := RSSDPSetupHoldConn;
    stPassive:  Result := RSSDPSetupPassive;
    stUnknown:  Result := Id_SDP_Unknown;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdSdpSetupType',
                                       Ord(SetupType),
                                       'String']));
  end;
end;

function StrToAddressType(const S: String): TIdIPVersion;
begin
  if (Trim(S) = '') then
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdIPVersion']));

  if (S = Id_SDP_IP4) then
    Result := Id_IPv4
  else if (S = Id_SDP_IP6) then
    Result := Id_IPv6
  else
    Result := Id_IPUnknown;
end;

function StrToBandwidthType(const S: String): TIdSdpBandwidthType;
begin
  if not TIdSdpParser.IsBandwidthType(S) then
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdSdpBandwidthType']));

       if (S = Id_SDP_ConferenceTotal)     then Result := btConferenceTotal
  else if (S = Id_SDP_ApplicationSpecific) then Result := btApplicationSpecific
  else if (S = Id_SDP_RS)                  then Result := btRS
  else if (S = Id_SDP_RR)                  then Result := btRR
  else
    Result := btUnknown;
end;

function StrToConnectionType(const S: String): TIdSdpConnectionType;
begin
  if not TIdSdpParser.IsToken(S) then
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdSdpConnectionType']));

       if (S = RSSDPConnectionExisting) then Result := ctExisting
  else if (S = RSSDPConnectionNew)      then Result := ctNew
  else
    Result := ctUnknown;
end;

function StrToDirection(const S: String): TIdSdpDirection;
begin
  if (Trim(S) = '') then
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdSdpDirection']));

       if (S = RSSDPDirectionInactive) then Result := sdInactive
  else if (S = RSSDPDirectionRecvOnly) then Result := sdRecvOnly
  else if (S = RSSDPDirectionSendOnly) then Result := sdSendOnly
  else if (S = RSSDPDirectionSendRecv) then Result := sdSendRecv
  else
    Result := sdUnknown;
end;

function StrToKeyType(const S: String): TIdSDPKeyType;
begin
  if not TIdSdpParser.IsKeyType(S) then
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdSdpKeyType']));

       if (S = Id_SDP_Clear)  then Result := ktClear
  else if (S = Id_SDP_Base64) then Result := ktBase64
  else if (S = Id_SDP_URI)    then Result := ktURI
  else if (S = Id_SDP_Prompt) then Result := ktPrompt
  else
    Result := ktUnknown;
end;

function StrToMediaType(const S: String): TIdSDPMediaType;
begin
  if not TIdSdpParser.IsMediaType(S) then
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdSdpMediaType']));

       if (S = RSSDPMediaTypeAudio)       then Result := mtAudio
  else if (S = RSSDPMediaTypeVideo)       then Result := mtVideo
  else if (S = RSSDPMediaTypeApplication) then Result := mtApplication
  else if (S = RSSDPMediaTypeData)        then Result := mtData
  else if (S = RSSDPMediaTypeControl)     then Result := mtControl
  else if (S = RSSDPMediaTypeText)        then Result := mtText
  else
    Result := mtUnknown;
end;

function StrToSetupType(const S: String): TIdSDPSetupType;
begin
  // This is more generous than RFC 4145, which only allows "active", "actpass",
  // "holdconn" and "passive". We seek to protect ourselves against malicious
  // or careless UAs though.
  if not TIdSdpParser.IsToken(S) then
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdSdpSetupType']));

       if (S = RSSDPSetupActive)   then Result := stActive
  else if (S = RSSDPSetupActPass)  then Result := stActPass
  else if (S = RSSDPSetupHoldConn) then Result := stHoldConn
  else if (S = RSSDPSetupPassive)  then Result := stPassive
  else
    Result := stUnknown;
end;

//******************************************************************************
//* TIdPrintable                                                               *
//******************************************************************************
//* TIdPrintable Public methods ************************************************

constructor TIdPrintable.Create;
begin
  inherited Create;
end;

function TIdPrintable.AsString: String;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.PrintOn(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

procedure TIdPrintable.PrintOn(Dest: TStream);
begin
  RaiseAbstractError(Self.ClassName, 'PrintOn');
end;

//******************************************************************************
//* TIdSdpAttribute                                                            *
//******************************************************************************
//* TIdSdpAttribute Public methods *********************************************

class function TIdSdpAttribute.CreateAttribute(Value: String): TIdSdpAttribute;
var
  Name: String;
begin
  Name := Fetch(Value, ':');

  if (Name = RTPMapAttribute) then
    Result := TIdSdpRTPMapAttribute.Create
  else
    Result := TIdSdpAttribute.Create;

  if (Value = '') then begin
    Result.Name  := Name;
    Result.Value := '';
  end
  else begin
    Result.Name  := Name;
    Result.Value := Value;
  end;
end;

constructor TIdSdpAttribute.Create;
begin
  inherited Create;
end;

procedure TIdSdpAttribute.Assign(Src: TPersistent);
var
  Other: TIdSdpAttribute;
begin
  if (Src is TIdSdpAttribute) then begin
    Other := Src as TIdSdpAttribute;

    Self.Name  := Other.Name;
    Self.Value := Other.Value;
  end
  else inherited Assign(Src);
end;

function TIdSdpAttribute.Copy: TIdSdpAttribute;
begin
  Result := TIdSdpAttributeClass(Self.ClassType).Create;
  try
    Result.Assign(Self);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSdpAttribute.Equals(Other: TIdSdpAttribute): Boolean;
begin
  Result := (Self.Name = Other.Name) and (Self.Value = Other.Value) 
end;

function TIdSdpAttribute.IsRTPMap: Boolean;
begin
  Result := false;
end;

procedure TIdSdpAttribute.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := 'a=' + Self.Name;

  if (Self.Value <> '') then
    S := S + ':' + Self.Value;

  S := S + #13#10;

  Dest.Write(PChar(S)^, Length(S));
end;

//* TIdSdpAttribute Private methods ********************************************

function TIdSdpAttribute.GetName: String;
begin
  Result := fName;
end;

function TIdSdpAttribute.GetValue: String;
begin
  Result := fValue;
end;

procedure TIdSdpAttribute.SetValue(const Value: String);
begin
  fValue := Value;
end;

//******************************************************************************
//* TIdSdpRTPMapAttribute                                                      *
//******************************************************************************
//* TIdSdpRTPMapAttribute Public methods ***************************************

constructor TIdSdpRTPMapAttribute.Create;
begin
  inherited Create;

  Self.fEncoding := TIdNullPayload.Create;
end;

destructor TIdSdpRTPMapAttribute.Destroy;
begin
  Self.Encoding.Free;

  inherited Destroy;
end;

function TIdSdpRTPMapAttribute.IsRTPMap: Boolean;
begin
  Result := true;
end;

//* TIdSdpRTPMapAttribute Protected methods ************************************

function TIdSdpRTPMapAttribute.GetName: String;
begin
  Result := RTPMapAttribute;
end;

function TIdSdpRTPMapAttribute.GetValue: String;
begin
  Result := Self.Format + ' ' + Self.Encoding.EncodingName;
end;

procedure TIdSdpRTPMapAttribute.SetValue(const Value: String);
var
  EncodingDesc: String;
begin
  // cf RFC 2327 page 21:
  // a=rtpmap:<payload type> <encoding name>/<clock rate>[/<encoding
  //   parameters>]
  inherited SetValue(Value);

  EncodingDesc := Value;
  Self.Format := Fetch(EncodingDesc, ' ');

  Self.SetEncoding(TIdRTPPayload.CreatePayload(EncodingDesc));
end;

//* TIdSdpRTPMapAttribute Private methods **************************************

procedure TIdSdpRTPMapAttribute.SetEncoding(Value: TIdRTPPayload);
begin
  Self.Encoding.Free;
  Self.fEncoding := Value;
end;

//******************************************************************************
//* TIdSdpBandwidth                                                            *
//******************************************************************************
//* TIdSdpBandwidth Public methods *********************************************

procedure TIdSdpBandwidth.Assign(Src: TPersistent);
var
  Other: TIdSdpBandwidth;
begin
  if (Src is TIdSdpBandwidth) then begin
    Other := Src as TIdSdpBandwidth;

    Self.Bandwidth     := Other.Bandwidth;
    Self.BandwidthName := Other.BandwidthName;
    Self.BandwidthType := Other.BandwidthType;
  end
  else inherited Assign(Src);
end;

procedure TIdSdpBandwidth.PrintOn(Dest: TStream);
var
  BName: String;
  S:     String;
begin
  if (Self.BandwidthType <> btUnknown) then
    BName := BandwidthTypeToStr(Self.BandwidthType)
  else
    BName := Self.BandwidthName;

  S := 'b=' + BName + ':'
            + IntToStr(Self.Bandwidth) + #13#10;

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpConnection                                                           *
//******************************************************************************
//* TIdSdpConnection Public methods ********************************************

procedure TIdSdpConnection.Assign(Src: TPersistent);
var
  Other: TIdSdpConnection;
begin
  if Src is TIdSdpConnection then begin
    Other := Src as TIdSdpConnection;

    Self.AddressType       := Other.AddressType;
    Self.Address           := Other.Address;
    Self.NetType           := Other.NetType;
    Self.NumberOfAddresses := Other.NumberOfAddresses;
    Self.RoutableAddress   := Other.RoutableAddress;
    Self.TTL               := Other.TTL;
  end
  else inherited Assign(Src);
end;

function TIdSdpConnection.Copy: TIdSdpConnection;
begin
  Result := TIdSdpConnection.Create;
  try
    Result.Assign(Self);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

procedure TIdSdpConnection.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := S + 'c=' + Self.NetType + ' '
         + AddressTypeToStr(Self.AddressType)
         + ' ' + Self.RoutableAddress;

  if (Self.TTL > 0) then begin
    S := S + '/' + IntToStr(Self.TTL);

    if (Self.NumberOfAddresses > 0) then begin
      S := S + '/' + IntToStr(Self.NumberOfAddresses);
    end;
  end;

  S := S + #13#10;

  Dest.Write(PChar(S)^, Length(S));
end;

//* TIdSdpConnection Private methods *******************************************

procedure TIdSdpConnection.SetAddress(Value: String);
begin
  Self.fAddress := Value;

  if (Self.RoutableAddress = '') then
    Self.fRoutableAddress := Value;
end;

procedure TIdSdpConnection.SetRoutableAddress(Value: String);
begin
  Self.fRoutableAddress := Value;

  if (Self.Address = '') then
    Self.fAddress := Value;
end;

//******************************************************************************
//* TIdSdpKey                                                                  *
//******************************************************************************
//* TIdSdpKey Public methods ***************************************************

procedure TIdSdpKey.Assign(Src: TPersistent);
var
  Other: TIdSdpKey;
begin
  if (Src is TIdSdpKey) then begin
    Other := Src as TIdSdpKey;

    Self.KeyName := Other.KeyName;
    Self.KeyType := Other.KeyType;
    Self.Value   := Other.Value;
  end
  else inherited Assign(Src);
end;

procedure TIdSdpKey.PrintOn(Dest: TStream);
var
  KName: String;
  S:     String;
begin
  if (Self.KeyType <> ktUnknown) then
    KName := KeyTypeToStr(Self.KeyType)
  else
    KName := Self.KeyName;

  S := 'k=' + KName;

  if (Self.KeyType <> ktPrompt) then
    S := S + ':' + Self.Value;

  S := S + #13#10;

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpMediaDescription                                                     *
//******************************************************************************
//* TIdSdpMediaDescription Public methods **************************************

constructor TIdSdpMediaDescription.Create;
begin
  inherited Create;

  Self.FormatList := TStringList.Create;
  Self.PortCount := 1;
end;

destructor TIdSdpMediaDescription.Destroy;
begin
  fAttributes.Free;
  fBandwidths.Free;
  fConnections.Free;
  fKey.Free;
  fRTPMapAttributes.Free;

  Self.FormatList.Free;

  inherited Destroy;
end;

procedure TIdSdpMediaDescription.AddAttribute(const Name, Value: String);
begin
  if (LowerCase(Name) = LowerCase(RTPMapAttribute)) then
    Self.RTPMapAttributes.Add(Value)
  else
    Self.Attributes.Add(Name + ':' + Value);
end;

procedure TIdSdpMediaDescription.AddRTPMapAttribute(const EncodingName: String;
                                                    PayloadType: TIdRTPPayloadType);
var
  NewAtt: TIdSdpRTPMapAttribute;
begin
  NewAtt := Self.RTPMapAttributes.Add;
  NewAtt.Value := IntToStr(PayloadType) + ' ' + EncodingName;
end;

procedure TIdSdpMediaDescription.Assign(Src: TPersistent);
var
  I:     Integer;
  Other: TIdSdpMediaDescription;
begin
  if Src is TIdSdpMediaDescription then begin
    Other := Src as TIdSdpMediaDescription;

    Self.Attributes.Assign(Other.Attributes);
    Self.Bandwidths.Assign(Other.Bandwidths);
    Self.Connections.Assign(Other.Connections);

    if Other.HasKey then
      Self.Key.Assign(Other.Key)
    else
      Self.ClearKey;

    Self.RTPMapAttributes.Assign(Other.RTPMapAttributes);

    Self.ClearFormats;
    for I := 0 to Other.FormatCount - 1 do
      Self.AddFormat(Other.Formats[I]);

    Self.Info := Other.Info;

    Self.MediaName := Other.MediaName;
    Self.MediaType := Other.MediaType;
    Self.Port      := Other.Port;
    Self.PortCount := Other.PortCount;
    Self.Protocol := Other.Protocol;
  end
  else inherited Assign(Src);
end;

procedure TIdSdpMediaDescription.AddFormat(const Fmt: String);
begin
  Self.FormatList.Add(Fmt);
end;

procedure TIdSdpMediaDescription.ClearAttributes;
begin
  Self.Attributes.Clear;
end;

procedure TIdSdpMediaDescription.ClearFormats;
begin
  Self.FormatList.Clear;
end;

function TIdSdpMediaDescription.Equals(Other: TIdSdpMediaDescription): Boolean;
begin
  Result := Self.RTPMapAttributes.Equals(Other.RTPMapAttributes);
end;

function TIdSdpMediaDescription.FormatCount: Integer;
begin
  Result := Self.FormatList.Count;
end;

function TIdSdpMediaDescription.HasAttribute(Att: TIdSdpAttribute): Boolean;
begin
  Result := Self.Attributes.HasAttribute(Att);
end;

function TIdSdpMediaDescription.HasConnection: Boolean;
begin
  Result := Self.Connections.Count > 0;
end;

function TIdSdpMediaDescription.HasFormat(Fmt: String): Boolean;
begin
  Result := Self.FormatList.IndexOf(Fmt) <> ItemNotFoundIndex;
end;

function TIdSdpMediaDescription.HasKey: Boolean;
begin
  Result := Assigned(fKey);
end;

function TIdSdpMediaDescription.IsRefusedStream: Boolean;
begin
  Result := Self.Port = RefusedPort;
end;

function TIdSdpMediaDescription.IsText: Boolean;
begin
  Result := Self.MediaType = mtText;
end;

function TIdSdpMediaDescription.IsValidFormat(Token: String): Boolean;
begin
  // RFC 4566 tells us (section 5.14) that
  //      If the <proto> sub-field is "RTP/AVP" or "RTP/SAVP" the <fmt>
  //      sub-fields contain RTP payload type numbers.
  // and
  //      If the <proto> sub-field is "udp" the <fmt> sub-fields MUST
  //      reference a media type describing the format under the "audio",
  //      "video", "text", "application", or "message" top-level media
  //      types.  The media type registration SHOULD define the packet
  //      format for use with UDP transport.
  //
  //      For media using other transport protocols, the <fmt> field is
  //      protocol specific.  Rules for interpretation of the <fmt> sub-
  //      field MUST be defined when registering new protocols (see Section
  //      8.2.2).
  // and RFC 4145 (section 10) tells us that
  //      For the TCP protocol, new formats SHOULD have an associated
  //      MIME registration.

  if Self.UsesRtpProtocol then
    Result := TIdSdpParser.IsByte(Token)
  else if ((Self.Protocol = Id_SDP_TCP) or (Self.Protocol = Id_SDP_udp)) then
    Result := TIdSdpParser.IsMimeType(Token)
  else
    Result := true;
end;

function TIdSdpMediaDescription.MediaTypeAsString: String;
begin
  if (Self.MediaType <> mtUnknown) then
    Result := MediaTypeToStr(Self.MediaType)
  else
    Result := Self.MediaName;
end;

procedure TIdSdpMediaDescription.PrintOn(Dest: TStream);
begin
  Self.PrintMediaField(Dest);
  Self.PrintInfoField(Dest);

  if Self.HasConnection then
    Self.Connections.PrintOn(Dest);

  Self.Bandwidths.PrintOn(Dest);

  if Self.HasKey then
    Self.Key.PrintOn(Dest);

  Self.RTPMapAttributes.PrintOn(Dest);
  Self.Attributes.PrintOn(Dest);
end;

procedure TIdSdpMediaDescription.RefuseStream;
begin
  Self.Port := RefusedPort;
end;

function TIdSdpMediaDescription.UsesBinding(Binding: TIdConnectionBindings): Boolean;
var
  I: Integer;
  J: Integer;
begin
  // This works as long as we're using standard RTP behaviour: even ports. It
  // doesn't (always) work with TCP-based media, or RTP with non-standard RTCP
  // ports!

  Result := false;
  for I := 0 to Self.Connections.Count - 1 do begin
    for J := 0 to Self.PortCount - 1 do begin
      if (Binding.LocalIP = Self.Connections[I].Address) and (Binding.LocalPort = (Self.Port + Cardinal(2*J))) then begin
        Result := true;
        Break;
      end;
    end;
  end;
end;

function TIdSdpMediaDescription.UsesRtpProtocol: Boolean;
begin
  Result := Copy(Self.Protocol, 1, 3) = 'RTP';
end;

//* TIdSdpMediaDescription Private methods *************************************

procedure TIdSdpMediaDescription.ClearKey;
begin
  FreeAndNil(Self.fKey);
end;

function TIdSdpMediaDescription.GetAttributes: TIdSdpAttributes;
begin
  if not Assigned(fAttributes) then
    fAttributes := TIdSdpAttributes.Create;

  Result := fAttributes;
end;

function TIdSdpMediaDescription.GetRTPMapAttributes: TIdSdpRTPMapAttributes;
begin
  if not Assigned(fRTPMapAttributes) then
    fRTPMapAttributes := TIdSdpRTPMapAttributes.Create;

  Result := fRTPMapAttributes;
end;

function TIdSdpMediaDescription.GetBandwidths: TIdSdpBandwidths;
begin
  if not Assigned(fBandwidths) then
    fBandwidths := TIdSdpBandwidths.Create;

  Result := fBandwidths;
end;

function TIdSdpMediaDescription.GetConnections: TIdSdpConnections;
begin
  if not Assigned(fConnections) then
    fConnections := TIdSdpConnections.Create;

  Result := fConnections;
end;

function TIdSdpMediaDescription.GetFormats(Index: Integer): String;
begin
  Result := Self.FormatList[Index];
end;

function TIdSdpMediaDescription.GetKey: TIdSdpKey;
begin
  if not Assigned(fKey) then
    fKey := TIdSdpKey.Create;

  Result := fKey;
end;

procedure TIdSdpMediaDescription.PrintInfoField(Dest: TStream);
var
  S: String;
begin
  if (Self.Info <> '') then begin
    S := 'i=' + Self.Info + #13#10;

    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpMediaDescription.PrintMediaField(Dest: TStream);
var
  I: Integer;
  S: String;
begin
  S := 'm=' + Self.MediaTypeAsString + ' ' + PortNumToStr(Self.Port);

  if (Self.PortCount > 1) then
    S := S + '/' + IntToStr(PortCount);

  S := S + ' ' + Self.Protocol;

  for I := 0 to Self.FormatCount - 1 do
    S := S + ' ' + Self.Formats[I];

  S := S + #13#10;
     
  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpOrigin                                                               *
//******************************************************************************
//* TIdSdpOrigin Public methods ************************************************

procedure TIdSdpOrigin.Assign(Src: TPersistent);
var
  Other: TIdSdpOrigin;
begin
  if (Src is TIdSdpOrigin) then begin
    Other := Src as TIdSdpOrigin;

    Self.Address        := Other.Address;
    Self.AddressType    := Other.AddressType;
    Self.NetType        := Other.NetType;
    Self.SessionID      := Other.SessionID;
    Self.SessionVersion := Other.SessionVersion;
    Self.Username       := Other.Username;
  end
  else inherited Assign(Src);
end;

procedure TIdSdpOrigin.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := 'o=' + Self.UsernameEncode(Self.Username) + ' '
     + Self.SessionID + ' '
     + Self.SessionVersion + ' '
     + Self.NetType + ' '
     + AddressTypeToStr(Self.AddressType) + ' '
     + Self.Address
     + #13#10;

  Dest.Write(PChar(S)^, Length(S));
end;

function TIdSdpOrigin.UsernameEncode(Name: String): String;
begin
  Result := StringReplace(Name, ' ', '_', [rfReplaceAll]);
end;

//******************************************************************************
//* TIdSdpRepeat                                                               *
//******************************************************************************
//* TIdSdpRepeat Public methods ************************************************

procedure TIdSdpRepeat.Assign(Src: TPersistent);
var
  Other: TIdSdpRepeat;
begin
  if (Src is TIdSdpRepeat) then begin
    Other := Src as TIdSdpRepeat;
    Self.Value := Other.Value;
  end
  else
    inherited Assign(Src);
end;

procedure TIdSdpRepeat.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := 'r=' + Self.Value + #13#10;

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpZoneAdjustment                                                       *
//******************************************************************************
//* TIdSdpZoneAdjustment Public methods ****************************************

procedure TIdSdpZoneAdjustment.Assign(Src: TPersistent);
var
  Other: TIdSdpZoneAdjustment;
begin
  if (Src is TIdSdpZoneAdjustment) then begin
    Other := Src as TIdSdpZoneAdjustment;
    Self.Value := Other.Value;
  end
  else
    inherited Assign(Src);
end;

procedure TIdSdpZoneAdjustment.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := 'z=' + Self.Value + #13#10;

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpTime                                                                 *
//******************************************************************************
//* TIdSdpTime Public methods **************************************************

destructor TIdSdpTime.Destroy;
begin
  fRepeats.Free;
  fZoneAdjustments.Free;

  inherited Destroy;
end;

procedure TIdSdpTime.Assign(Src: TPersistent);
var
  Other: TIdSdpTime;
begin
  if (Src is TIdSdpTime) then begin
    Other := Src as TIdSdpTime;

    Self.EndTime := Other.EndTime;
    Self.Repeats.Assign(Other.Repeats);
    Self.StartTime := Other.StartTime;
    Self.ZoneAdjustments.Assign(Other.ZoneAdjustments);
  end
  else inherited Assign(Src);
end;

procedure TIdSdpTime.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := 't=' + IntToStr(Self.StartTime) + ' ' + IntToStr(Self.EndTime) + #13#10;
  Dest.Write(PChar(S)^, Length(S));

  Self.Repeats.PrintOn(Dest);
  Self.ZoneAdjustments.PrintOn(Dest);
end;

//* TIdSdpTime Private methods *************************************************

function TIdSdpTime.GetRepeats: TIdSdpRepeats;
begin
  if not Assigned(fRepeats) then
    fRepeats := TIdSdpRepeats.Create;

  Result := fRepeats;
end;

function TIdSdpTime.GetZoneAdjustments: TIdSdpZoneAdjustments;
begin
  if not Assigned(fZoneAdjustments) then
    fZoneAdjustments := TIdSdpZoneAdjustments.Create;

  Result := fZoneAdjustments;
end;

//******************************************************************************
//* TIdSdpList                                                                 *
//******************************************************************************
//* TIdSdpList Public methods **************************************************

constructor TIdSdpList.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdSdpList.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdSdpList.Clear;
begin
  Self.List.Clear;
end;

procedure TIdSdpList.Assign(Src: TPersistent);
var
  I:     Integer;
  Other: TIdSdpList;
begin
  if (Src.ClassType = Self.ClassType) then begin
    Other := Src as TIdSdpList;
    Self.Clear;
    for I := 0 to Other.Count - 1 do
      Self.AddItem(Other.ItemAt(I));
  end
  else inherited Assign(Src);
end;

function TIdSdpList.Count: Integer;
begin
  Result := Self.List.Count;
end;

function TIdSdpList.Contains(O: TObject): Boolean;
begin
  Result := Self.List.IndexOf(O) <> ItemNotFoundIndex;
end;

function TIdSdpList.Equals(Other: TIdSdpList): Boolean;
var
  I:            Integer;
  Ours, Theirs: TStringList;
begin
  Ours := TStringList.Create;
  try
    Theirs := TStringList.Create;
    try
      for I := 0 to Self.Count - 1 do
        Ours.Add(Self.ItemAt(I).AsString);

      for I := 0 to Other.Count - 1 do
        Theirs.Add(Other.ItemAt(I).AsString);

      Ours.Sort;
      Theirs.Sort;

      Result := Ours.Text = Theirs.Text;
    finally
      Theirs.Free;
    end;
  finally
    Ours.Free;
  end;
end;

function TIdSdpList.ItemAt(Index: Integer): TIdPrintable;
begin
  Result := Self.List[Index] as TIdPrintable;
end;

procedure TIdSdpList.PrintOn(Dest: TStream);
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    Self.ItemAt(I).PrintOn(Dest);
end;

procedure TIdSdpList.Remove(O: TObject);
begin
  Self.List.Remove(O);
end;

//* TIdSdpList Protected methods ***********************************************

function TIdSdpList.AddItem: TIdPrintable;
begin
  Result := Self.ItemType.Create;
  Self.List.Add(Result);
end;

function TIdSdpList.AddItem(ToBeCopied: TIdPrintable): TIdPrintable;
begin
  Result := Self.AddItem;
  Result.Assign(ToBeCopied);
end;

function TIdSdpList.ItemType: TIdPrintableClass;
begin
  Result := nil;
  RaiseAbstractError(Self.ClassName, 'ItemType');
end;

//******************************************************************************
//* TIdSdpAttributes                                                           *
//******************************************************************************
//* TIdSdpAttributes Public methods ********************************************

function TIdSdpAttributes.Add: TIdSdpAttribute;
begin
  Result := Self.AddItem as TIdSdpAttribute;
end;

function TIdSdpAttributes.Add(Att: TIdSdpAttribute): TIdSdpAttribute;
begin
  Result := Att.Copy;
  Self.List.Add(Result);
end;

procedure TIdSdpAttributes.Add(A: TIdSdpAttributes);
var
  I: Integer;
begin
  for I := 0 to A.Count - 1 do
    Self.Add(A[I]);
end;

procedure TIdSdpAttributes.Add(const NameAndValue: String);
var
  NewAtt: TIdSdpAttribute;
begin
  NewAtt := TIdSdpAttribute.CreateAttribute(NameAndValue);
  try
    Self.Add(NewAtt);
  finally
    NewAtt.Free;
  end;
end;

function TIdSdpAttributes.HasAttribute(Att: TIdSdpAttribute): Boolean;
var
  I: Integer;
begin
  Result := false;
  I := 0;
  while not Result and (I < Self.Count) do begin
    Result := Self[I].Equals(Att);

    Inc(I);
  end;
end;

function TIdSdpAttributes.HasAttributeNamed(Name: String): Boolean;
begin
  Result := Self.IndexOfAttributeNamed(Name) <> ItemNotFoundIndex;
end;

function TIdSdpAttributes.ValueFor(AttributeName: String): String;
var
  Index: Integer;
begin
  Result := '';

  Index := Self.IndexOfAttributeNamed(AttributeName);

  if (Index <> ItemNotFoundIndex) then
    Result := Self[Index].Value;
end;

//* TIdSdpAttributes Protected methods *****************************************

function TIdSdpAttributes.ItemType: TIdPrintableClass;
begin
  Result := TIdSdpAttribute;
end;

//* TIdSdpAttributes Private methods *******************************************

function TIdSdpAttributes.GetDirection: TIdSdpDirection;
var
  Found: Boolean;
  I: Integer;
begin
  Result := sdSendRecv;

  Found := false;
  I     := 0;
  while (I < Self.Count) and not Found do begin
    if not TIdSdpParser.IsDirection(Self[I].Name) then
      Inc(I)
    else begin
      Result := StrToDirection(Self[I].Name);
      Found := true;
    end;
  end;
end;

function TIdSdpAttributes.GetItems(Index: Integer): TIdSdpAttribute;
begin
  Result := Self.List[Index] as TIdSdpAttribute;
end;

function TIdSdpAttributes.GetSetupType: TIdSdpSetupType;
var
  Index: Integer;
begin
  Index := Self.IndexOfAttributeNamed(SetupAttribute);

  if (Index = ItemNotFoundIndex) then begin
    // Actually, Result should be stActive or stPassive, depending on whether
    // the session description's an offer or an answer. We have no context here
    // though, thus we default to a safe option.
    Result := stHoldConn;
  end
  else
    Result := StrToSetupType(Self[Index].Value);
end;

function TIdSdpAttributes.IndexOfAttributeNamed(Name: String): Integer;
var
  I: Integer;
begin
  Result := ItemNotFoundIndex;

  for I := 0 to Self.Count - 1 do begin
    if (Self[I].Name = Name) then begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TIdSdpAttributes.SetDirection(Value: TIdSdpDirection);
var
  Direction: TIdSdpAttribute;
  Found: Boolean;
  I: Integer;
begin
  Found := false;
  I     := 0;
  while (I < Self.Count) and not Found do begin
    if not TIdSdpParser.IsDirection(Self[I].Name) then
      Inc(I)
    else
      Found := true;
  end;

  if Found then begin
    Self[I].Name := DirectionToStr(Value)
  end
  else begin
    Direction := Self.Add;
    Direction.Name := DirectionToStr(Value);
  end;
end;

procedure TIdSdpAttributes.SetSetupType(Value: TIdSdpSetupType);
var
  Index: Integer;
  ST:    TIdSdpAttribute;
begin
  Index := Self.IndexOfAttributeNamed(SetupAttribute);

  if (Index <> ItemNotFoundIndex) then
    Self[Index].Value := SetupTypeToStr(Value)
  else begin
    ST       := Self.Add;
    ST.Name  := SetupAttribute;
    ST.Value := SetupTypeToStr(Value);
  end;
end;

//******************************************************************************
//* TIdSdpRTPMapAttributes                                                     *
//******************************************************************************
//* TIdSdpRTPMapAttributes Public methods **************************************

function TIdSdpRTPMapAttributes.Add: TIdSdpRTPMapAttribute;
begin
  Result := Self.AddItem as TIdSdpRTPMapAttribute;
end;

function TIdSdpRTPMapAttributes.Add(Att: TIdSdpRTPMapAttribute): TIdSdpRTPMapAttribute;
begin
  Result := Att.Copy as TIdSdpRTPMapAttribute;
  Self.List.Add(Result);
end;

procedure TIdSdpRTPMapAttributes.Add(A: TIdSdpRTPMapAttributes);
var
  I: Integer;
begin
  for I := 0 to A.Count - 1 do
    Self.Add(A[I]);
end;

function TIdSdpRTPMapAttributes.Add(const Value: String): TIdSdpRTPMapAttribute;
begin
  Result := Self.Add;
  Result.Value := Value;
end;

function TIdSdpRTPMapAttributes.EncodingFor(Format: String): String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Self.Count - 1 do begin
    if (Self[I].Format = Format) then begin
      Result := Self[I].Encoding.EncodingName;
      Break;
    end;
  end;
end;

function TIdSdpRTPMapAttributes.FormatFor(EncodingName: String): String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Self.Count - 1 do begin
    if (Self[I].Encoding.EncodingName = EncodingName) then begin
      Result := Self[I].Format;
      Break;
    end;
  end;
end;

function TIdSdpRTPMapAttributes.HasAttribute(Att: TIdSdpAttribute): Boolean;
var
  I: Integer;
begin
  Result := false;
  I := 0;
  while not Result and (I < Self.Count) do begin
    Result := Self[I].Equals(Att);

    Inc(I);
  end;
end;

//* TIdSdpRTPMapAttributes Protected methods ***********************************

function TIdSdpRTPMapAttributes.ItemType: TIdPrintableClass;
begin
  Result := TIdSdpRTPMapAttribute;
end;

//* TIdSdpRTPMapAttributes Private methods *************************************

function TIdSdpRTPMapAttributes.GetItems(Index: Integer): TIdSdpRTPMapAttribute;
begin
  Result := Self.List[Index] as TIdSdpRTPMapAttribute;
end;

//******************************************************************************
//* TIdSdpBandwidths                                                           *
//******************************************************************************
//* TIdSdpBandwidths Public methods ********************************************

function TIdSdpBandwidths.Add: TIdSdpBandwidth;
begin
  Result := Self.AddItem as TIdSdpBandwidth;
end;

function TIdSdpBandwidths.Add(BW: TIdSdpBandwidth): TIdSdpBandwidth;
begin
  Result := Self.Add;
  Result.Assign(BW);
end;

procedure TIdSdpBandwidths.Add(B: TIdSdpBandwidths);
var
  I: Integer;
begin
  for I := 0 to B.Count - 1 do
    Self.Add(B[I]);
end;

//* TIdSdpBandwidths Protected methods *****************************************

function TIdSdpBandwidths.ItemType: TIdPrintableClass;
begin
  Result := TIdSdpBandwidth;
end;

//* TIdSdpBandwidths Private methods *******************************************

function TIdSdpBandwidths.GetItems(Index: Integer): TIdSdpBandwidth;
begin
  Result := Self.List[Index] as TIdSdpBandwidth;
end;

//******************************************************************************
//* TIdSdpConnections                                                          *
//******************************************************************************
//* TIdSdpConnections Public methods *******************************************

function TIdSdpConnections.Add: TIdSdpConnection;
begin
  Result := Self.AddItem as TIdSdpConnection;
end;

function TIdSdpConnections.Add(C: TIdSdpConnection): TIdSdpConnection;
begin
  Result := Self.Add;
  Result.Assign(C);
end;

procedure TIdSdpConnections.Add(C: TIdSdpConnections);
var
  I: Integer;
begin
  for I := 0 to C.Count - 1 do
    Self.Add(C[I]);
end;

procedure TIdSdpConnections.AddConnection(NetType: String;
                                          AddrType: TIdIPVersion;
                                          Addr: String;
                                          TTL: Byte);
var
  NewConnection: TIdSdpConnection;
begin
  NewConnection := Self.Add;

  NewConnection.NetType     := NetType;
  NewConnection.AddressType := AddrType;
  NewConnection.Address     := Addr;
  NewConnection.TTL         := TTL;
end;

//* TIdSdpConnections Protected methods ****************************************

function TIdSdpConnections.ItemType: TIdPrintableClass;
begin
  Result := TIdSdpConnection;
end;

//* TIdSdpConnections Private methods ******************************************

function TIdSdpConnections.GetItems(Index: Integer): TIdSdpConnection;
begin
  Result := Self.List[Index] as TIdSdpConnection;
end;

//******************************************************************************
//* TIdSdpMediaDescriptions                                                    *
//******************************************************************************
//* TIdSdpMediaDescriptions Public methods *************************************

function TIdSdpMediaDescriptions.Add: TIdSdpMediaDescription;
begin
  Result := Self.AddItem as TIdSdpMediaDescription;
end;

function TIdSdpMediaDescriptions.Add(Desc: TIdSdpMediaDescription): TIdSdpMediaDescription;
begin
  Result := Self.Add;
  Result.Assign(Desc);
end;

function TIdSdpMediaDescriptions.AllDescriptionsHaveConnections: Boolean;
var
  I: Integer;
begin
  Result := true;

  if Result then
    for I := 0 to Self.Count - 1 do
      Result := Result and Self[I].HasConnection;
end;

//* TIdSdpMediaDescriptions Protected methods **********************************

function TIdSdpMediaDescriptions.ItemType: TIdPrintableClass;
begin
  Result := TIdSdpMediaDescription;
end;

//* TIdSdpMediaDescriptions Private methods ************************************

function TIdSdpMediaDescriptions.GetItems(Index: Integer): TIdSdpMediaDescription;
begin
  Result := Self.List[Index] as TIdSdpMediaDescription;
end;

//******************************************************************************
//* TIdSdpRepeats                                                              *
//******************************************************************************
//* TIdSdpRepeats Public methods ***********************************************

function TIdSdpRepeats.Add: TIdSdpRepeat;
begin
  Result := Self.AddItem as TIdSdpRepeat;
end;

function TIdSdpRepeats.Add(R: TIdSdpRepeat): TIdSdpRepeat;
begin
  Result := Self.Add;
  Result.Assign(R);
end;

//* TIdSdpRepeats Protected methods ********************************************

function TIdSdpRepeats.ItemType: TIdPrintableClass;
begin
  Result := TIdSdpRepeat;
end;

//* TIdSdpRepeats Private methods **********************************************

function TIdSdpRepeats.GetItems(Index: Integer): TIdSdpRepeat;
begin
  Result := Self.List[Index] as TIdSdpRepeat;
end;

//******************************************************************************
//* TIdSdpTimes                                                                *
//******************************************************************************
//* TIdSdpTimes Public methods *************************************************

function TIdSdpTimes.Add: TIdSdpTime;
begin
  Result := Self.AddItem as TIdSdpTime;
end;

function TIdSdpTimes.Add(T: TIdSdpTime): TIdSdpTime;
begin
  Result := Self.Add;
  Result.Assign(T);
end;

//* TIdSdpTimes Protected methods **********************************************

function TIdSdpTimes.ItemType: TIdPrintableClass;
begin
  Result := TIdSdpTime;
end;

//* TIdSdpTimes Private methods ************************************************

function TIdSdpTimes.GetItems(Index: Integer): TIdSdpTime;
begin
  Result := Self.List[Index] as TIdSdpTime;
end;

//******************************************************************************
//* TIdSdpZoneAdjustments                                                      *
//******************************************************************************
//* TIdSdpZoneAdjustments Public methods ***************************************

function TIdSdpZoneAdjustments.Add: TIdSdpZoneAdjustment;
begin
  Result := Self.AddItem as TIdSdpZoneAdjustment;
end;

function TIdSdpZoneAdjustments.Add(Adj: TIdSdpZoneAdjustment): TIdSdpZoneAdjustment;
begin
  Result := Self.Add;
  Result.Assign(Adj);
end;

//* TIdSdpZoneAdjustments Protected methods ************************************

function TIdSdpZoneAdjustments.ItemType: TIdPrintableClass;
begin
  Result := TIdSdpZoneAdjustment;
end;

//* TIdSdpZoneAdjustments Private methods **************************************

function TIdSdpZoneAdjustments.GetItems(Index: Integer): TIdSdpZoneAdjustment;
begin
  Result := Self.List[Index] as TIdSdpZoneAdjustment;
end;

//******************************************************************************
//* TIdSdpPayload                                                              *
//******************************************************************************
//* TIdSdpPayload Public methods ***********************************************

class function TIdSdpPayload.CreateFrom(Src: TStream): TIdSdpPayload;
begin
  Result := TIdSdpPayload.Create;
  try
    Result.ReadFrom(Src);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

class function TIdSdpPayload.CreateFrom(Src: String): TIdSdpPayload;
var
  S: TStringStream;
begin
  S := TStringStream.Create(Src);
  try
    Result := Self.CreateFrom(S);
  finally
    S.Free;
  end;
end;

destructor TIdSdpPayload.Destroy;
begin
  fAttributes.Free;
  fBandwidths.Free;
  fConnections.Free;
  fEmailAddress.Free;
  fKey.Free;
  fMediaDescriptions.Free;
  fOrigin.Free;
  fRTPMapAttributes.Free;
  fTimes.Free;

  inherited Destroy;
end;

function TIdSdpPayload.AddConnection: TIdSdpConnection;
var
  I: Integer;
begin
  Result := Self.Connections.Add;

  for I := 0 to Self.MediaDescriptionCount - 1 do
    Self.MediaDescriptionAt(I).Connections.Add(Result);
end;

function TIdSdpPayload.AddMediaDescription(Desc: TIdSdpMediaDescription): TIdSdpMediaDescription;
begin
  // Note the absence of rtpmap attributes below. rtpmap attributes make no
  // sense at a session level.
  Result := Self.MediaDescriptions.Add(Desc);
  Result.Attributes.Add(Self.Attributes);
  Result.Connections.Add(Self.Connections);
end;

function TIdSdpPayload.AddMediaDescription: TIdSdpMediaDescription;
var
  Desc: TIdSdpMediaDescription;
begin
  Desc := TIdSdpMediaDescription.Create;
  try
    Result := Self.AddMediaDescription(Desc);
  finally
    Desc.Free;
  end;
end;

function TIdSdpPayload.AllDescriptionsHaveConnections: Boolean;
begin
  Result := Self.MediaDescriptions.AllDescriptionsHaveConnections;
end;

function TIdSdpPayload.AsString: String;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.PrintOn(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

function TIdSdpPayload.ConnectionAt(Index: Integer): TIdSdpConnection;
begin
  if (Index >= 0) and (Index < Self.ConnectionCount) then
    Result := Self.Connections[Index]
  else
    Result := nil;
end;

function TIdSdpPayload.ConnectionCount: Integer;
begin
  Result := Self.Connections.Count;
end;

function TIdSdpPayload.Equals(Other: TIdSdpPayload; IgnoreTimestamps: Boolean = false): Boolean;
begin
  Result :=  Self.Attributes.Equals(Other.Attributes)
        and  Self.Bandwidths.Equals(Other.Bandwidths)
        and (Self.EmailAddress.Text = Other.EmailAddress.Text)
        and (Self.Info = Other.Info)
        and (Self.Key.AsString = Other.Key.AsString)
        and (Self.Origin.AsString = Other.Origin.AsString)
        and (Self.PhoneNumber = Other.PhoneNumber)
        and  Self.RTPMapAttributes.Equals(Other.RTPMapAttributes)
        and (Self.SessionName = Other.SessionName)
        and  Self.Times.Equals(Other.Times)
        and (Self.Uri = Other.Uri)
        and (Self.Version = Other.Version)
end;

function TIdSdpPayload.Equals(Other: String; IgnoreTimestamps: Boolean = false): Boolean;
var
  OtherPayload: TIdSdpPayload;
begin
  try
    OtherPayload := TIdSdpPayload.CreateFrom(Other);
    try
      Result := Self.Equals(OtherPayload, IgnoreTimestamps)
    finally
      OtherPayload.Free;
    end;
  except
    Result := false;
  end;
end;

procedure TIdSdpPayload.GetRtpMapAttributes(Atts: TIdSdpRTPMapAttributes);
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to Self.RTPMapAttributes.Count - 1 do
    if not Atts.HasAttribute(Self.RTPMapAttributes[I]) then
      Atts.Add(Self.RTPMapAttributes[I]);

  for I := 0 to Self.MediaDescriptionCount - 1 do
    for J := 0 to Self.MediaDescriptionAt(I).RTPMapAttributes.Count - 1 do
      if not Atts.HasAttribute(Self.MediaDescriptionAt(I).RTPMapAttributes[J]) then
        Atts.Add(Self.MediaDescriptionAt(I).RTPMapAttributes[J]);
end;

function TIdSdpPayload.HasAttribute(Att: TIdSdpAttribute): Boolean;
begin
  Result := Self.Attributes.HasAttribute(Att);
end;

function TIdSdpPayload.HasKey: Boolean;
begin
  Result := Assigned(fKey);
end;

procedure TIdSdpPayload.InitializeProfile(Profile: TIdRTPProfile);
var
  I:       Integer;
  RTPMaps: TIdSdpRTPMapAttributes;
begin
  RTPMaps := TIdSdpRTPMapAttributes.Create;
  try
    Self.GetRtpMapAttributes(RTPMaps);

    for I := 0 to RTPMaps.Count - 1 do begin
      Profile.AddEncoding(RTPMaps[I].Encoding,
                          StrToInt(RTPMaps[I].Format));
    end;
  finally
    RTPMaps.Free;
  end;
end;

function TIdSdpPayload.MediaDescriptionAt(Index: Integer): TIdSdpMediaDescription;
begin
  if (Index >= 0) and (Index < Self.MediaDescriptionCount) then
    Result := Self.MediaDescriptions[Index]
  else
    Result := nil;
end;

function TIdSdpPayload.MediaDescriptionCount: Integer;
begin
  Result := Self.MediaDescriptions.Count;
end;

function TIdSdpPayload.MimeType: String;
begin
  Result := SdpMimeType;
end;

procedure TIdSdpPayload.PrintOn(Dest: TStream);
begin
  Self.PrintVersionField(Dest);
  Self.Origin.PrintOn(Dest);
  Self.PrintSessionNameField(Dest);
  Self.PrintInfo(Dest);
  Self.PrintUriField(Dest);
  Self.PrintEmailAddressField(Dest);
  Self.PrintPhoneNumber(Dest);

  if (Self.MediaDescriptionCount = 0) then
    Self.Connections.PrintOn(Dest);

  Self.Bandwidths.PrintOn(Dest);
  Self.Times.PrintOn(Dest);

  if Self.HasKey then
    Self.Key.PrintOn(Dest);

  Self.Attributes.PrintOn(Dest);
  Self.MediaDescriptions.PrintOn(Dest);
end;

procedure TIdSdpPayload.ReadFrom(Src: TStream);
var
  P: TIdSdpParser;
begin
  P := TIdSdpParser.Create;
  try
    P.Source := Src;

    P.Parse(Self);
  finally
    P.Free;
  end;
end;

procedure TIdSdpPayload.ReadFrom(Src: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(Src);
  try
    Self.ReadFrom(S);
  finally
    S.Free;
  end;
end;

procedure TIdSdpPayload.RemoveLastMediaDescription;
begin
  Self.MediaDescriptions.Remove(Self.MediaDescriptionAt(Self.MediaDescriptionCount - 1));
end;

procedure TIdSdpPayload.SetConnectionAddress(NewAddress: String);
var
  I, J: Integer;
begin
  for I := 0 to Self.ConnectionCount - 1 do
    Self.Connections[I].Address := NewAddress;

  for I := 0 to Self.MediaDescriptionCount -1 do
    for J := 0 to Self.MediaDescriptionAt(I).Connections.Count - 1 do
      Self.MediaDescriptionAt(I).Connections[J].Address := NewAddress;
end;

//* TIdSdpPayload Private methods **********************************************

function TIdSdpPayload.GetAttributes: TIdSdpAttributes;
begin
  if not Assigned(fAttributes) then
    fAttributes := TIdSdpAttributes.Create;

  Result := fAttributes;
end;

function TIdSdpPayload.GetBandwidths: TIdSdpBandwidths;
begin
  if not Assigned(fBandwidths) then
    fBandwidths := TIdSdpBandwidths.Create;

  Result := fBandwidths;
end;

function TIdSdpPayload.GetConnections: TIdSdpConnections;
begin
  if not Assigned(fConnections) then
    fConnections := TIdSdpConnections.Create;

  Result := fConnections;
end;

function TIdSdpPayload.GetEmailAddress: TIdEmailAddressItem;
begin
  if not Assigned(fEmailAddress) then
    fEmailAddress := TIdEmailAddressItem.Create(nil);

  Result := fEmailAddress;
end;

function TIdSdpPayload.GetKey: TIdSdpKey;
begin
  if not Assigned(fKey) then
    fKey := TIdSdpKey.Create;

  Result := fKey;
end;

function TIdSdpPayload.GetMediaDescriptions: TIdSdpMediaDescriptions;
begin
  if not Assigned(fMediaDescriptions) then
    fMediaDescriptions := TIdSdpMediaDescriptions.Create;

  Result := fMediaDescriptions;
end;

function TIdSdpPayload.GetOrigin: TIdSdpOrigin;
begin
  if not Assigned(fOrigin) then
    fOrigin := TIdSdpOrigin.Create;

  Result := fOrigin;
end;

function TIdSdpPayload.GetTimes: TIdSdpTimes;
begin
  if not Assigned(fTimes) then
    fTimes := TIdSdpTimes.Create;

  Result := fTimes;
end;

function TIdSdpPayload.MakeRTPMapAttributes: TIdSdpRTPMapAttributes;
begin
  if not Assigned(fRTPMapAttributes) then
    fRTPMapAttributes := TIdSdpRTPMapAttributes.Create;

  Result := fRTPMapAttributes;
end;

procedure TIdSdpPayload.PrintEmailAddressField(Dest: TStream);
var
  S: String;
begin
  if (Self.EmailAddress.Address <> '') then begin
    S := 'e=' + Self.EmailAddress.Address + #13#10;
    
    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpPayload.PrintInfo(Dest: TStream);
var
  S: String;
begin
  if (Self.Info <> '') then begin
    S := 'i=' + Self.Info + #13#10;

    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpPayload.PrintPhoneNumber(Dest: TStream);
var
  S: String;
begin
  if (Self.PhoneNumber <> '') then begin
    S := 'p=' + Self.PhoneNumber + #13#10;
    
    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpPayload.PrintSessionNameField(Dest: TStream);
var
  S:                 String;
  MungedSessionName: String;
begin
  if (Self.SessionName <> '') then
    MungedSessionName := Self.SessionName
  else
    MungedSessionName := BlankSessionName;

  S := 's=' + MungedSessionName + #13#10;
  Dest.Write(PChar(S)^, Length(S));
end;

procedure TIdSdpPayload.PrintUriField(Dest: TStream);
var
  S: String;
begin
  if (Self.URI <> '') then begin
    S := 'u=' + Self.URI + #13#10;

    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpPayload.PrintVersionField(Dest: TStream);
var
  S: String;
begin
  S := 'v=' + IntToStr(Self.Version) + #13#10;

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpParser                                                               *
//******************************************************************************
//* TIdSdpParser Public methods ************************************************

class function TIdSdpParser.IsAddressType(const Token: String): Boolean;
begin
  Result := Trim(Token) <> '';

  if Result then
    Result := StrToAddressType(Token) <> Id_IPUnknown;
end;

class function TIdSdpParser.IsBandwidthType(const Token: String): Boolean;
begin
  Result := Self.IsToken(Token);
end;

class function TIdSdpParser.IsByteString(const Token: String): Boolean;
begin
//   byte-string =         1*(0x01..0x09|0x0b|0x0c|0x0e..0xff)
//                         ;any byte except NUL, CR or LF

  Result := Self.ContainsNoneOf(Token, IllegalByteStringChars);
end;

class function TIdSdpParser.IsDirection(const Token: String): Boolean;
begin
  Result := Trim(Token) <> '';

  if Result then
    Result := StrToDirection(Token) <> sdUnknown;
end;

class function TIdSdpParser.IsKeyData(const Token: String): Boolean;
var
  I: Integer;
begin
  Result := Token <> '';

  if Result then
    for I := 1 to Length(Token) do
      Result := Result and (Token[I] in EmailSafeChars);
end;

class function TIdSdpParser.IsKeyType(const Token: String): Boolean;
begin
  // Note that this is a departure from RFC 4566! RFC 4566 limits the allowed
  // key types to {prompt, clear, base64, uri}. We accept tokens in the
  // interests of supporting as-yet-undefined key types.
   
  Result := Self.IsToken(Token);
end;

class function TIdSdpParser.IsMediaType(const Token: String): Boolean;
begin
  Result := Self.IsToken(Token);
end;

class function TIdSdpParser.IsMimeType(const Token: String): Boolean;
const
  TSpecials = ['(', ')', '<', '>', '@', ',', ';', ':', '\', '"', '/', '[', ']', '?', '='];
  MimeTokenCharset = [#33..#126] - TSpecials;
var
  MimeType: String;
  SubType:  String;
  WorkStr:  String;
begin
  // Return true if Token is a well-formed MIME type.

  // From RFC 2045:
  //     token := 1*<any (US-ASCII) CHAR except SPACE, CTLs,
  //                 or tspecials>
  //     tspecials :=  "(" / ")" / "<" / ">" / "@" /
  //                   "," / ";" / ":" / "\" / <">
  //                   "/" / "[" / "]" / "?" / "="
  //                   ; Must be in quoted-string,
  //                   ; to use within parameter values

  Result := Token <> '';

  if Result then begin
    WorkStr := Token;

    MimeType := Fetch(WorkStr, '/', true);

    Result := Result and ContainsOnly(MimeType, MimeTokenCharset);

    if Result then begin
      SubType := Fetch(WorkStr, ';', true);

      Result := Result and ContainsOnly(SubType, MimeTokenCharset);

      // parse parameters
    end;
  end;
end;

class function TIdSdpParser.IsMulticastAddress(IpVersion: TIdIPVersion;
                                               const Token: String): Boolean;
var
  Address:   String;
  N:         String;
  FirstByte: Integer;
begin
  Address := Token;

  case IpVersion of
    Id_IPv4: begin
      Result := TIdIPAddressParser.IsIPv4Address(Address);

      if Result then begin
        N := Fetch(Address, '.');
        FirstByte := StrToInt(N);
        Result := Result and (FirstByte >= 224) and (FirstByte <= 239);
      end;
    end;
    Id_IPv6: begin
      Result := TIdIPAddressParser.IsIPv6Address(Address);
      Result := Result and (Lowercase(Copy(Address, 1, 2)) = 'ff');
    end;
  else
    raise EParserError.Create('Unknown TIdIPVersion in IsMulticastAddress');
  end;
end;

class function TIdSdpParser.IsNetType(const Token: String): Boolean;
begin
  Result := (Token = Id_SDP_IN);
end;

class function TIdSdpParser.IsPhone(const Token: String): Boolean;
begin
  Result := Length(Token) >= 3;

  if Result then begin
    Result := Result and (Token[1] = '+');
    Result := Result and (Token[2] in ['1'..'9']);

      Result := Result and Self.ContainsOnly(Copy(Token, 3, Length(Token)), ['0'..'9', '-', ' ']);
  end;
end;

class function TIdSdpParser.IsPhoneNumber(const Header: String): Boolean;
var
  Token, S: String;
  I:        Integer;
begin
  Result := true;

  S := Header;
  if (Pos('<', S) > 0) then begin
    Token := Fetch(S, '<');
    for I := 1 to Length(Token) do
      Result := Result and (Token[I] in EmailSafeChars);

    Token := Fetch(S, '>');
    Result := Result and Self.IsPhone(Token);

    Result := Result and (S = '');
  end else begin
    if (Pos('(', S) > 0) then begin
      Token := Trim(Fetch(S, '('));
      Result := Result and Self.IsPhone(Token);
      Fetch(S, ')');
      Result := Result and (S = '');
    end
    else
      Result := Self.IsPhone(S);
  end;
end;

class function TIdSdpParser.IsPort(const Token: String): Boolean;
var
  N: Integer;
  E: Integer;
begin
  Result := Token = Trim(Token);

  if Result then begin
    Val(Token, N, E);
    Result := Result and (E = 0) and (0 <= N){ and (N < 65536)};
  end;
end;

class function TIdSdpParser.IsProtocol(const Token: String): Boolean;
begin
  // See the BNF in RFC 4566, section 9. Note that the production for this
  // entity in RFC 2327's BNF is broken - it cannot produce "RTP/AVP".

  if (Token = '') then begin
    Result := false;
    Exit;
  end;

  if (Token[1] = '/') then begin
    Result := false;
    Exit;
  end;

  if (Token[Length(Token)] = '/') then begin
    Result := false;
    Exit;
  end;

  Result := (Pos('//', Token) = 0) and
            Self.ContainsOnly(Token, TokenChars + ['/']);
end;

class function TIdSdpParser.IsText(const Token: String): Boolean;
begin
  Result := Self.ContainsNoneOf(Token, [#0, #10, #13]);
end;

class function TIdSdpParser.IsTime(const Token: String): Boolean;
var
  I: Integer;
begin
  Result := Token <> '';

  if Result then
    Result := Result and (Self.IsNumber(Token[1]));

  if Result then
    for I := 1 to Length(Token) - 1 do
      Result := Result and Self.IsDigit(Token[I]);

   if Result then
     Result := Result and (Self.IsDigit(Token[Length(Token)])
                       or (Token[Length(Token)] in TimeTypes));
end;

class function TIdSdpParser.IsToken(const S: String): Boolean;
begin
  Result := Self.ContainsOnly(S, TokenChars);
end;

procedure TIdSdpParser.Parse(Payload: TIdSdpPayload);
begin
  Self.ParseSessionHeaders(Payload);

  while (Self.PeekLine <> '') do
    Self.ParseMediaDescription(Payload);

  if (Payload.Connections.Count = 0)
     and not ((Payload.MediaDescriptionCount > 0)
              and Payload.AllDescriptionsHaveConnections) then
    raise EParserError.Create(MissingConnection);
end;

//* TIdSdpParser Private methods ***********************************************

class function TIdSdpParser.ContainsOnly(Token: String; AllowedChars: TCharSet): Boolean;
var
  I: Integer;
begin
  Result := (Token <> '');

  if Result then
    for I := 1 to Length(Token) do begin
      Result := Result and (Token[I] in AllowedChars);
    end;
end;

class function TIdSdpParser.ContainsNoneOf(Token: String; DisallowedChars: TCharSet): Boolean;
var
  I: Integer;
begin
  Result := (Token <> '');

  if Result then
    for I := 1 to Length(Token) do begin
      Result := Result and not (Token[I] in DisallowedChars);
    end;
end;

procedure TIdSdpParser.AssertHeaderOrder;
var
  CurrentHeader: Char;
  HeaderOrder:   String;
  LastHeader:    Char;
begin
  // Self.PeekChar gives us the current header. Call this the CurrentHeader.
  // Let's look in the appropriate header order to see if this header
  // occurs in the wrong place. The "wrong place" means that we have
  // already processed a successor header. Call this header LastSessionHeader

  CurrentHeader := Self.Peek;

  if Self.ParsingSessionHeaders then begin
    HeaderOrder := SessionHeaderOrder;
    LastHeader  := Self.LastSessionHeader;
  end
  else begin
    HeaderOrder := MediaHeaderOrder;
    LastHeader  := Self.LastMediaHeader;
  end;

  if (Pos(LastHeader, HeaderOrder) > Pos(CurrentHeader, HeaderOrder)) then
    raise EParserError.Create(Format(BadHeaderOrder, [CurrentHeader, LastHeader]));
end;

function TIdSdpParser.GetAndCheckInfo: String;
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

  if (Name <> RSSDPInformationName) then
    raise EParserError.Create(BadHeaderOrder);

  if not Self.IsText(Value) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPInformationName,
                                      Name + '=' + Value]));
  Result := Value;

  if Self.ParsingSessionHeaders then
    Self.LastSessionHeader := RSSDPInformationName
  else
    Self.LastMediaHeader := RSSDPInformationName;
end;

procedure TIdSdpParser.ParseAttribute(Attributes: TIdSdpAttributes);
var
  Att:           TIdSdpAttribute;
  OriginalValue: String;
  Name:          String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  Att := TIdSdpAttribute.CreateAttribute(Value);
  try
    if not Self.IsAlphaNumeric(Att.Name) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPAttributeName,
                                        Name + '=' + OriginalValue]));

    if (Att.Value <> '') and not Self.IsByteString(Att.Value) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPAttributeName,
                                        Name + '=' + OriginalValue]));

    Attributes.Add(Att);
  finally
    Att.Free;
  end;
end;

procedure TIdSdpParser.ParseBandwidth(Bandwidths: TIdSdpBandwidths);
var
  BW:            TIdSdpBandwidth;
  Name:          String;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  Token := Fetch(Value, ':');
  if not Self.IsBandwidthType(Token) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPBandwidthName,
                                      Name + '=' + OriginalValue]));

  BW := Bandwidths.Add;
  try
    BW.BandwidthType := StrToBandwidthType(Token);
    BW.BandwidthName := Token;

    // We should just be able to take the rest of the string. However, as of
    // this change, there's at least one SIP stack that uses a space in the
    // bandwidth. Bastards.
    Token := Fetch(Value, ' ');
    if not Self.IsNumber(Token) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPBandwidthName,
                                        Name + '=' + OriginalValue]));
    BW.Bandwidth := StrToInt(Token);

    if Self.ParsingSessionHeaders then
      Self.LastSessionHeader := RSSDPBandwidthName
    else
      Self.LastMediaHeader := RSSDPBandwidthName;
  except
    Bandwidths.Remove(BW);

    raise;
  end;
end;

procedure TIdSdpParser.ParseConnection(Connections: TIdSdpConnections);
var
  Addr:          String;
  AddrType:      String;
  I:             Integer;
  Multicast:     Boolean;
  Name:          String;
  NetType:       String;
  NumAddrs:      String;
  OriginalValue: String;
  TTL:           String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  NetType := Fetch(Value, ' ');
  if not Self.IsNetType(NetType) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPConnectionName,
                                      Name + '=' + OriginalValue]));

  AddrType := Fetch(Value, ' ');
  if not Self.IsAddressType(AddrType) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPConnectionName,
                                      Name + '=' + OriginalValue]));

  Multicast := Pos('/', Value) > 0;

  if Multicast then begin
    Addr := Fetch(Value, '/');
    if not Self.IsMulticastAddress(StrToAddressType(AddrType), Addr)
      and not Self.IsFQDN(Addr) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPConnectionName,
                                        Name + '=' + OriginalValue]));

    TTL := Fetch(Value, '/');
    if not Self.IsByte(TTL) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPConnectionName,
                                        Name + '=' + OriginalValue]));

    NumAddrs := Value;
  end
  else begin
    Addr     := Value;
    NumAddrs := '';
    TTL      := '0';

    if not TIdIPAddressParser.IsIPAddress(StrToAddressType(AddrType), Value)
      and not Self.IsFQDN(Value) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPConnectionName,
                                        Name + '=' + OriginalValue]));
  end;

  if (NumAddrs <> '') then begin
    for I := 0 to StrToInt(NumAddrs) - 1 do
      Connections.AddConnection(NetType,
                                StrToAddressType(AddrType),
                                TIdIPAddressParser.IncIPAddress(Addr, I),
                                StrToInt(TTL))
  end
  else
    Connections.AddConnection(NetType,
                              StrToAddressType(AddrType),
                              Addr,
                              StrToInt(TTL));

  if Self.ParsingSessionHeaders then
    Self.LastSessionHeader := RSSDPConnectionName
  else
    Self.LastMediaHeader := RSSDPConnectionName;
end;

procedure TIdSdpParser.ParseEmail(Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

//  if not Self.IsEmailAddress(Value) then
//    raise EParserError.Create(Format(MalformedToken, [RSSDPEmailName, Name + '=' + Value]));

  Payload.EmailAddress.Text := Value;
  Self.LastSessionHeader := RSSDPEmailName;
end;

procedure TIdSdpParser.ParseHeader(var Name, Value: String);
var
  Line: String;
begin
  Line  := Self.ReadLn;
  Value := Line;
  Name  := Fetch(Value, '=');

  if (Name = '') then
    raise EParserError.Create(Format(MalformedToken, ['Header', Line]));

  if (Value = '') then
    raise EParserError.Create(Format(MalformedToken, [Name, Line]));

  // Technically speaking we should throw out the header, but we don't because
  // we can't answer the question "Can an Origin header have an empty string
  // as the username?" 'o= 467752 467752 IN IP4 192.168.1.41' might be legal -
  // the BNF says nothing on this.
//  if (Name <> Trim(Name)) or (Value <> Trim(Value)) then
//    raise EParserError.Create(Format(MalformedToken, [Trim(Name), Line]));
end;

procedure TIdSdpParser.ParseInfo(MediaDescription: TIdSdpMediaDescription);
begin
  MediaDescription.Info := Self.GetAndCheckInfo;
end;

procedure TIdSdpParser.ParseInfo(Payload: TIdSdpPayload);
begin
  Payload.Info := Self.GetAndCheckInfo;
end;

procedure TIdSdpParser.ParseKey(Key: TIdSdpKey);
var
  Name:          String;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  if (Pos(':', Value) > 0) then
    Token := Fetch(Value, ':')
  else begin
    Token := Value;
    Value := '';
  end;

  if not Self.IsKeyType(Token) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPKeyName,
                                      Name + '=' + OriginalValue]));

  Key.KeyName := Token;
  Key.KeyType := StrToKeyType(Token);

  if (Key.KeyType = ktPrompt) then begin
    if (Value <> '') then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPKeyName,
                                        Name + '=' + OriginalValue]))
  end
  else begin
    // Given that we don't know whether an unknown key type even uses a key (we
    // can safely say only that we know nothing about it), if Value is empty
    // we don't care.
    if Self.IsKeyData(Value) or (Key.KeyType = ktUnknown) then
      Key.Value := Value
    else
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPKeyName,
                                        Name + '=' + OriginalValue]));
  end;

  if Self.ParsingSessionHeaders then
    Self.LastSessionHeader := RSSDPKeyName
  else
    Self.LastMediaHeader := RSSDPKeyName;
end;

procedure TIdSdpParser.ParseMediaDescription(Payload: TIdSdpPayload);
var
  Count:         String;
  Name:          String;
  NewMediaDesc:  TIdSdpMediaDescription;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  // m=<media type> <port>[/<number>] <transport> <fmt list>
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  NewMediaDesc := TIdSdpMediaDescription.Create;
  try
    Token := Fetch(Value, ' ');
    if not Self.IsMediaType(Token) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPMediaDescriptionName,
                                        Name + '=' + OriginalValue]));
    NewMediaDesc.MediaName := Token;
    NewMediaDesc.MediaType := StrToMediaType(Token);

    Token := Fetch(Value, ' ');
    if (Pos('/', Token) > 0) then begin
      Count := Token;
      Token := Fetch(Count, '/');
    end;

    if not Self.IsPort(Token) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPMediaDescriptionName,
                                        Name + '=' + OriginalValue]));
      NewMediaDesc.Port := StrToPortNum(Token);

    if (Count <> '') and not Self.IsNumber(Count) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPMediaDescriptionName,
                                        Name + '=' + OriginalValue]));
    NewMediaDesc.PortCount := StrToIntDef(Count, 1);

    Token := Fetch(Value, ' ');
    if not Self.IsProtocol(Token) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPMediaDescriptionName,
                                        Name + '=' + OriginalValue]));
    NewMediaDesc.Protocol := Token;

    while (Value <> '') do begin
      Token := Fetch(Value, ' ');
      if not NewMediaDesc.IsValidFormat(Token) then
        raise EParserError.Create(Format(MalformedToken,
                                         [RSSDPMediaDescriptionName,
                                          Name + '=' + OriginalValue]));
      NewMediaDesc.AddFormat(Token);
    end;

    if (NewMediaDesc.FormatCount = 0) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPMediaDescriptionName,
                                        Name + '=' + OriginalValue]));

    Self.ParseMediaOptionalHeaders(NewMediaDesc);

    Payload.AddMediaDescription(NewMediaDesc);
  except
    Payload.MediaDescriptions.Remove(NewMediaDesc);
    FreeAndNil(NewMediaDesc);

    raise;
  end;

  Self.LastSessionHeader := RSSDPMediaDescriptionName;
  Self.LastMediaHeader   := RSSDPMediaDescriptionName;
end;

procedure TIdSdpParser.ParseMediaOptionalHeaders(MediaDescription: TIdSdpMediaDescription);
var
  NextHeader: String;
begin
  NextHeader := Self.PeekLine;
  while not Self.Eof
        and (NextHeader <> '')
        and (NextHeader[1] <> RSSDPMediaDescriptionName) do begin
    case NextHeader[1] of
      RSSDPInformationName: Self.ParseInfo(MediaDescription);
      RSSDPConnectionName:  Self.ParseConnection(MediaDescription.Connections);
      RSSDPBandwidthName:   Self.ParseBandwidth(MediaDescription.Bandwidths);
      RSSDPKeyName:         Self.ParseKey(MediaDescription.Key);
      RSSDPAttributeName:
        if (Pos(RTPMapAttribute, NextHeader) > 0) then
          Self.ParseRTPMapAttribute(MediaDescription.RTPMapAttributes)
        else
          Self.ParseAttribute(MediaDescription.Attributes);
    else
      raise EParserError.Create(Format(UnknownOptionalHeader, [NextHeader]));
    end;

    NextHeader := Self.PeekLine;
  end;
end;

procedure TIdSdpParser.ParseOrigin(Payload: TIdSdpPayload);
var
  Name:          String;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  if (Name <> RSSDPOriginName) then
    raise EParserError.Create(MissingOrigin);

  Payload.Origin.Username := Fetch(Value, ' ');

  // Cf RFC 2327 Appendix A and meditate on the production "Username = safe".
  // Note, please, that the SDP examples clearly show that Username has more
  // than one character, normally, so Username SHOULD be either 1*(safe) or
  // *(safe). We don't know, ergo 'o= 467752 467752 IN IP4 192.168.1.41' might
  // be legal (meaning Username = '').
//  if (Payload.Origin.Username = '') then
//    raise EParserError.Create(Format(MalformedToken,
//                                     [RSSDPOriginName,
//                                      Name + '=' + OriginalValue]));

  Token := Fetch(Value, ' ');
  if not Self.IsNumber(Token) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPOriginName,
                                      Name + '=' + OriginalValue]));
  Payload.Origin.SessionID := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsNumber(Token) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPOriginName,
                                      Name + '=' + OriginalValue]));
  Payload.Origin.SessionVersion := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsNetType(Token) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPOriginName,
                                      Name + '=' + OriginalValue]));
  Payload.Origin.NetType := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsAddressType(Token) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPOriginName,
                                      Name + '=' + OriginalValue]));

  Payload.Origin.AddressType := StrToAddressType(Token);

  Payload.Origin.Address := Value;
  if (Payload.Origin.Address = '') then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPOriginName,
                                      Name + '=' + OriginalValue]));

  Self.LastSessionHeader := RSSDPOriginName;
end;

procedure TIdSdpParser.ParsePhone(Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

  if not Self.IsPhoneNumber(Value) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPPhoneName,
                                      Name + '=' + Value]));

  if (Payload.PhoneNumber <> '') then
    raise EParserError.Create(Format(TooManyHeaders, [RSSDPPhoneName]));

  Payload.PhoneNumber := Value;
  Self.LastSessionHeader := RSSDPPhoneName;
end;

procedure TIdSdpParser.ParseRepeat(Time: TIdSdpTime);
var
  Name:          String;
  OriginalValue: String;
  Rpt:           TIdSdpRepeat;
  Token:         String;
  Value:         String;
begin
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  if (Name <> RSSDPRepeatName) then
    raise EParserError.Create(BadHeaderOrder);

  Rpt := TIdSdpRepeat.Create;
  try
    Rpt.Value := Value;

    while (Value <> '') do begin
      Token := Fetch(Value, ' ');

      if not Self.IsTime(Token) then
        raise EParserError.Create(Format(MalformedToken,
                                         [RSSDPRepeatName,
                                          Name + '=' + OriginalValue]));
    end;

    Time.Repeats.Add(Rpt);
  except
    if not Time.Repeats.Contains(Rpt) then
      Rpt.Free;

    raise;
  end;
end;

procedure TIdSdpParser.ParseRTPMapAttribute(RTPMapAttributes: TIdSdpRTPMapAttributes);
var
  Att:           TIdSdpRTPMapAttribute;
  OriginalValue: String;
  Name:          String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  Att := TIdSdpAttribute.CreateAttribute(Value) as TIdSdpRTPMapAttribute;
  try
    if not Self.IsAlphaNumeric(Att.Name) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPAttributeName,
                                        Name + '=' + OriginalValue]));

    if (Att.Value <> '') and not Self.IsByteString(Att.Value) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPAttributeName,
                                        Name + '=' + OriginalValue]));

    RTPMapAttributes.Add(Att);
  finally
    Att.Free;
  end;
end;

procedure TIdSdpParser.ParseSessionHeaders(Payload: TIdSdpPayload);
begin
  Self.ParsingSessionHeaders := true;

  Self.ParseVersion(Payload);
  Self.ParseOrigin(Payload);
  Self.ParseSessionName(Payload);
  Self.ParseSessionOptionalHeaders(Payload);

  Self.ParsingSessionHeaders := false;
end;

procedure TIdSdpParser.ParseSessionOptionalHeaders(Payload: TIdSdpPayload);
var
  NextHeader: String;
begin
  NextHeader := Self.PeekLine;
  while not Self.Eof
    and (NextHeader <> '')
    and (NextHeader[1] <> RSSDPMediaDescriptionName) do begin

    case NextHeader[1] of
      RSSDPAttributeName:
        if (Pos(RTPMapAttribute, NextHeader) > 0) then
          Self.ParseRTPMapAttribute(Payload.RTPMapAttributes)
        else
          Self.ParseAttribute(Payload.Attributes);
      RSSDPBandwidthName:   Self.ParseBandwidth(Payload.Bandwidths);
      RSSDPConnectionName:  Self.ParseConnection(Payload.Connections);
      RSSDPEmailName:       Self.ParseEmail(Payload);
      RSSDPKeyName:         Self.ParseKey(Payload.Key);
      RSSDPInformationName: Self.ParseInfo(Payload);
      RSSDPPhoneName:       Self.ParsePhone(Payload);
      RSSDPTimeName:        Self.ParseTime(Payload);
      RSSDPUriName:         Self.ParseUri(Payload);
    else
      raise EParserError.Create(Format(UnknownOptionalHeader, [NextHeader]));
    end;

    NextHeader := Self.PeekLine;
  end;
end;

procedure TIdSdpParser.ParseSessionName(Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.ParseHeader(Name, Value);

  if (Name <> RSSDPSessionName) then
    raise EParserError.Create(MissingSessionName);

  if not Self.IsText(Value) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPSessionName,
                                      Name + '=' + Value]));

  Payload.SessionName := Value;
  Self.LastSessionHeader := RSSDPSessionName;
end;

procedure TIdSdpParser.ParseTime(Payload: TIdSdpPayload);
var
  Name:          String;
  OriginalValue: String;
  Time:          TIdSdpTime;
  Token:         String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

  Time := TIdSdpTime.Create;
  try
    Token := Fetch(Value, ' ');
    if not Self.IsNumber(Token) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPTimeName,
                                        Name + '=' + OriginalValue]));
    Time.StartTime := StrToInt64(Token);

    Token := Fetch(Value, ' ');
    if not Self.IsNumber(Token) then
      raise EParserError.Create(Format(MalformedToken,
                                       [RSSDPTimeName,
                                        Name + '=' + OriginalValue]));
    Time.EndTime := StrToInt64(Token);

    while not Self.Eof and (Copy(Self.PeekLine, 1, 1) = RSSDPZoneAdjustmentName) do
      Self.ParseZoneAdjustment(Time);

    while not Self.Eof and (Copy(Self.PeekLine, 1, 1) = RSSDPRepeatName) do
      Self.ParseRepeat(Time);

    Payload.Times.Add(Time);
  except
    if not Payload.Times.Contains(Time) then
      Time.Free;

    raise;
  end;

  Self.LastSessionHeader := RSSDPTimeName;
end;

procedure TIdSdpParser.ParseZoneAdjustment(Time: TIdSdpTime);
var
  Name:          String;
  OriginalValue: String;
  Zone:          TIdSdpZoneAdjustment;
  Token:         String;
  Value:         String;
begin
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  Zone := TIdSdpZoneAdjustment.Create;
  try
    Zone.Value := Value;

    while (Value <> '') do begin
      Token := Fetch(Value, ' ');

//      if not Self.IsZoneAdjustment(Token) then
//        raise EParserError.Create(Format(MalformedToken,
//                                         [RSSDPRepeatName,
//                                          Name + '=' + OriginalValue]));
    end;

    Time.ZoneAdjustments.Add(Zone);
  except
    if not Time.ZoneAdjustments.Contains(Zone) then
      Zone.Free;

    raise;
  end;
end;

procedure TIdSdpParser.ParseURI(Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

//  if not Self.IsUri(Value) then
//    raise EParserError.Create(Format(MalformedToken,
//                                     [RSSDPUriName,
//                                      Name + '=' + Value]));

  Payload.URI := Value;
  Self.LastSessionHeader := RSSDPUriName;
end;

procedure TIdSdpParser.ParseVersion(Payload: TIdSdpPayload);
var
  E:     Integer;
  N:     Cardinal;
  Name:  String;
  Value: String;
begin
  if Self.Eof then
    raise EParserError.Create(EmptyInputStream);

  Self.ParseHeader(Name, Value);

  if (Name <> RSSDPVersionName) then
    raise EParserError.Create(MissingVersion);

  Val(Value, N, E);

  if (E <> 0) then
    raise EParserError.Create(Format(MalformedToken,
                                     [RSSDPVersionName,
                                      Name + '=' + Value]));

  Payload.Version := N;
  Self.LastSessionHeader := RSSDPVersionName;
end;

//******************************************************************************
//* TIdSdpBaseMediaStream                                                      *
//******************************************************************************
//* TIdSdpBaseMediaStream Public methods ***************************************

constructor TIdSdpBaseMediaStream.Create(Factory: TIdSdpMediaStreamFactory);
begin
  inherited Create;

  Self.Factory := Factory;

  Self.InternalCreate;
end;

destructor TIdSdpBaseMediaStream.Destroy;
begin
  Self.RemoteDescription.Free;
  Self.LocalDescription.Free;
  Self.DataSendListeners.Free;
  Self.DataListeners.Free;

  inherited Destroy;
end;

procedure TIdSdpBaseMediaStream.AddDataListener(const Listener: IIdSdpMediaListener);
begin
  Self.DataListeners.AddListener(Listener);
end;

procedure TIdSdpBaseMediaStream.AddDataSendListener(const Listener: IIdSdpMediaSendListener);
begin
  Self.DataSendListeners.AddListener(Listener);
end;

function TIdSdpBaseMediaStream.AllowedPort(Port: TPortNum): Boolean;
begin
  Result := (Self.LowestAllowedPort <= Port) and (Port <= Self.HighestAllowedPort);
end;

function TIdSdpBaseMediaStream.IsListening: Boolean;
begin
  Result := false;
end;

function TIdSdpBaseMediaStream.IsNull: Boolean;
begin
  // Null streams are streams that have been rejected (indicated by a zero port
  // in the SDP media description).
  Result := false;
end;

function TIdSdpBaseMediaStream.IsReceiver: Boolean;
begin
  Result := Self.LocalDescription.Attributes.Direction in [sdRecvOnly, sdSendRecv];
end;

function TIdSdpBaseMediaStream.IsRefusedStream: Boolean;
begin
  Result := Self.LocalDescription.IsRefusedStream or Self.RemoteDescription.IsRefusedStream;
end;

function TIdSdpBaseMediaStream.IsSender: Boolean;
begin
  Result := Self.LocalDescription.Attributes.Direction in [sdSendOnly, sdSendRecv];
end;

function TIdSdpBaseMediaStream.IsText: Boolean;
begin
  // We use OR here, because one of the local or remote descriptions might not
  // be set.

  Result := Self.LocalDescription.IsText or Self.RemoteDescription.IsText;
end;

procedure TIdSdpBaseMediaStream.JoinSession;
begin
  // Lose this ASAP - it only makes sense for RTP streams.
end;

procedure TIdSdpBaseMediaStream.PutOnHold;
begin
  if not Self.OnHold then begin
    Self.PreHoldDirection := Self.Direction;
    case Self.Direction of
      sdRecvOnly: Self.Direction := sdInactive;
      sdSendRecv: Self.Direction := sdSendOnly;
    end;
    Self.fOnHold := true;
  end;
end;

procedure TIdSdpBaseMediaStream.RemoveDataListener(const Listener: IIdSdpMediaListener);
begin
  Self.DataListeners.RemoveListener(Listener);
end;

procedure TIdSdpBaseMediaStream.RemoveDataSendListener(const Listener: IIdSdpMediaSendListener);
begin
  Self.DataSendListeners.RemoveListener(Listener);
end;

procedure TIdSdpBaseMediaStream.SendData(Data: TStream; Format: String; LayerID: Integer = 0);
begin
  if Self.IsSender and not Self.OnHold then
    Self.ReallySendData(Data, Format, LayerID);
end;

procedure TIdSdpBaseMediaStream.StartListening;
begin
  // Subclasses implement this.
end;

procedure TIdSdpBaseMediaStream.StopListening;
begin
  // Subclasses implement this.
end;

procedure TIdSdpBaseMediaStream.TakeOffHold;
begin
  if Self.OnHold then begin
    Self.Direction := Self.PreHoldDirection;
    Self.fOnHold   := false;
  end;
end;

function TIdSdpBaseMediaStream.UsesBinding(Binding: TIdConnectionBindings): Boolean;
begin
  Result := Self.LocalDescription.UsesBinding(Binding);
end;

//* TIdSdpBaseMediaStream Protected methods ************************************

procedure TIdSdpBaseMediaStream.AfterSetLocalDescription(Value: TIdSdpMediaDescription);
begin
  // Any initialisation operations associated with changing the local session
  // description go here.
  //
  // By default do nothing.
end;

procedure TIdSdpBaseMediaStream.AfterSetRemoteDescription(Value: TIdSdpMediaDescription);
begin
  // Any initialisation operations associated with changing the remote session
  // description go here.
  //
  // By default do nothing.
end;

procedure TIdSdpBaseMediaStream.BeforeSetLocalDescription(Value: TIdSdpMediaDescription);
begin
  // Any cleanup operations associated with changing the local session
  // description go here.
  //
  // By default do nothing.
end;

procedure TIdSdpBaseMediaStream.BeforeSetRemoteDescription(Value: TIdSdpMediaDescription);
begin
  // Any cleanup operations associated with changing the remote session
  // description go here.
  //
  // By default do nothing.
end;

procedure TIdSdpBaseMediaStream.BindListeningStreams;
  procedure SetPort(Port: TPortNum);
  begin
    Self.LocalDescription.Port := Port;
    Self.InitializeLocalServers;
  end;
var
  SocketBound: Boolean;
begin
  if (Self.HighestAllowedPort < Self.LowestAllowedPort) then begin
    Self.LocalDescription.RefuseStream;
    Exit;
  end;

  if not Self.SufficientPortsFree(Self.LocalDescription.Port) then
    SetPort(Self.LowestAllowedPort);

  SocketBound := false;
  while not SocketBound and Self.SufficientPortsFree(Self.LocalDescription.Port) do begin
    try
      Self.StartServers;
      SocketBound := true;
    except
      on EIdCouldNotBindSocket do
        SetPort(Self.NextPort(Self.LocalDescription.Port));
      on EIdSocketError do
        SetPort(Self.NextPort(Self.LocalDescription.Port));
    end;
  end;

  // If the stream doesn't bind to a port, we indicate that we won't be using
  // the stream.
  if not SocketBound then
    Self.LocalDescription.RefuseStream;
end;

function TIdSDPBaseMediaStream.GetPort(Index: Integer): TPortNum;
begin
  Result := 0;
  RaiseAbstractError(Self.ClassName, 'GetPort');
end;

procedure TIdSdpBaseMediaStream.InitializeLocalServers;
begin
  RaiseAbstractError(Self.ClassName, 'InitializeLocalServers');
end;

procedure TIdSdpBaseMediaStream.InternalCreate;
begin
  Self.DataListeners      := TIdNotificationList.Create;
  Self.DataSendListeners  := TIdNotificationList.Create;
  Self.fIsOffer           := true;
  Self.fLocalDescription  := TIdSdpMediaDescription.Create;
  Self.fOnHold            := false;
  Self.fRemoteDescription := TIdSdpMediaDescription.Create;
  Self.LowestAllowedPort  := LowestPossiblePort;
  Self.HighestAllowedPort := HighestPossiblePort;
end;

function TIdSdpBaseMediaStream.NextPort(PreviousPort: TPortNum): TPortNum;
begin
  // Return the next port to use for this kind of stream. Some streams require
  // multiple ports!
  Result := PreviousPort + 1;
end;

procedure TIdSdpBaseMediaStream.NotifyOfData(Binding: TIdConnectionBindings; Data: TStream; Format: String);
var
  Notification: TIdSdpMediaListenerOnDataMethod;
begin
  if Self.IsReceiver then begin
    Notification := TIdSdpMediaListenerOnDataMethod.Create;
    try
      Notification.Binding := Binding;
      Notification.Chunk   := Data;
      Notification.Format  := Format;
      Notification.Stream  := Self;

      Self.DataListeners.Notify(Notification);
    finally
      Notification.Free;
    end;
  end;
end;

procedure TIdSdpBaseMediaStream.NotifyOfSentData(Data: TStream; Format: String; LayerID: Integer);
var
  Notification: TIdSdpMediaListenerOnSentDataMethod;
begin
  Notification := TIdSdpMediaListenerOnSentDataMethod.Create;
  try
    Notification.Chunk   := Data;
    Notification.Format  := Format;
    Notification.LayerID := LayerID;
    Notification.Stream  := Self;

    Self.DataSendListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSdpBaseMediaStream.ReallySendData(Data: TStream; Format: String; LayerID: Integer = 0);
begin
end;

function TIdSdpBaseMediaStream.RequiredPorts: Cardinal;
begin
  // Return the number of ports that this stream requires.
  Result := Self.LocalDescription.PortCount;
end;

procedure TIdSdpBaseMediaStream.SetTimer(Value: TIdTimerQueue);
begin
  Self.fTimer := Value;
end;

procedure TIdSdpBaseMediaStream.StartServers;
begin
  RaiseAbstractError(Self.ClassName, 'StartServers');
end;

//* TIdSdpBaseMediaStream Private methods **************************************

function TIdSDPBaseMediaStream.GetDirection: TIdSdpDirection;
begin
  Result := Self.LocalDescription.Attributes.Direction;
end;

function TIdSDPBaseMediaStream.GetPortCount: Cardinal;
begin
  Result := Self.LocalDescription.PortCount;
end;

procedure TIdSDPBaseMediaStream.SetDirection(Value: TIdSdpDirection);
begin
  Self.LocalDescription.Attributes.Direction := Value;
end;

procedure TIdSdpBaseMediaStream.SetLocalDescription(Value: TIdSdpMediaDescription);
begin
  Assert(Value.PortCount > 0, 'You have to have a PortCount of at least 1.');

  Self.BeforeSetLocalDescription(Value);

  Self.fLocalDescription.Assign(Value);

  Self.AfterSetLocalDescription(Value);
end;

procedure TIdSdpBaseMediaStream.SetRemoteDescription(const Value: TIdSdpMediaDescription);
begin
  Self.BeforeSetRemoteDescription(Value);

  Self.fRemoteDescription.Assign(Value);

  Self.AfterSetRemoteDescription(Value);
end;

function TIdSdpBaseMediaStream.SufficientPortsFree(Port: TPortNum): Boolean;
begin
  // Return true if there are enough ports free between Port and
  // HighestAllowedPort to start this stream. RTP, for instance, requires two
  // ports, while TCP requires only one.
  Result := Self.AllowedPort(Port + Self.RequiredPorts - 1);
end;

//******************************************************************************
//* TIdSDPMediaStream                                                          *
//******************************************************************************
//* TIdSDPMediaStream Public methods *******************************************

destructor TIdSDPMediaStream.Destroy;
begin
  Self.StopListening;

  Self.Servers.Free;
  Self.RTPSendListeners.Free;
  Self.RTPListeners.Free;

  Self.RemoteProfile.Free;
  Self.LocalProfile.Free;

  inherited Destroy;
end;

procedure TIdSDPMediaStream.AddRTPListener(const Listener: IIdRTPListener);
begin
  Self.RTPListeners.AddListener(Listener);
end;

procedure TIdSDPMediaStream.AddRTPSendListener(const Listener: IIdRTPSendListener);
begin
  Self.RTPSendListeners.AddListener(Listener);
end;

function TIdSDPMediaStream.IsListening: Boolean;
begin
  Result := (Self.Servers.Count > 0) and Self.ServerAt(0).Active;
end;

procedure TIdSDPMediaStream.JoinSession;
var
  I: Integer;
begin
  for I := 0 to Self.Servers.Count - 1 do
    Self.ServerAt(I).Session.JoinSession;
end;

function TIdSDPMediaStream.MatchPort(Port: TPortNum): Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to Self.Servers.Count - 1 do
    if (Self.ServerAt(I).RTPPort = Port) then begin
      Result := true;
      Break;
    end;
end;

procedure TIdSDPMediaStream.RemoveRTPListener(const Listener: IIdRTPListener);
begin
  Self.RTPListeners.RemoveListener(Listener);
end;

procedure TIdSDPMediaStream.RemoveRTPSendListener(const Listener: IIdRTPSendListener);
begin
  Self.RTPSendListeners.RemoveListener(Listener);
end;

procedure TIdSDPMediaStream.StartListening;
begin
  if Self.LocalDescription.IsRefusedStream then Exit;

  Self.BindListeningStreams;
end;

procedure TIdSDPMediaStream.StopListening;
var
  I: Integer;
begin
  for I := 0 to Self.Servers.Count - 1 do
    if Self.ServerAt(I).Active then begin
      Self.ServerAt(I).Session.LeaveSession('Goodbye');
      Self.ServerAt(I).Active := false;
    end;
end;

//* TIdSDPMediaStream Protected methods ****************************************

procedure TIdSDPMediaStream.AfterSetLocalDescription(Value: TIdSdpMediaDescription);
begin
  Self.RegisterEncodingMaps(Self.LocalProfile,
                            Value.RTPMapAttributes);

  Self.InitializeLocalServers;
end;

procedure TIdSDPMediaStream.AfterSetRemoteDescription(Value: TIdSdpMediaDescription);
begin
  if Value.IsRefusedStream then begin
    Self.RecreateServers(0);
  end
  else begin
    Self.RegisterEncodingMaps(Self.RemoteProfile,
                              Value.RTPMapAttributes);

    Self.InitializeRemoteServers;
  end;
end;

procedure TIdSDPMediaStream.BeforeSetLocalDescription(Value: TIdSdpMediaDescription);
begin
  Self.UnregisterEncodingMaps(Self.LocalProfile,
                              Self.LocalDescription.RTPMapAttributes);
end;

procedure TIdSDPMediaStream.BeforeSetRemoteDescription(Value: TIdSdpMediaDescription);
begin
//  Self.UnregisterEncodingMaps(Self.RemoteProfile,
//                              Self.RemoteDescription.RTPMapAttributes);
end;

function TIdSDPMediaStream.GetPort(Index: Integer): TPortNum;
begin
  Result := Self.ServerAt(Index).RTPPort;
end;

procedure TIdSDPMediaStream.InternalCreate;
begin
  inherited InternalCreate;

  Self.fLocalProfile  := TIdRTPProfile.Create;
  Self.fRemoteProfile := TIdRTPProfile.Create;

  Self.RTPListeners     := TIdNotificationList.Create;
  Self.RTPSendListeners := TIdNotificationList.Create;
  Self.Servers          := TObjectList.Create(true);
end;

function TIdSDPMediaStream.NextPort(PreviousPort: TPortNum): TPortNum;
begin
  Result := PreviousPort + 2; // One for the RTP socket, one for the RTCP socket.
end;

procedure TIdSDPMediaStream.ReallySendData(Data: TStream; Format: String; LayerID: Integer = 0);
var
  Payload: TIdRTPPayload;
  Wait:    TIdRTPSendDataWait;
begin
  Payload := Self.LocalProfile.EncodingFor(StrToInt(Format)).Copy;
  try
    Payload.ReadFrom(Data);
    Payload.StartTime := Now;

    Wait := TIdRTPSendDataWait.Create;
    Wait.Data      := Payload.Copy;
    Wait.SessionID := Self.FindServer(LayerID).Session.ID;

    Self.Timer.AddEvent(TriggerImmediately, Wait);

    Self.NotifyOfSentData(Data, Format, LayerID);
  finally
    Payload.Free;
  end;
end;

function TIdSDPMediaStream.RequiredPorts: Cardinal;
begin
  // One port for RTP, one for RTCP.
  Result := Self.LocalDescription.PortCount * 2;
end;

procedure TIdSDPMediaStream.SetTimer(Value: TIdTimerQueue);
var
  I: Integer;
begin
  inherited SetTimer(Value);

  for I := 0 to Self.Servers.Count - 1 do
    Self.ServerAt(I).Timer := Value;
end;

procedure TIdSDPMediaStream.StartServers;
var
  I: Integer;
begin
  for I := 0 to Self.Servers.Count - 1 do
    Self.ServerAt(I).Active := true;
end;

//* TIdSDPMediaStream Private methods ******************************************

function TIdSDPMediaStream.CreateServer: TIdBaseRTPAbstractPeer;
begin
  Result := Self.Factory.RtpServerType.Create;
  Self.Servers.Add(Result);

  Result.AddListener(Self);
  Result.LocalProfile  := Self.LocalProfile;
  Result.RemoteProfile := Self.RemoteProfile;
  Result.Session.AddListener(Self);
  Result.AddSendListener(Self);
  Result.Timer := Self.Timer;
end;

function TIdSDPMediaStream.FindServer(LayerID: Cardinal): TIdBaseRTPAbstractPeer;
var
  I: Integer;
begin
  // LayerID denotes a layer in an hierarchically encoded stream.
  Result := nil;
  I      := 0;

  while (Result = nil) and (I < Self.Servers.Count) do begin
    if (Self.ServerAt(I).RTPPort = LayerID) then
      Result := Self.ServerAt(I);
    Inc(I);
  end;

  if (Result = nil) then
    Result := Self.ServerAt(0);
end;

procedure TIdSDPMediaStream.InitializeLocalServers;
var
  I:           Cardinal;
  Server:      TIdBaseRTPAbstractPeer;
  ServerCount: Cardinal;
begin
  // Given our local session description, instantiate the RTP servers we need.

  Self.StopListening;

  ServerCount := Self.Servers.Count;

  if (ServerCount <> Self.LocalDescription.PortCount) then
    Self.RecreateServers(Self.LocalDescription.PortCount);

  for I := 0 to Self.LocalDescription.PortCount - 1 do begin
    Server := Self.ServerAt(I);
    Server.Address  := Self.LocalDescription.Connections[0].Address;
    Server.RTPPort  := Self.LocalDescription.Port + 2*I;
    Server.RTCPPort := Server.RTPPort + 1;
  end;

//  if AlreadyRunning then
//    Self.StartListening;
end;

procedure TIdSDPMediaStream.InitializeRemoteServers;
var
  I:           Cardinal;
  Peer:        TIdRTPMember;
  ServerCount: Cardinal;
begin
  if (Self.RemoteDescription.PortCount = 0) then Exit;

  ServerCount := Self.Servers.Count;
  if (ServerCount <> Self.RemoteDescription.PortCount) then
    Self.RecreateServers(Self.RemoteDescription.PortCount);

  // We ASSUME that the local & remote descriptions are symmetrical: that, for
  // this stream, both ports have the same port count.
  // THIS IS NOT SUCH A GREAT IDEA. TODO.
  for I := 0 to Self.RemoteDescription.PortCount - 1 do begin
    Peer := Self.ServerAt(I).Session.AddReceiver(Self.RemoteDescription.Connections[0].Address,
                                                 Self.RemoteDescription.Port + 2*I);
    Peer.ControlAddress := Self.RemoteDescription.Connections[0].Address;
    Peer.ControlPort    := Peer.SourcePort + 1;
  end;
end;

procedure TIdSDPMediaStream.OnNewData(Data: TIdRTPPayload;
                                      Binding: TIdConnectionBindings);
var
  Chunk:       TStream;
  ReceivedOn: TIdConnectionBindings;
begin
  ReceivedOn := TIdConnectionBindings.Create;
  try
    ReceivedOn.LocalIP   := Binding.LocalIP;
    ReceivedOn.LocalPort := Binding.LocalPort;
    ReceivedOn.PeerIP    := Binding.PeerIP;
    ReceivedOn.PeerPort  := Binding.PeerPort;
    ReceivedOn.Transport := Self.LocalDescription.Protocol;

    Chunk := TMemoryStream.Create;
    try
      // TODO This might not be correct! (A TIdRTPPayload might, in its PrintOn,
      // print metadata or framing information or something silly.
      // TIdRTPT140Payloads without redundancy write just a chunk of text, which
      // is fine, but if the payload uses redundancy, naively printing out the
      // below will result in those previous generations of text being printed
      // out.)

      Data.PrintOn(Chunk);
      Chunk.Seek(soFromBeginning, 0);

      Self.NotifyOfData(ReceivedOn, Chunk, IntToStr(Self.RemoteProfile.PayloadTypeFor(Data)));
    finally
      Chunk.Free;
    end;
  finally
    ReceivedOn.Free;
  end;
end;

procedure TIdSDPMediaStream.OnRTCP(Packet: TIdRTCPPacket;
                                   Binding: TIdConnectionBindings);
var
  Notification: TIdRTPListenerReceiveRTCPMethod;
begin
  Notification := TIdRTPListenerReceiveRTCPMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Packet  := Packet;

    Self.RTPListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSDPMediaStream.OnRTP(Packet: TIdRTPPacket;
                                  Binding: TIdConnectionBindings);
var
  Notification: TIdRTPListenerReceiveRTPMethod;
begin
  Notification := TIdRTPListenerReceiveRTPMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Packet  := Packet;

    Self.RTPListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSDPMediaStream.OnSendRTCP(Packet: TIdRTCPPacket;
                                       Binding: TIdConnectionBindings);
var
  Notification: TIdRTPSendListenerSendRTCPMethod;
begin
  Notification := TIdRTPSendListenerSendRTCPMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Packet  := Packet;

    Self.RTPSendListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSDPMediaStream.OnSendRTP(Packet: TIdRTPPacket;
                                      Binding: TIdConnectionBindings);
var
  Notification: TIdRTPSendListenerSendRTPMethod;
begin
  Notification := TIdRTPSendListenerSendRTPMethod.Create;
  try
    Notification.Binding := Binding;
    Notification.Packet  := Packet;

    Self.RTPSendListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSDPMediaStream.RecreateServers(NumberOfServers: Cardinal);
var
  I: Integer;
begin
  Self.Servers.Clear;

  for I := 1 to NumberOfServers do
    Self.CreateServer;
end;

procedure TIdSDPMediaStream.RegisterEncodingMaps(Profile: TIdRTPProfile;
                                                 Maps: TIdSdpRTPMapAttributes);
var
  I: Integer;
begin
  for I := 0 to Maps.Count - 1 do
    Profile.AddEncoding(Maps.Items[I].Encoding, StrToInt(Maps.Items[I].Format));
end;

function TIdSDPMediaStream.ServerAt(Index: Integer): TIdBaseRTPAbstractPeer;
begin
  Result := Self.Servers[Index] as TIdBaseRTPAbstractPeer;
end;

procedure TIdSDPMediaStream.SetLocalProfile(Value: TIdRTPProfile);
begin
  Self.LocalProfile.Assign(Value);
end;

procedure TIdSDPMediaStream.SetRemoteProfile(Value: TIdRTPProfile);
begin
  Self.RemoteProfile.Assign(Value);
end;

procedure TIdSDPMediaStream.UnregisterEncodingMaps(Profile: TIdRTPProfile;
                                                   Maps: TIdSdpRTPMapAttributes);
var
  I:    Integer;
  Null: TIdNullPayload;
begin
  Null := TIdNullPayload.Create;
  try
    for I := 0 to Maps.Count - 1 do
      Profile.AddEncoding(Null, StrToInt(Maps.Items[I].Format));
  finally
    Null.Free;
  end;
end;

//******************************************************************************
//* TIdSdpNullMediaStream                                                      *
//******************************************************************************
//* TIdSdpNullMediaStream Public methods ***************************************

function TIdSdpNullMediaStream.IsNull: Boolean;
begin
  Result := true;
end;

//* TIdSdpNullMediaStream Protected methods ************************************

procedure TIdSdpNullMediaStream.AfterSetLocalDescription(Value: TIdSdpMediaDescription);
begin
  Self.LocalDescription.Port := RefusedPort;
end;

//******************************************************************************
//* TIdSdpBaseTcpConnection                                                    *
//******************************************************************************
//* TIdSdpBaseTcpConnection Public methods *************************************

class function TIdSdpBaseTcpConnection.ClientConnectionType: TIdSdpBaseTcpConnectionClass;
begin
  Result := TIdSdpTcpClientConnection;
end;

class function TIdSdpBaseTcpConnection.NullConnectionType: TIdSdpBaseTcpConnectionClass;
begin
  Result := TIdSdpTcpNullConnection;
end;

class function TIdSdpBaseTcpConnection.ServerConnectionType: TIdSdpBaseTcpConnectionClass;
begin
  Result := TIdSdpTcpServerConnection;
end;

constructor TIdSdpBaseTcpConnection.Create;
begin
  inherited Create;

  Self.DataListeners := TIdNotificationList.Create;
  Self.Timeout       := FiveSeconds;
end;

destructor TIdSdpBaseTcpConnection.Destroy;
begin
  Self.DataListeners.Free;

  inherited Destroy;
end;

procedure TIdSdpBaseTcpConnection.AddDataListener(Listener: IIdSdpTcpConnectionListener);
begin
  Self.DataListeners.AddListener(Listener);
end;

procedure TIdSdpBaseTcpConnection.ConnectTo(PeerAddress: String; PeerPort: TPortNum);
begin
  RaiseAbstractError(Self.ClassName, 'ConnectTo');
end;

procedure TIdSdpBaseTcpConnection.Disconnect;
begin
  RaiseAbstractError(Self.ClassName, 'Disconnect');
end;

function TIdSdpBaseTcpConnection.IsActive: Boolean;
begin
  // Return true if this connection is in the process of establishing a
  // connection. For instance, a client connection might have started connecting
  // to a server, but hasn't yet sent an ACK. Or, a server connection is
  // listening on a socket, but a client has not yet connected to it.
  Result := false;
  RaiseAbstractError(Self.ClassName, 'IsActive');
end;

function TIdSdpBaseTcpConnection.IsConnected: Boolean;
begin
  // A server connection starts listening, so is active, but until a client
  // connects, the connection doesn't really exist. When that client actually
  // connects (and the connection is hence fully established), this function
  // returns true.

  Result := false;
  RaiseAbstractError(Self.ClassName, 'IsConnected');
end;

function TIdSdpBaseTcpConnection.IsServer: Boolean;
begin
  // Any answer here is wrong, so we just default to saying "no, we're a
  // client".
  Result := false;
end;

procedure TIdSdpBaseTcpConnection.ListenOn(Address: String; Port: TPortNum);
begin
  RaiseAbstractError(Self.ClassName, 'ListenOn');
end;

procedure TIdSdpBaseTcpConnection.ReceiveData(Data: TStream; ReceivedOn: TIdConnectionBindings);
var
  Notification: TIdSdpTcpConnectionOnDataMethod;
begin
  // Typically this executes in the context of a TimerQueue.

  Notification := TIdSdpTcpConnectionOnDataMethod.Create;
  try
    Notification.Connection := Self;
    Notification.Data       := Data;

    Self.DataListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSdpBaseTcpConnection.RemoveDataListener(Listener: IIdSdpTcpConnectionListener);
begin
  Self.DataListeners.RemoveListener(Listener);
end;

procedure TIdSdpBaseTcpConnection.SendData(Data: TStream);
begin
  // This executes in the context of a TimerQueue; see
  // TIdSdpTcpMediaStream.ReallySendData.

  RaiseAbstractError(Self.ClassName, 'SendData');
end;

//* TIdSdpBaseTcpConnection Protected methods **********************************

function TIdSdpBaseTcpConnection.GetAddress: String;
begin
  Result := '';
  RaiseAbstractError(Self.ClassName, 'GetAddress');
end;

function TIdSdpBaseTcpConnection.GetPeerAddress: String;
begin
  Result := '';
  RaiseAbstractError(Self.ClassName, 'GetPeerAddress');
end;

function TIdSdpBaseTcpConnection.GetPeerPort: TPortNum;
begin
  Result := 0;
  RaiseAbstractError(Self.ClassName, 'GetPeerPort');
end;

function TIdSdpBaseTcpConnection.GetPort: TPortNum;
begin
  Result := 0;
  RaiseAbstractError(Self.ClassName, 'GetPort');
end;

procedure TIdSdpBaseTcpConnection.NotifyOfConnection;
var
  Notification: TIdSdpTcpConnectionOnConnectMethod;
begin
  // This executes in the context of a TimerQueue.

  Notification := TIdSdpTcpConnectionOnConnectMethod.Create;
  try
    Notification.Connection := Self;

    Self.DataListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSdpBaseTcpConnection.NotifyOfDisconnection;
var
  Notification: TIdSdpTcpConnectionOnDisconnectMethod;
begin
  // This executes in the context of a TimerQueue.

  Notification := TIdSdpTcpConnectionOnDisconnectMethod.Create;
  try
    Notification.Connection := Self;

    Self.DataListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSdpBaseTcpConnection.NotifyOfException(ExceptionType: ExceptClass;
                                                    ExceptionMessage: String);
var
  Notification: TIdSdpTcpConnectionOnExceptionMethod;
begin
  // This executes in the context of a TimerQueue.

  Notification := TIdSdpTcpConnectionOnExceptionMethod.Create;
  try
    Notification.Connection       := Self;
    Notification.ExceptionType    := ExceptionType;
    Notification.ExceptionMessage := ExceptionMessage;

    Self.DataListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSdpBaseTcpConnection.RaiseSocketError(Msg: String);
begin
  // Make it look like this is a socket error; in a sense, it is!

  raise EIdSocketError.Create(Msg);
end;

procedure TIdSdpBaseTcpConnection.ScheduleNotification(WaitTime: Cardinal; Wait: TIdWait);
begin
  if Assigned(Self.Timer) then
    Self.Timer.AddEvent(TriggerImmediately, Wait);
end;

procedure TIdSdpBaseTcpConnection.SetTimer(Value: TIdTimerQueue);
begin
  Self.fTimer := Value;
end;

//******************************************************************************
//* TIdSdpTcpConnectionRegistry                                                *
//******************************************************************************
//* TIdSdpTcpConnectionRegistry Public methods *********************************

class function TIdSdpTcpConnectionRegistry.Singleton: TIdSdpTcpConnectionRegistry;
begin
  Result := GSdpTcpConnectionRegistry;
end;

function TIdSdpTcpConnectionRegistry.ClientConnectedTo(Host: String; Port: TPortNum): TIdSdpBaseTcpConnection;
var
  I:      Integer;
  L:      TStrings;
  Client: TIdSdpBaseTcpConnection;
begin
  // Return the (first) connection that's connected to Host:Port, whether active or
  // not.
  Result := nil;

  L := Self.GetAllConnections;
  try
    for I := 0 to L.Count - 1 do begin
      Client := L.Objects[I] as TIdSdpBaseTcpConnection;
      if (Client.PeerAddress = Host) and (Client.PeerPort = Port) then begin
        Result := Client;
        Break;
      end;
    end;
  finally
    L.Free;
  end;
end;

function TIdSdpTcpConnectionRegistry.FindConnection(const ServerID: TRegisteredObjectID): TIdSdpBaseTcpConnection;
var
  O: TObject;
begin
  O := TIdObjectRegistry.Singleton.FindObject(ServerID);

  if not Assigned(O) then
    raise ERegistry.Create(OidAsString(ServerID) + ' does not point to a registered object');

  if not (O is TIdSdpBaseTcpConnection) then
    raise ERegistry.Create(OidAsString(ServerID) + ' points to a ' + O.ClassName + ', not a ' + TIdSdpBaseTcpConnection.ClassName);

  Result := O as TIdSdpBaseTcpConnection;
end;

function TIdSdpTcpConnectionRegistry.ServerOn(Host: String; Port: TPortNum): TIdSdpBaseTcpConnection;
var
  I:      Integer;
  L:      TStrings;
  Server: TIdSdpBaseTcpConnection;
begin
  // Return the (first) server that's running on Host:Port, whether active or
  // not.
  Result := nil;

  L := Self.GetAllConnections;
  try
    for I := 0 to L.Count - 1 do begin
      Server := L.Objects[I] as TIdSdpBaseTcpConnection;
      if (Server.Address = Host) and (Server.Port = Port) then begin
        Result := Server;
        Break;
      end;
    end;
  finally
    L.Free;
  end;
end;

function TIdSdpTcpConnectionRegistry.ServerRunningOn(Host: String; Port: TPortNum): Boolean;
var
  I:      Integer;
  L:      TStrings;
  Server: TIdSdpBaseTcpConnection;
begin
  // Return true if a TIdSdpBaseTcpConnection is running on Host:Port.
  Result := false;

  L := Self.GetAllConnections;
  try
    for I := 0 to L.Count - 1 do begin
      Server := L.Objects[I] as TIdSdpBaseTcpConnection;

    if (Server.Address = Host) and (Server.Port = Port) and Server.IsActive then begin
      Result := true;
      Break;
    end;
    end;
  finally
    L.Free;
  end;
end;

//* TIdSdpTcpConnectionRegistry Private methods ********************************

function TIdSdpTcpConnectionRegistry.GetAllConnections: TStrings;
begin
  Result := TStringList.Create;

  TIdObjectRegistry.Singleton.CollectAllObjectsOfClass(TIdSdpBaseTcpConnection, Result);
end;

//******************************************************************************
//* TIdSdpTcpNullConnection                                                    *
//******************************************************************************
//* TIdSdpTcpNullConnection Public methods *************************************

procedure TIdSdpTcpNullConnection.ConnectTo(PeerAddress: String; PeerPort: TPortNum);
begin
  // Do nothing.
end;

procedure TIdSdpTcpNullConnection.SendData(Data: TStream);
begin
  // Do nothing.
end;

procedure TIdSdpTcpNullConnection.Disconnect;
begin
  // Do nothing.
end;

function TIdSdpTcpNullConnection.IsActive: Boolean;
begin
  Result := false;
end;

function TIdSdpTcpNullConnection.IsConnected: Boolean;
begin
  Result := false;
end;

procedure TIdSdpTcpNullConnection.ListenOn(Address: String; Port: TPortNum);
begin
  // Do nothing.
end;

procedure TIdSdpTcpNullConnection.ReceiveData(Data: TStream; ReceivedOn: TIdConnectionBindings);
begin
  // Do nothing.
end;

//* TIdSdpTcpNullConnection Protected methods **********************************

function TIdSdpTcpNullConnection.GetAddress: String;
begin
  Result := IPv4ZeroAddress;
end;

function TIdSdpTcpNullConnection.GetPeerAddress: String;
begin
  Result := IPv4ZeroAddress;
end;

function TIdSdpTcpNullConnection.GetPeerPort: TPortNum;
begin
  Result := TcpDiscardPort;
end;

function TIdSdpTcpNullConnection.GetPort: TPortNum;
begin
  Result := TcpDiscardPort;
end;

//******************************************************************************
//* TIdSdpTcpClient                                                            *
//******************************************************************************
//* TIdSdpTcpClient Public methods *********************************************

constructor TIdSdpTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.BufferSize  := 100;
  Self.ReadTimeout := OneSecond;
end;

procedure TIdSdpTcpClient.ReceiveMessages;
var
  ConnClosedOrTimedOut: Boolean;
  ReceivedOn:           TIdConnectionBindings;
begin
  ReceivedOn := TIdConnectionBindings.Create;
  try
    ConnClosedOrTimedOut := false;

    if not Self.Terminated and Self.Connected then begin
      ReceivedOn.LocalIP   := Self.Socket.Binding.IP;
      ReceivedOn.LocalPort := Self.Socket.Binding.Port;
      ReceivedOn.PeerIP    := Self.Socket.Binding.PeerIP;
      ReceivedOn.PeerPort  := Self.Socket.Binding.PeerPort;
      ReceivedOn.Transport := TcpTransport;
    end;

    while not Self.Terminated and Self.Connected and not ConnClosedOrTimedOut do begin
      try
        // We want to notify as soon as there's ANY data. When there is data, we
        // want to read as much as we can.

        if (Self.ReadFromStack(true, OneSecond, false) > 0) then begin
          Self.ReceiveMessage(Self.InputBuffer, ReceivedOn);
          Self.InputBuffer.Remove(Self.InputBuffer.Size);
        end;
      except
        // We just catch read timeouts and loop again: it just means that the
        // remote party hasn't sent us data in a while, which could well be
        // perfectly normal, depending on the nature of the media stream.
        on EIdReadTimeout do;
        on EIdConnClosedGracefully do
          ConnClosedOrTimedOut := true;
        on EIdClosedSocket do
          ConnClosedOrTimedOut := true;
        on EIdNotConnected do
          ConnClosedOrTimedOut := true;
      end;
    end;
  finally
    ReceivedOn.Free;
  end;

  Self.NotifyOfDisconnection(Self, Self.ConnectionID);
end;

//* TIdSdpTcpClient Protected methods ******************************************

function TIdSdpTcpClient.GetTimer: TIdTimerQueue;
begin
  Result := nil;
end;

procedure TIdSdpTcpClient.SetTimer(Value: TIdTimerQueue);
begin
  // Do nothing.
end;

//* TIdSdpTcpClient Private methods ********************************************

procedure TIdSdpTcpClient.NotifyOfDisconnection(Sender: TObject; ConnectionID: TRegisteredObjectID);
begin
  // Notify that we have finished receiving messages and there will be no more
  // data.

  if Assigned(Self.fOnDisconnection) then
    Self.fOnDisconnection(Sender, ConnectionID);
end;

//******************************************************************************
//* TIdSdpThreadedTcpClient                                                    *
//******************************************************************************
//* TIdSdpThreadedTcpClient Public methods *************************************

constructor TIdSdpThreadedTcpClient.Create(Connection: TIdThreadableTcpClient;
                                           ConnectionLock: TCriticalSection);
begin
  inherited Create(Connection);

  Self.ConnectionLock := ConnectionLock;
end;

//* TIdSdpThreadedTcpClient Protected methods **********************************

function TIdSdpThreadedTcpClient.GetTimer: TIdTimerQueue;
begin
  Result := Self.Client.Timer;
end;

procedure TIdSdpThreadedTcpClient.Run;
begin
  try
    try
      try
        Self.Client.ReceiveMessages;
      finally
        Self.DisconnectClient(Self.Client, Self.ConnectionLock);
      end;
    finally
      Self.FreeClient(Self.Client, Self.ConnectionLock);
    end;
  except
    on EIdConnClosedGracefully do;
    on EIdConnectTimeout do;
    on E: Exception do begin
//      Self.NotifyOfException(E);
      raise E;
    end;
  end;
end;

procedure TIdSdpThreadedTcpClient.SetTimer(Value: TIdTimerQueue);
begin
  Self.Client.Timer := Value;
end;

procedure TIdSdpThreadedTcpClient.DisconnectClient(C: TIdThreadableTcpClient;
                                                   L: TCriticalSection);
begin
  L.Acquire;
  try
    C.Disconnect;
  finally
    L.Release;
  end;
end;

procedure TIdSdpThreadedTcpClient.FreeClient(C: TIdThreadableTcpClient;
                                             L: TCriticalSection);
begin
  L.Acquire;
  try
    C.Free;
  finally
    L.Release;
  end;
end;

//******************************************************************************
//* TIdSdpTcpClientConnection                                                  *
//******************************************************************************
//* TIdSdpTcpClientConnection Public methods ***********************************

constructor TIdSdpTcpClientConnection.Create;
begin
  inherited Create;

  Self.ThreadLock := TCriticalSection.Create;

  Self.Client := Self.CreateClient;
end;

destructor TIdSdpTcpClientConnection.Destroy;
begin
  Self.ThreadLock.Acquire;
  try
    if Assigned(Self.ClientThread) then begin
      Self.ClientThread.Terminate;
      Self.ClientThread.WaitFor;
    end;
  finally
    Self.ThreadLock.Release;
  end;
  Self.ThreadLock.Free;

  inherited Destroy;
end;

procedure TIdSdpTcpClientConnection.ConnectTo(PeerAddress: String; PeerPort: TPortNum);
begin
  // If we're already connected somewhere, disconnect, terminate the listening
  // thread. Then reconnect, and instantiate a new listening thread.
  Self.ThreadLock.Acquire;
  try
    if Self.IsConnected then
      Self.Disconnect;

    if not Assigned(Self.Client) then
      Self.Client := Self.CreateClient;

    Self.Client.Host := PeerAddress;
    Self.Client.Port := PeerPort;

    try
      Self.Client.Connect(Self.Timeout);

      Self.NotifyOfConnection;
    except
      on E: EIdAlreadyConnected do
        Self.NotifyOfException(ExceptClass(E.ClassType), E.Message);
      on E: EIdConnectTimeout do
        Self.NotifyOfException(ExceptClass(E.ClassType), E.Message);
      on E: EIdConnectException do
        Self.NotifyOfException(ExceptClass(E.ClassType), E.Message);
    end;
  finally
    Self.ThreadLock.Release;
  end;
end;

procedure TIdSdpTcpClientConnection.Disconnect;
begin
  Self.ThreadLock.Acquire;
  try
    if Assigned(Self.Client) then
      Self.Client.Disconnect;
  finally
    Self.ThreadLock.Release;
  end;

  Self.NotifyOfDisconnection;
end;

function TIdSdpTcpClientConnection.IsActive: Boolean;
begin
  Result := Assigned(Self.Client) and Self.Client.Connected;
end;

function TIdSdpTcpClientConnection.IsConnected: Boolean;
begin
  // For active (in the SDP sense) connections, being connected is the same as
  // being active (in the TCP sense).
  Result := Self.IsActive;
end;

procedure TIdSdpTcpClientConnection.ListenOn(Address: String; Port: TPortNum);
begin
  Self.RaiseSocketError('Do not call ListenOn on a client connection');
end;

procedure TIdSdpTcpClientConnection.SendData(Data: TStream);
begin
  // This executes in the context of a TimerQueue; see
  // * TIdSdpBaseTcpConnection.SendData
  // * TIdSdpTcpMediaStream.ReallySendData

  try
    Self.ThreadLock.Acquire;
    try
      if Assigned(Self.ClientThread) and Self.Client.Connected then
        Self.Client.WriteStream(Data);
    finally
      Self.ThreadLock.Release;
    end;
  except
    on EIdReadTimeout do;
    on EIdConnClosedGracefully do;
    on EIdClosedSocket do;
  end;
end;

//* TIdSdpTcpClientConnection Protected methods ********************************

function TIdSdpTcpClientConnection.GetAddress: String;
begin
  Result := IPv4ZeroAddress;

  Self.ThreadLock.Acquire;
  try
    if Assigned(Self.ClientThread) and Self.Client.Connected then
      Result := Self.Client.Socket.Binding.IP;
  finally
    Self.ThreadLock.Release;
  end;
end;

function TIdSdpTcpClientConnection.GetPeerAddress: String;
begin
  Result := IPv4ZeroAddress;

  Self.ThreadLock.Acquire;
  try
    if Assigned(Self.ClientThread) and Self.Client.Connected then
      Result := Self.Client.Socket.Binding.PeerIP;
  finally
    Self.ThreadLock.Release;
  end;
end;

function TIdSdpTcpClientConnection.GetPeerPort: TPortNum;
begin
  Result := TcpDiscardPort;

  Self.ThreadLock.Acquire;
  try
    if Assigned(Self.ClientThread) and Self.Client.Connected then
      Result := Self.Client.Socket.Binding.PeerPort
  finally
    Self.ThreadLock.Release;
  end;
end;

function TIdSdpTcpClientConnection.GetPort: TPortNum;
begin
  Result := TcpDiscardPort;

  Self.ThreadLock.Acquire;
  try
    if Assigned(Self.ClientThread) and Self.Client.Connected then
      Result := Self.Client.Socket.Binding.Port
  finally
    Self.ThreadLock.Release;
  end;
end;

procedure TIdSdpTcpClientConnection.SetTimer(Value: TIdTimerQueue);
begin
  inherited SetTimer(Value);

  Self.ThreadLock.Acquire;
  try
    if Assigned(Self.Client) then
      Self.Client.Timer := Value;
  finally
    Self.ThreadLock.Release;
  end;
end;

//* TIdSdpTcpClientConnection Private methods **********************************

procedure TIdSdpTcpClientConnection.ClientConnected(Client: TObject);
begin
  // ClientThread will loop, reading data off Self.Client, for as long as
  // Client is Connected.
  //
  // This executes in ClientThread's context.

  Self.ThreadLock.Acquire;
  try
    Self.ClientThread := TIdSdpThreadedTcpClient.Create(Self.Client, Self.ThreadLock);
  finally
    Self.ThreadLock.Release;
  end;
end;

procedure TIdSdpTcpClientConnection.ClientDisconnected(Client: TObject);
begin
  // This executes in ClientThread's context.

  Self.ThreadLock.Acquire;
  try
    Self.ClientThread.Terminate;
    Self.ClientThread := nil;
    Self.Client       := nil;
  finally
    Self.ThreadLock.Release;
  end;
end;

procedure TIdSdpTcpClientConnection.ClientDisconnection(Sender: TObject; ConnectionID: TRegisteredObjectID);
var
  Wait: TIdSdpTcpConnectionDisconnectedWait;
begin
  // This executes in ClientThread's context.

  Self.ThreadLock.Acquire;
  try
    Wait := TIdSdpTcpConnectionDisconnectedWait.Create;
    Wait.ConnectionID := ConnectionID;

    Self.ScheduleNotification(TriggerImmediately, Wait);
  finally
    Self.ThreadLock.Release;
  end;
end;

function TIdSdpTcpClientConnection.CreateClient: TIdSdpTcpClient;
begin
  Result := TIdSdpTcpClient.Create(nil);
  Result.ConnectionID     := Self.ID;
  Result.OnConnected      := Self.ClientConnected;
  Result.OnDisconnected   := Self.ClientDisconnected;
  Result.OnDisconnection  := Self.ClientDisconnection;
  Result.OnReceiveMessage := Self.ReceiveMessage;
end;

procedure TIdSdpTcpClientConnection.ReceiveMessage(Sender: TObject; Msg: String; ReceivedOn: TIdConnectionBindings);
var
  Data: TStringStream;
  Wait: TIdSdpTcpReceiveDataWait;
begin
  // This runs in the context of the socket-listening thread.

  Data := TStringStream.Create(Msg);
  try
    Wait := TIdSdpTcpReceiveDataWait.Create;
    Wait.ConnectionID := (Sender as TIdSdpTcpClient).ConnectionID;
    Wait.Data         := Data;
    Wait.ReceivedOn   := ReceivedOn;

    Self.ScheduleNotification(TriggerImmediately, Wait);
  finally
    Data.Free;
  end;
end;

//******************************************************************************
//* TIdSdpTcpServerConnection                                                  *
//******************************************************************************
//* TIdSdpTcpServerConnection Public methods ***********************************

constructor TIdSdpTcpServerConnection.Create;
begin
  inherited Create;

  Self.Connection := TIdTcpServer.Create(nil);
  Self.Connection.OnConnect    := Self.ClientConnected;
  Self.Connection.OnDisconnect := Self.ClientDisconnected;
  Self.Connection.OnExecute    := Self.ReadData;
end;

destructor TIdSdpTcpServerConnection.Destroy;
begin
  Self.Connection.Free;

  inherited Destroy;
end;

procedure TIdSdpTcpServerConnection.ConnectTo(PeerAddress: String; PeerPort: TPortNum);
begin
  Self.RaiseSocketError('Do not call ConnectTo on a server connection');
end;

procedure TIdSdpTcpServerConnection.Disconnect;
var
  Connections: TList;
  I:           Integer;
begin
  Connections := Self.Connection.Threads.LockList;
  try
    for I := 0 to Connections.Count - 1 do
      TIdPeerThread(Connections[I]).Connection.Disconnect;
  finally
    Self.Connection.Threads.UnlockList;
  end;

  Self.Connection.Active := false;

  // We don't need to NotifyOfDisconnection here, because the listeners will
  // receive one notification per connected client (supposedly only one) as
  // those connections are disconnected, through Indy's callbacks.
  // See ClientDisconnected.
end;

function TIdSdpTcpServerConnection.IsActive: Boolean;
begin
  Result := Self.Connection.Active;
end;

function TIdSdpTcpServerConnection.IsConnected: Boolean;
var
  Connections: TList;
begin
  Connections := Self.Connection.Threads.LockList;
  try
    Result := Self.Connection.Active and
              (Connections.Count > 0);
  finally
    Self.Connection.Threads.UnlockList;
  end;
end;

function TIdSdpTcpServerConnection.IsServer: Boolean;
begin
  Result := true;
end;

procedure TIdSdpTcpServerConnection.ListenOn(Address: String; Port: TPortNum);
var
  B: TIdSocketHandle;
begin
  Self.Connection.Active := false;
  try
    Self.Connection.Bindings.Clear;
    B := Self.Connection.Bindings.Add;
    B.IP   := Address;
    B.Port := Port;
  finally
    Self.Connection.Active := true;
  end;
end;

procedure TIdSdpTcpServerConnection.SendData(Data: TStream);
var
  Connections: TList;
  I:           Integer;
begin
  // There should be only one client connected. In case by some pathology
  // there's more than one, we just send the same data to all connected
  // clients.
  Connections := Self.Connection.Threads.LockList;
  try
    for I := 0 to Connections.Count - 1 do begin
      try
        TIdPeerThread(Connections[I]).Connection.WriteStream(Data);
      except
        on EIdReadTimeout do;
        on EIdConnClosedGracefully do;
        on EIdClosedSocket do;
      end;
    end;
  finally
    Self.Connection.Threads.UnlockList;
  end;
end;

//* TIdSdpTcpServerConnection Protected methods ********************************

function TIdSdpTcpServerConnection.GetAddress: String;
begin
  if Self.Connection.Active and (Self.Connection.Bindings.Count > 0) then
    Result := Self.Connection.Bindings[0].IP
  else
    Result := IPv4ZeroAddress;
end;

function TIdSdpTcpServerConnection.GetPeerAddress: String;
var
  Connections: TList;
  C:           TIdTCPConnection;
begin
  Result := IPv4ZeroAddress;

  Connections := Self.Connection.Threads.LockList;
  try
    if (Connections.Count > 0) then begin
      C := TIdPeerThread(Connections[0]).Connection;

      if C.Connected then
        Result := C.Socket.Binding.PeerIP;
    end;
  finally
    Self.Connection.Threads.UnlockList;
  end;
end;

function TIdSdpTcpServerConnection.GetPeerPort: TPortNum;
var
  Connections: TList;
  C:           TIdTCPConnection;
begin
  Result := TcpDiscardPort;

  Connections := Self.Connection.Threads.LockList;
  try
    if (Connections.Count > 0) then begin
      C := TIdPeerThread(Connections[0]).Connection;

      if C.Connected then
        Result := C.Socket.Binding.PeerPort
    end;
  finally
    Self.Connection.Threads.UnlockList;
  end;
end;

function TIdSdpTcpServerConnection.GetPort: TPortNum;
begin
  if Self.Connection.Active and (Self.Connection.Bindings.Count > 0) then
    Result := Self.Connection.Bindings[0].Port
  else
    Result := TcpDiscardPort;
end;

//* TIdSdpTcpServerConnection Private methods **********************************

procedure TIdSdpTcpServerConnection.ClientConnected(Thread: TIdPeerThread);
begin
  KeepAliveSocket(Thread.Connection, true);
  Self.NotifyOfConnectionInTimerContext;
end;

procedure TIdSdpTcpServerConnection.ClientDisconnected(Thread: TIdPeerThread);
begin
  Self.NotifyOfDisconnectionInTimerContext;
end;

procedure TIdSdpTcpServerConnection.NotifyOfConnectionInTimerContext;
var
  Wait: TIdSdpTcpConnectionConnectedWait;
begin
  // This is invoked in the context of one of Self.Connection's TIdPeerThreads,
  // and makes the notification happen in Self.Timer's context.

  Wait := TIdSdpTcpConnectionConnectedWait.Create;
  Wait.ConnectionID := Self.ID;

  Self.ScheduleNotification(TriggerImmediately, Wait);
end;

procedure TIdSdpTcpServerConnection.NotifyOfDisconnectionInTimerContext;
var
  Wait: TIdSdpTcpConnectionDisconnectedWait;
begin
  // This is invoked in the context of one of Self.Connection's TIdPeerThreads,
  // and makes the notification happen in Self.Timer's context.

  Wait := TIdSdpTcpConnectionDisconnectedWait.Create;
  Wait.ConnectionID := Self.ID;

  Self.ScheduleNotification(TriggerImmediately, Wait);
end;

procedure TIdSdpTcpServerConnection.ReadData(Thread: TIdPeerThread);
var
  Binding:    TIdSocketHandle;
  NewText:    TStringStream;
  ReceivedOn: TIdConnectionBindings;
begin
//ReceiveData(Data: TStream; ReceivedOn: TIdConnectionBindings)
  Binding := Thread.Connection.Socket.Binding;
  
  ReceivedOn := TIdConnectionBindings.Create(Binding.IP,
                                             Binding.Port,
                                             Binding.PeerIP,
                                             Binding.PeerPort,
                                             Id_SDP_TCP);
  try
    NewText := TStringStream.Create(Thread.Connection.CurrentReadBuffer);
    try
      Self.ReceiveData(NewText, ReceivedOn);
    finally
      NewText.Free;
    end;
  finally
    ReceivedOn.Free;
  end;
end;

//******************************************************************************
//* TIdSdpMockTcpConnection                                                    *
//******************************************************************************
//* TIdSdpMockTcpConnection Public methods *************************************

class function TIdSdpMockTcpConnection.ClientConnectionType: TIdSdpBaseTcpConnectionClass;
begin
  Result := TIdSdpMockTcpClientConnection;
end;

class function TIdSdpMockTcpConnection.NullConnectionType: TIdSdpBaseTcpConnectionClass;
begin
  Result := TIdSdpMockTcpNullConnection;
end;

class function TIdSdpMockTcpConnection.ServerConnectionType: TIdSdpBaseTcpConnectionClass;
begin
  Result := TIdSdpMockTcpServerConnection;
end;

constructor TIdSdpMockTcpConnection.Create;
begin
  inherited Create;

  Self.fAddress          := IPv4ZeroAddress;
  Self.fPeerAddress      := IPv4ZeroAddress;
  Self.fConnectToCalled  := false;
  Self.fPeerPort         := 0;
  Self.fIsActive         := false;
  Self.fIsServer         := false;
  Self.fListenOnCalled   := false;
  Self.fPort             := 0;
end;

procedure TIdSdpMockTcpConnection.Disconnect;
begin
  Self.fIsActive    := false;
  Self.fIsConnected := false;
  Self.NotifyOfDisconnection;
end;

procedure TIdSdpMockTcpConnection.ForceDisconnect;
begin
  // Use this method to simulate a network failure, or the remote end
  // disconnecting the connection.

  Self.Disconnect;
end;

function TIdSdpMockTcpConnection.IsActive: Boolean;
begin
  Result := Self.fIsActive;
end;

function TIdSdpMockTcpConnection.IsConnected: Boolean;
begin
  Result := Self.fIsConnected;
end;

function TIdSdpMockTcpConnection.IsServer: Boolean;
begin
  Result := Self.fIsServer;
end;

procedure TIdSdpMockTcpConnection.RemotePartyAccepts;
begin
  // Use this method to simulate the remote party establishing their side of the
  // connection.

  Self.fIsConnected := true;
  Self.NotifyOfConnection;
end;

procedure TIdSdpMockTcpConnection.ReceiveData(Data: TStream; ReceivedOn: TIdConnectionBindings);
begin
  if Self.IsActive then begin
    inherited ReceiveData(Data, ReceivedOn);

    Self.fReceivedData := Self.fReceivedData + StreamToStr(Data);
  end;
end;

procedure TIdSdpMockTcpConnection.SendData(Data: TStream);
begin
  if Self.IsActive then
    Self.fSentData := Self.fSentData + StreamToStr(Data);
end;

//* TIdSdpMockTcpConnection Protected methods **********************************

function TIdSdpMockTcpConnection.GetAddress: String;
begin
  Result := Self.fAddress;
end;

function TIdSdpMockTcpConnection.GetPeerAddress: String;
begin
  Result := Self.fPeerAddress;
end;

function TIdSdpMockTcpConnection.GetPeerPort: TPortNum;
begin
  Result := Self.fPeerPort;
end;

function TIdSdpMockTcpConnection.GetPort: TPortNum;
begin
  Result := Self.fPort;
end;

//******************************************************************************
//* TIdSdpMockTcpNullConnection                                                *
//******************************************************************************
//* TIdSdpMockTcpNullConnection Public methods *********************************

procedure TIdSdpMockTcpNullConnection.ConnectTo(PeerAddress: String; PeerPort: TPortNum);
begin
  // Do nothing.
end;

procedure TIdSdpMockTcpNullConnection.ForceDisconnect;
begin
  // Do nothing.
end;

procedure TIdSdpMockTcpNullConnection.ListenOn(Address: String; Port: TPortNum);
begin
  // Do nothing.
end;

procedure TIdSdpMockTcpNullConnection.ReceiveData(Data: TStream; ReceivedOn: TIdConnectionBindings);
begin
  // Do nothing.
end;

procedure TIdSdpMockTcpNullConnection.RemotePartyAccepts;
begin
  // Do nothing.
end;

//******************************************************************************
//* TIdSdpMockTcpClientConnection                                              *
//******************************************************************************
//* TIdSdpMockTcpClientConnection Public methods *******************************

procedure TIdSdpMockTcpClientConnection.ConnectTo(PeerAddress: String; PeerPort: TPortNum);
begin
  Self.fAddress         := '127.0.0.1';
  Self.fConnectToCalled := true;
  Self.fPeerAddress     := PeerAddress;
  Self.fPeerPort        := PeerPort;
  Self.fPort            := Self.EphemeralPort;
  Self.fIsActive        := true;
  Self.fIsServer        := false;
end;

function TIdSdpMockTcpClientConnection.IsServer: Boolean;
begin
  Result := false;
end;

procedure TIdSdpMockTcpClientConnection.ListenOn(Address: String; Port: TPortNum);
begin
  Self.RaiseSocketError('Do not call ListenOn on a client connection');
end;

//* TIdSdpMockTcpClientConnection Private methods ******************************

function TIdSdpMockTcpClientConnection.EphemeralPort: TPortNum;
begin
  repeat
    Result := GRandomNumber.NextCardinal
  until (Result >= 1024) and (Result <= HighestPossiblePort);
end;

//******************************************************************************
//* TIdSdpMockTcpServerConnection                                              *
//******************************************************************************
//* TIdSdpMockTcpServerConnection Public methods *******************************

procedure TIdSdpMockTcpServerConnection.ConnectTo(PeerAddress: String; PeerPort: TPortNum);
begin
  Self.RaiseSocketError('Do not call ConnectTo on a server connection');
end;

function TIdSdpMockTcpServerConnection.IsServer: Boolean;
begin
  Result := true;
end;

procedure TIdSdpMockTcpServerConnection.ListenOn(Address: String; Port: TPortNum);
begin
  Self.fAddress        := Address;
  Self.fIsActive       := true;
  Self.fIsServer       := true;
  Self.fListenOnCalled := true;
  Self.fPort           := Port;
end;

//******************************************************************************
//* TIdSdpTcpMediaStream                                                       *
//******************************************************************************
//* TIdSdpTcpMediaStream Public methods ****************************************

destructor TIdSdpTcpMediaStream.Destroy;
begin
  Self.Servers.Free;

  inherited Destroy;
end;

function TIdSdpTcpMediaStream.IsActiveConnection: Boolean;
begin
  // Did this stream initiate the TCP connection?

  Result := Self.Servers.Count > 0;

  if Result then
    Result := not Self.ServerAt(0).IsServer;
end;

function TIdSdpTcpMediaStream.IsListening: Boolean;
begin
  Result := Self.Servers.Count > 0;

  if Result then
    Result := Self.ServerAt(0).IsActive;
end;

procedure TIdSdpTcpMediaStream.StartListening;
begin
  Assert(Self.HaveLocalDescription, 'You MUST set the local description before calling StartListening');
  if not Self.IsOffer and not Self.HaveCompleteDescription then Exit;
  if Self.LocalDescription.IsRefusedStream then Exit;

  Self.PossiblyConnect(Self.HaveRemoteDescription);
end;

procedure TIdSdpTcpMediaStream.StopListening;
var
  I: Integer;
begin
  for I := 0 to Self.Servers.Count - 1 do
    Self.ServerAt(I).Disconnect;
end;

//* TIdSdpTcpMediaStream Protected methods *************************************

procedure TIdSdpTcpMediaStream.AfterSetLocalDescription(Value: TIdSdpMediaDescription);
begin
  if Self.HaveLocalDescription and Self.HaveRemoteDescription then
   Self.ResetHaveDescriptionFlags;

  Self.HaveLocalDescription := true;

  Self.InitializeLocalServers;

  Self.PossiblyConnect(Self.HaveRemoteDescription);
end;

procedure TIdSdpTcpMediaStream.AfterSetRemoteDescription(Value: TIdSdpMediaDescription);
begin
  if Self.HaveLocalDescription and Self.HaveRemoteDescription then
   Self.ResetHaveDescriptionFlags;

  Self.HaveRemoteDescription := true;

  Self.InitializeRemoteServers;

  Self.PossiblyConnect(Self.HaveLocalDescription);
end;

function TIdSdpTcpMediaStream.GetPort(Index: Integer): TPortNum;
begin
  Result := Self.ServerAt(Index).Port;
end;

procedure TIdSdpTcpMediaStream.InitializeLocalServers;
begin
//  Self.StopListening;

  Self.RecreateServers(Self.LocalDescription.PortCount);
end;

procedure TIdSdpTcpMediaStream.InternalCreate;
begin
  inherited InternalCreate;

  Self.InitializeConnectionTypeTable(Self.Factory.TcpServerType);

  Self.ResetHaveDescriptionFlags;

  Self.Servers := TObjectList.Create(true);
end;

procedure TIdSdpTcpMediaStream.ReallySendData(Data: TStream; Format: String; LayerID: Integer = 0);
var
  Wait: TIdSdpTcpSendDataWait;
begin
  Wait := TIdSdpTcpSendDataWait.Create;
  Wait.ConnectionID := Self.FindServer(LayerID).ID;
  Wait.Data         := Data;

  Self.Timer.AddEvent(TriggerImmediately, Wait);

  // TODO: This should actually be part of the Wait - all we can say right here
  // is that we have SCHEDULED the data TO BE sent, not that we've ACTUALLY sent
  // it!
  Self.NotifyOfSentData(Data, Format, LayerID);
end;

procedure TIdSdpTcpMediaStream.SetTimer(Value: TIdTimerQueue);
var
  I: Integer;
begin
  inherited SetTimer(Value);

  for I := 0 to Self.Servers.Count - 1 do
    Self.ServerAt(I).Timer := Value;
end;

procedure TIdSdpTcpMediaStream.StartServers;
var
  I: Cardinal;
  S: TIdSdpBaseTcpConnection;
begin
  if (Self.Servers.Count = 0) then
    Exit;

  for I := 0 to Self.Servers.Count - 1 do begin
    S := Self.ServerAt(I);

    if S.IsServer then
      S.ListenOn(Self.LocalDescription.Connections[0].Address, Self.LocalDescription.Port + I);
  end;
end;

//* TIdSdpTcpMediaStream Private methods ***************************************

function TIdSdpTcpMediaStream.CreateServer(OfferSetupType, AnswerSetupType: TIdSdpSetupType): TIdSdpBaseTcpConnection;
var
  ServType: TIdSdpBaseTcpConnectionClass;
begin
  ServType := Self.ServerType(Self.IsOffer, OfferSetupType, AnswerSetupType);

  if (ServType = TIdSdpBaseTcpConnection) then
    ServType := Self.Factory.TcpServerType.NullConnectionType;
//    raise Exception.Create(Format('Invalid answer setup type - offer: %s, answer: %s',
//                                  [SetupTypeToStr(OfferSetupType), SetupTypeToStr(AnswerSetupType)]));

  Result := ServType.Create;
  Result.AddDataListener(Self);
  Result.Timer := Self.Timer;
  Self.Servers.Add(Result);
end;

function TIdSdpTcpMediaStream.DefaultSetupType(ForOffer: Boolean): TIdSdpSetupType;
begin
  // cf. RFC 4145, section 4.1, last paragraph.
  if ForOffer then
    Result := stActive
  else
    Result := stPassive;
end;

function TIdSdpTcpMediaStream.FindServer(LayerID: Cardinal): TIdSdpBaseTcpConnection;
var
  I: Integer;
begin
  // LayerID denotes a layer in an hierarchically encoded stream.
  Result := nil;
  I      := 0;
  while (Result = nil) and (I < Self.Servers.Count) do begin
    if (Self.ServerAt(I).Port = LayerID) then
      Result := Self.ServerAt(I);
    Inc(I);
  end;

  if (Result = nil) then
    Result := Self.ServerAt(0);
end;

function TIdSdpTcpMediaStream.HaveCompleteDescription: Boolean;
begin
  Result := Self.HaveLocalDescription and Self.HaveRemoteDescription;
end;

function TIdSdpTcpMediaStream.HaveCorrectConnections(NumberOfServers: Integer): Boolean;
var
  CorrectConnectionType: TIdSdpBaseTcpConnectionClass;
begin
  // Return true if we have already instantiated the correct number of the
  // correct type of connections.

  if Self.IsOffer then
    CorrectConnectionType := Self.ServerType(Self.IsOffer, Self.LocalSetupType, Self.RemoteSetupType)
  else
    CorrectConnectionType := Self.ServerType(Self.IsOffer, Self.RemoteSetupType, Self.LocalSetupType);

  Result := (Self.Servers.Count = NumberOfServers)
        and (Self.Servers.Count > 0)
        and (Self.ServerAt(0).ClassType = CorrectConnectionType);
end;

procedure TIdSdpTcpMediaStream.InitializeConnectionTypeTable(BaseServerType: TIdSdpBaseTcpConnectionClass);
begin
  Self.OfferConnectionType[stActive, stActive]    := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stActive, stActPass]   := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stActive, stPassive]   := BaseServerType.ClientConnectionType;
  Self.OfferConnectionType[stActive, stHoldConn]  := BaseServerType.NullConnectionType;
  Self.OfferConnectionType[stActive, stUnknown]   := TIdSdpBaseTcpConnection;

  Self.OfferConnectionType[stActPass, stActive]   := BaseServerType.ServerConnectionType;
  Self.OfferConnectionType[stActPass, stActPass]  := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stActPass, stPassive]  := BaseServerType.ClientConnectionType;
  Self.OfferConnectionType[stActPass, stHoldConn] := BaseServerType.NullConnectionType;
  Self.OfferConnectionType[stActPass, stUnknown]  := TIdSdpBaseTcpConnection;

  Self.OfferConnectionType[stPassive, stActive]   := BaseServerType.ServerConnectionType;
  Self.OfferConnectionType[stPassive, stActPass]  := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stPassive, stPassive]  := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stPassive, stHoldConn] := BaseServerType.NullConnectionType;
  Self.OfferConnectionType[stPassive, stUnknown]  := TIdSdpBaseTcpConnection;

  Self.OfferConnectionType[stHoldConn, stActive]   := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stHoldConn, stActPass]  := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stHoldConn, stPassive]  := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stHoldConn, stHoldConn] := BaseServerType.NullConnectionType;
  Self.OfferConnectionType[stHoldConn, stUnknown]  := TIdSdpBaseTcpConnection;

  Self.OfferConnectionType[stUnknown, stActive]   := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stUnknown, stActPass]  := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stUnknown, stPassive]  := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stUnknown, stHoldConn] := TIdSdpBaseTcpConnection;
  Self.OfferConnectionType[stUnknown, stUnknown]  := TIdSdpBaseTcpConnection;

  //
  Self.AnswerConnectionType[stActive, stActive]    := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stActive, stActPass]   := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stActive, stPassive]   := BaseServerType.ServerConnectionType;
  Self.AnswerConnectionType[stActive, stHoldConn]  := BaseServerType.NullConnectionType;
  Self.AnswerConnectionType[stActive, stUnknown]   := TIdSdpBaseTcpConnection;

  Self.AnswerConnectionType[stActPass, stActive]   := BaseServerType.ClientConnectionType;
  Self.AnswerConnectionType[stActPass, stActPass]  := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stActPass, stPassive]  := BaseServerType.ServerConnectionType;
  Self.AnswerConnectionType[stActPass, stHoldConn] := BaseServerType.NullConnectionType;
  Self.AnswerConnectionType[stActPass, stUnknown]  := TIdSdpBaseTcpConnection;

  Self.AnswerConnectionType[stPassive, stActive]   := BaseServerType.ClientConnectionType;
  Self.AnswerConnectionType[stPassive, stActPass]  := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stPassive, stPassive]  := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stPassive, stHoldConn] := BaseServerType.NullConnectionType;
  Self.AnswerConnectionType[stPassive, stUnknown]  := TIdSdpBaseTcpConnection;

  Self.AnswerConnectionType[stHoldConn, stActive]   := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stHoldConn, stActPass]  := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stHoldConn, stPassive]  := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stHoldConn, stHoldConn] := BaseServerType.NullConnectionType;
  Self.AnswerConnectionType[stHoldConn, stUnknown]  := TIdSdpBaseTcpConnection;

  Self.AnswerConnectionType[stUnknown, stActive]   := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stUnknown, stActPass]  := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stUnknown, stPassive]  := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stUnknown, stHoldConn] := TIdSdpBaseTcpConnection;
  Self.AnswerConnectionType[stUnknown, stUnknown]  := TIdSdpBaseTcpConnection;
end;

procedure TIdSdpTcpMediaStream.InitializeRemoteServers;
begin
  if (Self.RemoteDescription.PortCount = 0) then Exit;

  Self.RecreateServers(Self.RemoteDescription.PortCount);
end;

procedure TIdSdpTcpMediaStream.OnConnect(Connection: TIdSdpBaseTcpConnection);
begin
  // TODO: What to do here?
end;

procedure TIdSdpTcpMediaStream.OnData(Connection: TIdSdpBaseTcpConnection; Data: TStream);
var
  GuessedFormat: String;
  ReceivedOn:    TIdConnectionBindings;
begin
  if (Self.LocalDescription.FormatCount = 0) then begin
    // We should never enter this clause.
    GuessedFormat := ''
  end
  else begin
    // This works when FormatCount = 1. It's misleading when FormatCount > 1,
    // but without parsing Data I can't think of any way to determine the
    // Format. (And what if Data contains some of one format, and some of
    // another format?)
    GuessedFormat := Self.LocalDescription.Formats[0];
  end;

  ReceivedOn := TIdConnectionBindings.Create(Connection.Address, Connection.Port, Connection.PeerAddress, Connection.PeerPort, Id_SDP_TCP);
  try
    Self.NotifyOfData(ReceivedOn, Data, GuessedFormat);
  finally
    ReceivedOn.Free;
  end;
end;

procedure TIdSdpTcpMediaStream.OnDisconnect(Connection: TIdSdpBaseTcpConnection);
begin
  // TODO: What to do here?
end;

procedure TIdSdpTcpMediaStream.OnException(Connection: TIdSdpBaseTcpConnection;
                                           ExceptionType: ExceptClass;
                                           ExceptionMessage: String);
begin
  // TODO: What to do here?
end;

procedure TIdSdpTcpMediaStream.RecreateServers(NumberOfServers: Cardinal);
var
  I: Integer;
begin
  if Self.HaveCorrectConnections(NumberOfServers) then Exit;

  Self.Servers.Clear;

  for I := 1 to NumberOfServers do
    if Self.IsOffer then
      Self.CreateServer(Self.LocalSetupType, Self.RemoteSetupType)
    else
      Self.CreateServer(Self.RemoteSetupType, Self.LocalSetupType);
end;

procedure TIdSdpTcpMediaStream.ResetHaveDescriptionFlags;
begin
  Self.HaveLocalDescription  := false;
  Self.HaveRemoteDescription := false;
end;

function TIdSdpTcpMediaStream.ServerAt(Index: Integer): TIdSdpBaseTcpConnection;
begin
  Result := Self.Servers[Index] as TIdSdpBaseTcpConnection;
end;

function TIdSdpTcpMediaStream.LocalSetupType: TIdSdpSetupType;
begin
  if Self.HaveLocalDescription then begin
    if Self.LocalDescription.Attributes.HasAttributeNamed(SetupAttribute) then
      Result := Self.LocalDescription.Attributes.SetupType
    else
      Result := Self.DefaultSetupType(Self.IsOffer);
  end
  else
    Result := stHoldConn;
end;

procedure TIdSdpTcpMediaStream.PossiblyConnect(HaveCompleteSessionDescription: Boolean);
begin
  if Self.IsListening then Exit;

  case Self.LocalSetupType of
    stActive: begin
      if HaveCompleteSessionDescription then
        Self.StartConnectingStreams;
    end;
    stActPass: begin
      if HaveCompleteSessionDescription then begin
        case Self.RemoteSetupType of
          stActive: Self.BindListeningStreams;
          stPassive: begin
                       Self.StopListening;
                       Self.StartConnectingStreams;
                     end;
        else
          Self.StopListening;
        end;
      end
      else begin
        Self.BindListeningStreams;
      end;
    end;
    stPassive: Self.BindListeningStreams;
  end;
end;

function TIdSdpTcpMediaStream.RemoteSetupType: TIdSdpSetupType;
begin
  if Self.HaveRemoteDescription then begin
    if Self.RemoteDescription.Attributes.HasAttributeNamed(SetupAttribute) then
      Result := Self.RemoteDescription.Attributes.SetupType
    else
      Result := Self.DefaultSetupType(not Self.IsOffer);
  end
  else begin
    // If we don't know what the far end will offer, but we're prepared to
    // accept a connection, then we must start listening for a connection
    // attempt. Answering stActive tells other methods to create a listening
    // stream.
    if Self.HaveLocalDescription and Self.UsesServerSockets then
      Result := stActive
    else
      Result := stHoldConn;
  end;
end;

function TIdSdpTcpMediaStream.ServerType(IsOffer: Boolean; OfferSetupType, AnswerSetupType: TIdSdpSetupType): TIdSdpBaseTcpConnectionClass;
begin
  if IsOffer then
    Result := Self.OfferConnectionType[OfferSetupType, AnswerSetupType]
  else
    Result := Self.AnswerConnectionType[OfferSetupType, AnswerSetupType];
end;

procedure TIdSdpTcpMediaStream.StartConnectingStreams;
var
  I: Cardinal;
begin
  if (Self.Servers.Count = 0) then
    Exit;

  if not Self.HaveRemoteDescription then
    Exit;

  // cf. RFC 4145, section 4.1 (page 4, second half of first paragraph)
  Self.LocalDescription.Port := TcpDiscardPort;

  // Note that this doesn't work properly if the media description has multiple
  // connection headers - it will only use the first.
  for I := 0 to Self.Servers.Count - 1 do
    Self.ServerAt(I).ConnectTo(Self.RemoteDescription.Connections[0].Address,
                               Self.RemoteDescription.Port + I);
end;

function TIdSdpTcpMediaStream.UsesServerSockets: Boolean;
begin
  Result := Self.LocalSetupType in [stActPass, stPassive]
end;

//******************************************************************************
//* TIdSdpMediaStreamFactory                                                   *
//******************************************************************************
//* TIdSdpMediaStreamFactory Public methods ************************************

constructor TIdSdpMediaStreamFactory.Create;
begin
  inherited Create;

  Self.fRtpServerType := TIdRTPServer;
  Self.fTcpServerType := TIdSdpBaseTcpConnection;
end;

function TIdSdpMediaStreamFactory.CreateStream(Protocol: String): TIdSdpBaseMediaStream;
begin
  if (Protocol = Id_SDP_TCP) then
    Result := TIdSdpTcpMediaStream.Create(Self)
  else
    Result := TIdSDPMediaStream.Create(Self);
end;

//******************************************************************************
//* TIdSDPMultimediaSession                                                    *
//******************************************************************************
//* TIdSDPMultimediaSession Public methods *************************************

constructor TIdSDPMultimediaSession.Create(Profile: TIdRTPProfile; Factory: TIdSdpMediaStreamFactory; ExecutionContext: TIdTimerQueue);
begin
  inherited Create;

  Self.Factory := Factory;
  Self.Timer   := ExecutionContext;
  Self.InternalCreate(Profile);
end;

destructor TIdSDPMultimediaSession.Destroy;
begin
  Self.StreamLock.Acquire;
  try
    Self.fStreams.Free;
  finally
    Self.StreamLock.Release;
  end;
  Self.StreamLock.Free;

  Self.TimeHeader.Free;

  inherited Destroy;
end;

function TIdSDPMultimediaSession.AddressTypeFor(Address: String): TIdIPVersion;
begin
  if TIdIPAddressParser.IsIPv4Address(Address) then
    Result := Id_IPv4
  else if TIdIPAddressParser.IsIPv6Address(Address) then
    Result := Id_IPv6
  else
    Result := Id_IPUnknown;
end;

function TIdSDPMultimediaSession.IndexOfStream(S: TIdSdpBaseMediaStream): Integer;
begin
  // If S is not a stream that belongs to Self, then return -1.
  // Otherwise, return the index of the stream.

  Self.StreamLock.Acquire;
  try
    Result := Self.fStreams.IndexOf(S);
  finally
    Self.StreamLock.Release;
  end;
end;

function TIdSDPMultimediaSession.IsListening: Boolean;
begin
  Result := Self.StreamCount > 0;
end;

procedure TIdSDPMultimediaSession.JoinSession;
var
  I: Integer;
begin
  // Once you know both the local session description and the remote session
  // description, you may join the session.

  for I := 0 to Self.StreamCount - 1 do
    Self.Streams[I].JoinSession;
end;

function TIdSDPMultimediaSession.LocalSessionDescription: String;
var
  I: Integer;
  SDP: TIdSdpPayload;
begin
  SDP := TIdSdpPayload.Create;
  try
    SDP.Origin.Address        := Self.LocalMachineName;
    SDP.Origin.AddressType    := Self.AddressTypeFor(Self.LocalMachineName);
    SDP.Origin.NetType        := Self.NetTypeFor(Self.LocalMachineName);
    SDP.Origin.Username       := Self.Username;
    SDP.Origin.SessionID      := Self.LocalSessionID;
    SDP.Origin.SessionVersion := IntToStr(Self.LocalSessionVersion);
    SDP.SessionName           := Self.LocalSessionName;

    if Assigned(Self.TimeHeader) then
      SDP.Times.Add(Self.TimeHeader);

    for I := 0 to Self.StreamCount - 1 do
      SDP.MediaDescriptions.Add(Self.Streams[I].LocalDescription);

    Result := SDP.AsString;
  finally
    SDP.Free;
  end;
end;

function TIdSDPMultimediaSession.LocalSessionVersion: Int64;
begin
  Result := Self.fLocalSessionVersion;
end;

function TIdSDPMultimediaSession.MimeType: String;
begin
  Result := SdpMimeType;
end;

function TIdSDPMultimediaSession.NetTypeFor(Address: String): String;
begin
  if TIdIPAddressParser.IsIPv4Address(Address) or TIdIPAddressParser.IsIPv6Address(Address) then
    Result := Id_SDP_IN
  else
    Result := 'UNKNOWN'; 
end;

procedure TIdSDPMultimediaSession.PutOnHold;
var
  I: Integer;
begin
  for I := 0 to Self.StreamCount - 1 do
    Self.Streams[I].PutOnHold;

  Self.fOnHold := true;
  Self.UpdateSessionVersion;
end;

procedure TIdSDPMultimediaSession.SetRemoteDescription(RemoteSessionDesc: String);
var
  SDP: TIdSdpPayload;
begin
  // We don't need to know the MIME type: this is an SDP multimedia session,
  // ergo we simply assume that RemoteSessionDesc contains application/sdp.

  Self.StreamLock.Acquire;
  try
    SDP := TIdSdpPayload.CreateFrom(RemoteSessionDesc);
    try
      Self.SetRemoteDescription(SDP);
    finally
      SDP.Free;
    end;
  finally
    Self.StreamLock.Release;
  end;
end;

procedure TIdSDPMultimediaSession.SetRemoteDescription(RemoteSessionDesc: TIdSdpPayload);
var
  I: Integer;
begin
  // According to RFC 3264, the answer must have the same number of media
  // descriptions as an offer. Thus, (regardless of whether RemoteSessionDesc
  // contains an offer or an answer), RemoteSessionDesc must contain the same
  // number of media descriptions as LocalSessionDesc in StartListening had.
  // But you might call this method before StartListening, so we don't know
  // which number of media descriptions to follow. Thus we do nothing, and
  // let you, the user of this class, decide.

  Self.StreamLock.Acquire;
  try
    if (Self.StreamCount <> RemoteSessionDesc.MediaDescriptionCount) then
      Self.RecreateStreams(RemoteSessionDesc);

    for I := 0 to RemoteSessionDesc.MediaDescriptionCount - 1 do
      Self.Streams[I].RemoteDescription := RemoteSessionDesc.MediaDescriptionAt(I);

    // RFC 3264 section 5 says that an offer SHOULD have a "t=0 0" header.
    if (RemoteSessionDesc.Times.Count > 0) then begin
      Self.TimeHeader := TIdSdpTime.Create;
      Self.TimeHeader.Assign(RemoteSessionDesc.Times[0]);
    end
    else begin
      if Assigned(Self.TimeHeader) then
        FreeAndNil(Self.TimeHeader);
    end;
  finally
    Self.StreamLock.Release;
  end;
end;

function TIdSDPMultimediaSession.StartListening(LocalSessionDesc: String): String;
var
  SDP: TIdSdpPayload;
begin
  // We don't know, until we try, whether the ports in LocalSessionDesc are
  // free. LocalSessionDesc thus contains info on how many streams to create,
  // what data those streams will contain, but NOT on what ports they'll run:
  // the ports are just guidelines as to the lowest acceptable port number, if
  // you like.
  //
  // As an example, if there's one media description with port 8000, and we're
  // already running servers on ports 8000-8099, we'll start a server on 8100.
  // Result contains the ACTUAL port numbers used.

  Self.StreamLock.Acquire;
  try
    SDP := TIdSdpPayload.CreateFrom(LocalSessionDesc);
    try
      Result := Self.StartListening(SDP);
    finally
      SDP.Free;
    end;
  finally
    Self.StreamLock.Release;
  end;
end;

function TIdSDPMultimediaSession.StartListening(LocalSessionDesc: TIdSdpPayload): String;
var
  I: Integer;
begin
  // Note: We ignore fluff like session name, origin user name, origin sess-id,
  // sess-version and all that. For the purpose of setting up media streams we
  // only care about media descriptions. Besides, this class will take care of
  // sess-version and similar session descriptors.

  Self.StreamLock.Acquire;
  try
    if (Self.StreamCount <> LocalSessionDesc.MediaDescriptionCount) then
      Self.RecreateStreams(LocalSessionDesc);

    for I := 0 to LocalSessionDesc.MediaDescriptionCount - 1 do begin
      Self.Streams[I].LocalDescription := LocalSessionDesc.MediaDescriptionAt(I);
      Self.Streams[I].StartListening;
    end;

    Self.UpdateSessionVersion;

    Result := Self.LocalSessionDescription;
  finally
    Self.StreamLock.Release;
  end;
end;

procedure TIdSDPMultimediaSession.StopListening;
var
  I: Integer;
begin
  Self.StreamLock.Acquire;
  try
    for I := 0 to Self.StreamCount - 1 do
      Self.Streams[I].StopListening;

    Self.fStreams.Clear;
  finally
    Self.StreamLock.Release;
  end;
end;

function TIdSDPMultimediaSession.StreamCount: Integer;
begin
  Self.StreamLock.Acquire;
  try
    Result := Self.fStreams.Count;
  finally
    Self.StreamLock.Release;
  end;
end;

procedure TIdSDPMultimediaSession.TakeOffHold;
var
  I: Integer;
begin
  for I := 0 to Self.StreamCount - 1 do
    Self.Streams[I].TakeOffHold;

  Self.fOnHold := false;
  Self.UpdateSessionVersion;
end;

procedure TIdSDPMultimediaSession.ClearStreams;
begin
  // Precondition: you've acquired StreamLock
  Self.fStreams.Clear;
end;

function TIdSDPMultimediaSession.CreateStream(Protocol: String): TIdSDPBaseMediaStream;
begin
  Result := Self.Factory.CreateStream(Protocol);

  Result.HighestAllowedPort := Self.HighestAllowedPort;
  Result.IsOffer            := Self.IsOffer;
  Result.LowestAllowedPort  := Self.LowestAllowedPort;
  Result.Timer := Self.Timer;
  Self.fStreams.Add(Result);
end;

procedure TIdSDPMultimediaSession.InternalCreate(Profile: TIdRTPProfile);
begin
  Self.fStreams := TObjectList.Create;
  Self.StreamLock := TCriticalSection.Create;

  Self.FirstLocalSessDesc   := true;
  Self.fLocalSessionVersion := 0;
  Self.IsOffer              := true;
  Self.LocalMachineName     := '127.0.0.1';
  Self.LocalSessionID       := IntToStr(GRandomNumber.NextCardinal);
  Self.LocalSessionName     := BlankSessionName;
  Self.LowestAllowedPort    := LowestPossiblePort;
  Self.HighestAllowedPort   := HighestPossiblePort;
  Self.Username             := BlankUsername;
end;

procedure TIdSDPMultimediaSession.SetIsOffer(Value: Boolean);
var
  I: Integer;
begin
  Self.fIsOffer := Value;

  for I := 0 to Self.StreamCount - 1 do
    Self.Streams[I].IsOffer := Value;
end;

//* TIdSDPMultimediaSession Private methods ************************************

function TIdSDPMultimediaSession.GetStreams(Index: Integer): TIdSdpBaseMediaStream;
begin
  Self.StreamLock.Acquire;
  try
    Result := Self.fStreams[Index] as TIdSdpBaseMediaStream;
  finally
    Self.StreamLock.Release;
  end;
end;

procedure TIdSDPMultimediaSession.RecreateStreams(LocalDescription: TIdSdpPayload);
var
  I: Integer;
begin
  // Precondition: you've acquired StreamLock
  Self.ClearStreams;
  for I := 0 to LocalDescription.MediaDescriptionCount - 1 do
    Self.CreateStream(LocalDescription.MediaDescriptions[I].Protocol);
end;

procedure TIdSDPMultimediaSession.SetHighestAllowedPort(Value: TPortNum);
var
  I: Integer;
begin
  Self.fHighestAllowedPort := Value;

  Self.StreamLock.Acquire;
  try
    for I := 0 to Self.StreamCount - 1 do
      Self.Streams[I].HighestAllowedPort := Self.fHighestAllowedPort;
  finally
    Self.StreamLock.Release;
  end;
end;

procedure TIdSDPMultimediaSession.SetLocalMachineName(Value: String);
begin
  Self.fLocalMachineName := Value;
end;

procedure TIdSDPMultimediaSession.SetLowestAllowedPort(Value: TPortNum);
var
  I: Integer;
begin
  Self.fLowestAllowedPort := Value;

  Self.StreamLock.Acquire;
  try
    for I := 0 to Self.StreamCount - 1 do
      Self.Streams[I].LowestAllowedPort := Self.fLowestAllowedPort;
  finally
    Self.StreamLock.Release;
  end;
end;

procedure TIdSDPMultimediaSession.UpdateSessionVersion;
begin
  if Self.FirstLocalSessDesc then
    Self.FirstLocalSessDesc := false
  else begin
    Inc(Self.fLocalSessionVersion);
  end;
end;

//******************************************************************************
//* TIdSdpMediaListener                                                        *
//******************************************************************************
//* TIdSdpMediaListener Public methods *****************************************

constructor TIdSdpMediaListener.Create;
begin
  inherited Create;

  Self.fChunk := TMemoryStream.Create;
end;

destructor TIdSdpMediaListener.Destroy;
begin
  Self.fChunk.Free;

  inherited Destroy;
end;

//* TIdSdpMediaListener Private methods ****************************************

procedure TIdSdpMediaListener.SetChunk(Value: TStream);
begin
  Self.Chunk.CopyFrom(Value, 0);
end;

//******************************************************************************
//* TIdSdpTcpConnectionWait                                                    *
//******************************************************************************
//* TIdSdpTcpConnectionWait Public methods *************************************

procedure TIdSdpTcpConnectionWait.Trigger;
begin
  TIdObjectRegistry.Singleton.WithExtantObjectDo(Self.ConnectionID, Self.TriggerClosure);
end;

//* TIdSdpTcpConnectionWait Protected methods **********************************

procedure TIdSdpTcpConnectionWait.ActOnTrigger(C: TIdSdpBaseTcpConnection);
begin
  // By default do nothing.
end;

//* TIdSdpTcpConnectionWait Private methods ************************************

procedure TIdSdpTcpConnectionWait.TriggerClosure(O: TObject);
begin
  if (O is TIdSdpBaseTcpConnection) then
    Self.ActOnTrigger(O as TIdSdpBaseTcpConnection);
end;

//******************************************************************************
//* TIdSdpTcpConnectionConnectedWait                                           *
//******************************************************************************
//* TIdSdpTcpConnectionConnectedWait Protected methods *************************

procedure TIdSdpTcpConnectionConnectedWait.ActOnTrigger(C: TIdSdpBaseTcpConnection);
begin
  C.NotifyOfConnection;
end;

//******************************************************************************
//* TIdSdpTcpConnectionDisconnectedWait                                        *
//******************************************************************************
//* TIdSdpTcpConnectionDisconnectedWait Protected methods **********************

procedure TIdSdpTcpConnectionDisconnectedWait.ActOnTrigger(C: TIdSdpBaseTcpConnection);
begin
  C.NotifyOfDisconnection;
end;

//******************************************************************************
//* TIdSdpTcpConnectionExceptionWait                                           *
//******************************************************************************
//* TIdSdpTcpConnectionExceptionWait Protected methods *************************

procedure TIdSdpTcpConnectionExceptionWait.ActOnTrigger(C: TIdSdpBaseTcpConnection);
begin
  C.NotifyOfException(Self.ExceptionType, Self.ExceptionMessage);
end;

//******************************************************************************
//* TIdSdpTcpReceiveDataWait                                                   *
//******************************************************************************
//* TIdSdpTcpReceiveDataWait Public methods ************************************

constructor TIdSdpTcpReceiveDataWait.Create;
begin
  inherited Create;

  Self.fData := TMemoryStream.Create;
  Self.fReceivedOn := TIdConnectionBindings.Create;
end;

destructor TIdSdpTcpReceiveDataWait.Destroy;
begin
  Self.fReceivedOn.Free;
  Self.fData.Free;

  inherited Destroy;
end;

//* TIdSdpTcpReceiveDataWait Protected methods *********************************

procedure TIdSdpTcpReceiveDataWait.ActOnTrigger(C: TIdSdpBaseTcpConnection);
begin
  C.ReceiveData(Self.Data, Self.ReceivedOn);
end;

procedure TIdSdpTcpReceiveDataWait.LogTrigger;
const
  LogMsg = '%s with ID %s received data';
begin
  Self.OnLog(slDebug,
             '',
             LogEventRefTimerEvent,
             Format(LogMsg, [Self.ClassName, OidAsString(Self.ID)]),
             EncodeQuotedStr(StreamToStr(Data)));
end;

//* TIdSdpTcpReceiveDataWait Private methods ***********************************

procedure TIdSdpTcpReceiveDataWait.SetData(Value: TStream);
begin
  Self.fData.CopyFrom(Value, 0);
end;

procedure TIdSdpTcpReceiveDataWait.SetReceivedOn(Value: TIdConnectionBindings);
begin
  Self.fReceivedOn.Assign(Value);
end;

//******************************************************************************
//* TIdSdpTcpSendDataWait                                                      *
//******************************************************************************
//* TIdSdpTcpSendDataWait Public methods ***************************************

constructor TIdSdpTcpSendDataWait.Create;
begin
  inherited Create;

  Self.fData := TMemoryStream.Create;
end;

destructor TIdSdpTcpSendDataWait.Destroy;
begin
  Self.fData.Free;

  inherited Destroy;
end;

//* TIdSdpTcpSendDataWait Protected methods ************************************

procedure TIdSdpTcpSendDataWait.ActOnTrigger(C: TIdSdpBaseTcpConnection);
begin
  C.SendData(Self.Data);
end;

//* TIdSdpTcpSendDataWait Private methods **************************************

procedure TIdSdpTcpSendDataWait.SetData(Value: TStream);
begin
  Self.fData.CopyFrom(Value, 0);
end;

//******************************************************************************
//* TIdSdpMediaListenerOnDataMethod                                            *
//******************************************************************************
//* TIdSdpMediaListenerOnDataMethod Public methods *****************************

constructor TIdSdpMediaListenerOnDataMethod.Create;
begin
  inherited Create;

  Self.fBinding := TIdConnectionBindings.Create;
end;

destructor TIdSdpMediaListenerOnDataMethod.Destroy;
begin
  Self.fBinding.Free;

  inherited Destroy;
end;

procedure TIdSdpMediaListenerOnDataMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSdpMediaListener).OnData(Self.Stream, Self.Chunk, Self.Format, Self.Binding);
end;

//* TIdSdpMediaListenerOnDataMethod Private methods ****************************

procedure TIdSdpMediaListenerOnDataMethod.SetBinding(Value: TIdConnectionBindings);
begin
  Self.fBinding.Assign(Value);
end;

//******************************************************************************
//* TIdSdpMediaListenerOnSentDataMethod                                        *
//******************************************************************************
//* TIdSdpMediaListenerOnSentDataMethod Public methods *************************

procedure TIdSdpMediaListenerOnSentDataMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSdpMediaSendListener).OnSentData(Self.Stream, Self.Chunk, Self.Format, Self.LayerID);
end;

//******************************************************************************
//* TIdSdpTcpConnectionOnConnectMethod                                         *
//******************************************************************************
//* TIdSdpTcpConnectionOnConnectMethod Public methods **************************

procedure TIdSdpTcpConnectionOnConnectMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSdpTcpConnectionListener).OnConnect(Self.Connection);
end;

//******************************************************************************
//* TIdSdpTcpConnectionOnDataMethod                                            *
//******************************************************************************
//* TIdSdpTcpConnectionOnDataMethod Public methods *****************************

constructor TIdSdpTcpConnectionOnDataMethod.Create;
begin
  inherited Create;

  Self.fData := TMemoryStream.Create;
end;

destructor TIdSdpTcpConnectionOnDataMethod.Destroy;
begin
  Self.fData.Free;

  inherited Destroy;
end;

procedure TIdSdpTcpConnectionOnDataMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSdpTcpConnectionListener).OnData(Self.Connection, Self.Data);
end;

//* TIdSdpTcpConnectionOnDataMethod Private methods ****************************

procedure TIdSdpTcpConnectionOnDataMethod.SetData(Value: TStream);
begin
  Self.fData.CopyFrom(Value, 0);
end;

//******************************************************************************
//* TIdSdpTcpConnectionOnDisconnectMethod                                      *
//******************************************************************************
//* TIdSdpTcpConnectionOnDisconnectMethod Public methods ***********************

procedure TIdSdpTcpConnectionOnDisconnectMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSdpTcpConnectionListener).OnDisconnect(Self.Connection);
end;

//******************************************************************************
//* TIdSdpTcpConnectionOnExceptionMethod                                       *
//******************************************************************************
//* TIdSdpTcpConnectionOnExceptionMethod Public methods ************************

procedure TIdSdpTcpConnectionOnExceptionMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSdpTcpConnectionListener).OnException(Self.Connection,
                                                       Self.ExceptionType,
                                                       Self.ExceptionMessage);
end;

initialization
  GSdpTcpConnectionRegistry := TIdSdpTcpConnectionRegistry.Create;
end.
