unit IdSdp;

interface

uses
  Classes, Contnrs, IdSNTP, IdAssignedNumbers, IdEmailAddress,
  IdInterfacedObject, IdRTP, IdRTPServer, IdSimpleParser, IdSocketHandle,
  IdUDPServer, SyncObjs;

type
  TIdNtpTimestamp     = Int64;
  TIdSdpBandwidthType = (btConferenceTotal, btApplicationSpecific, btRS, btRR);
  TIdSdpKeyType       = (ktClear, ktBase64, ktURI, ktPrompt);
  // Technically, Text doesn't exist. However, it will once
  // draft-ietf-sip-callee-caps gets an RFC number.
  TIdSdpMediaType     = (mtAudio, mtVideo, mtApplication, mtData, mtControl,
                         mtText);

  TIdPrintable = class(TObject)
    procedure PrintOn(Dest: TStream); virtual; abstract;
  end;

  TIdSdpAttribute = class(TIdPrintable)
  private
    fName:  String;
    fValue: String;

  protected
    function  GetName: String; virtual;
    procedure SetValue(const Value: String); virtual;
  public
    class function CreateAttribute(Value: String): TIdSdpAttribute;

    constructor Create; virtual;

    function  Copy: TIdSdpAttribute;
    function  IsRTPMap: Boolean; virtual;
    procedure PrintOn(Dest: TStream); override;

    property Name:  String read GetName write fName;
    property Value: String read fValue  write SetValue;
  end;

  TIdSdpAttributeClass = class of TIdSdpAttribute;

  TIdSdpRTPMapAttribute = class(TIdSdpAttribute)
  private
    fPayloadType: TIdRTPPayloadType;
    fEncoding:    TIdRTPPayload;

    procedure SetEncoding(const Value: TIdRTPPayload);
  protected
    function  GetName: String; override;
    procedure SetValue(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function IsRTPMap: Boolean; override;

    property PayloadType: TIdRTPPayloadType read fPayloadType write fPayloadType;
    property Encoding:    TIdRTPPayload     read fEncoding;
  end;

  TIdSdpBandwidth = class(TIdPrintable)
  private
    fBandwidth:     Cardinal;
    fBandwidthType: TIdSdpBandwidthType;
  public
    procedure PrintOn(Dest: TStream); override;

    property Bandwidth:     Cardinal            read fBandwidth write fBandwidth;
    property BandwidthType: TIdSdpBandwidthType read fBandwidthType write fBandwidthType;
  end;

  TIdSdpConnection = class(TIdPrintable)
  private
    fAddress:           String;
    fAddressType:       TIdIPVersion;
    fNetType:           String;
    fNumberOfAddresses: Cardinal;
    fTTL:               Byte;
  public
    function  Copy: TIdSdpConnection;
    procedure PrintOn(Dest: TStream); override;

    property AddressType:       TIdIPVersion read fAddressType write fAddressType;
    property Address:           String       read fAddress write fAddress;
    property NetType:           String       read fNetType write fNetType;
    property NumberOfAddresses: Cardinal     read fNumberOfAddresses write fNumberOfAddresses;
    property TTL:               Byte         read fTTL write fTTL;
  end;

  TIdSdpKey = class(TIdPrintable)
  private
    fKeyType: TIdSdpKeyType;
    fValue:   String;
  public
    procedure PrintOn(Dest: TStream); override;

    property KeyType: TIdSdpKeyType read fKeyType write fKeyType;
    property Value:   String        read fValue write fValue;
  end;

  TIdSdpAttributes = class;
  TIdSdpBandwidths = class;
  TIdSdpConnections = class;

  TIdSdpMediaDescription = class(TIdPrintable)
  private
    fAttributes:  TIdSdpAttributes;
    fBandwidths:  TIdSdpBandwidths;
    fConnections: TIdSdpConnections;
    fInfo:        String;
    fKey:         TIdSdpKey;
    fMediaType:   TIdSdpMediaType;
    FormatList:   TStrings;
    fPort:        Cardinal;
    fPortCount:   Cardinal;
    fTransport:   String;

    function  GetAttributes: TIdSdpAttributes;
    function  GetBandwidths: TIdSdpBandwidths;
    function  GetConnections: TIdSdpConnections;
    function  GetFormats(Index: Integer): String;
    function  GetKey: TIdSdpKey;
    procedure PrintInfoField(Dest: TStream);
    procedure PrintMediaField(Dest: TStream);

    property Attributes: TIdSdpAttributes read GetAttributes;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddAttribute(const Name, Value: String);
    function  AttributeAt(Index: Integer): TIdSdpAttribute;
    function  AttributeCount: Integer;
    procedure AddFormat(const Fmt: String);
    function  FormatCount: Integer;
    procedure ClearFormats;
    function  HasConnection: Boolean;
    function  HasFormat(Fmt: String): Boolean;
    function  HasKey: Boolean;
    procedure PrintOn(Dest: TStream); override;

    property Bandwidths:              TIdSdpBandwidths  read GetBandwidths;
    property Connections:             TIdSdpConnections read GetConnections;
    property Formats[Index: Integer]: String            read GetFormats;
    property Info:                    String            read fInfo write fInfo;
    property Key:                     TIdSdpKey         read GetKey;
    property MediaType:               TIdSdpMediaType   read fMediaType write fMediaType;
    property Port:                    Cardinal          read fPort write fPort;
    property PortCount:               Cardinal          read fPortCount write fPortCount;
    property Transport:               String            read fTransport write fTransport;
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
    procedure PrintOn(Dest: TStream); override;

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
    procedure PrintOn(Dest: TStream); override;

    property Value: String read fValue write fValue;
  end;

  TIdSdpZoneAdjustment = class(TIdPrintable)
  private
    fValue: String;
  public
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

    procedure PrintOn(Dest: TStream); override;

    property EndTime:         TIdNtpTimestamp       read fEndTime write fEndTime;
    property Repeats:         TIdSdpRepeats         read GetRepeats;
    property StartTime:       TIdNtpTimestamp       read fStartTime write fStartTime;
    property ZoneAdjustments: TIdSdpZoneAdjustments read GetZoneAdjustments;
  end;

  TIdSdpList = class(TIdPrintable)
  protected
    List: TObjectList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    function  Count: Integer;
    function  Contains(O: TObject): Boolean;
    procedure PrintOn(Dest: TStream); override;
    procedure Remove(O: TObject);
  end;

  TIdSdpAttributes = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpAttribute;
  public
    procedure Add(Att: TIdSdpAttribute); overload;
    procedure Add(A: TIdSdpAttributes); overload;
    procedure Add(const NameAndValue: String); overload;

    property Items[Index: Integer]: TIdSdpAttribute read GetItems; default;
  end;

  TIdSdpRTPMapAttributes = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpRTPMapAttribute;
  public
    procedure Add(Att: TIdSdpRTPMapAttribute);

    property Items[Index: Integer]: TIdSdpRTPMapAttribute read GetItems; default;
  end;

  TIdSdpBandwidths = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpBandwidth;
  public
    procedure Add(BW: TIdSdpBandwidth);

    property Items[Index: Integer]: TIdSdpBandwidth read GetItems; default;
  end;

  TIdSdpConnections = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpConnection;
  public
    procedure Add(C: TIdSdpConnection); overload;
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
  public
    procedure Add(Desc: TIdSdpMediaDescription);
    function  AllDescriptionsHaveConnections: Boolean;

    property Items[Index: Integer]: TIdSdpMediaDescription read GetItems; default;
  end;

  TIdSdpRepeats = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpRepeat;
  public
    procedure Add(R: TIdSdpRepeat);

    property Items[Index: Integer]: TIdSdpRepeat read GetItems; default;
  end;

  TIdSdpTimes = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpTime;
  public
    procedure Add(T: TIdSdpTime);

    property Items[Index: Integer]: TIdSdpTime read GetItems; default;
  end;

  TIdSdpZoneAdjustments = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpZoneAdjustment;
  public
    procedure Add(Adj: TIdSdpZoneAdjustment);

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
    procedure PrintEmailAddressField(Dest: TStream);
    procedure PrintInfo(Dest: TStream);
    procedure PrintPhoneNumber(Dest: TStream);
    procedure PrintSessionNameField(Dest: TStream);
    procedure PrintUriField(Dest: TStream);
    procedure PrintVersionField(Dest: TStream);

    property Attributes:        TIdSdpAttributes        read GetAttributes;
    property Connections:       TIdSdpConnections       read GetConnections;
    property MediaDescriptions: TIdSdpMediaDescriptions read GetMediaDescriptions;
  public
    class function CreateFrom(Src: TStream): TIdSdpPayload; overload;
    class function CreateFrom(Src: String): TIdSdpPayload; overload;

    destructor Destroy; override;

    procedure AddAttribute(const Name, Value: String);
    procedure AddConnection(NewConnection: TIdSdpConnection); overload;
    function  AddConnection: TIdSdpConnection; overload;
    procedure AddMediaDescription(NewDesc: TIdSdpMediaDescription); overload;
    function  AddMediaDescription: TIdSdpMediaDescription; overload;
    function  AllDescriptionsHaveConnections: Boolean;
    function  AttributeAt(Index: Integer): TIdSdpAttribute;
    function  AttributeCount: Integer;
    function  AsString: String;
    function  ConnectionAt(Index: Integer): TIdSdpConnection;
    function  ConnectionCount: Integer;
    procedure GetRtpMapAttributes(Atts: TIdSdpRTPMapAttributes);
    function  HasKey: Boolean;
    procedure InitializeProfile(Profile: TIdRTPProfile);
    function  MediaDescriptionAt(Index: Integer): TIdSdpMediaDescription;
    function  MediaDescriptionCount: Integer;
    procedure PrintOn(Dest: TStream);
    procedure ReadFrom(Src: TStream);

    property Bandwidths:   TIdSdpBandwidths    read GetBandwidths;
    property EmailAddress: TIdEMailAddressItem read GetEmailAddress;
    property Info:         String              read fInfo write fInfo;
    property Key:          TIdSdpKey           read GetKey;
    property Origin:       TIdSdpOrigin        read GetOrigin;
    property PhoneNumber:  String              read fPhoneNumber write fPhoneNumber;
    property SessionName:  String              read fSessionName write fSessionName;
    property Times:        TIdSdpTimes         read GetTimes;
    property URI:          String              read fUri write fUri;
    property Version:      Cardinal            read fVersion write fVersion;
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
    class function IsKeyData(const Token: String): Boolean;
    class function IsKeyType(const Token: String): Boolean;
    class function IsMediaType(const Token: String): Boolean;
    class function IsMulticastAddress(IpVersion: TIdIPVersion; const Token: String): Boolean;
    class function IsNetType(const Token: String): Boolean;
    class function IsPhone(const Token: String): Boolean;
    class function IsPhoneNumber(const Header: String): Boolean;
    class function IsPort(const Token: String): Boolean;
    class function IsText(const Token: String): Boolean;
    class function IsTime(const Token: String): Boolean;
    class function IsTransport(const Token: String): Boolean;

    procedure Parse(Payload: TIdSdpPayload);
  end;

  TIdFilteredRTPPeer = class(TIdBaseRTPAbstractPeer,
                             IIdRTPListener)
  private
    fLocalDescription:  TIdSdpMediaDescription;
    fRemoteDescription: TIdSdpMediaDescription;
    fPeer:              Pointer;

    function  GetPeer: IIdAbstractRTPPeer;
    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdSocketHandle);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdSocketHandle);
  public
    constructor Create(Peer: IIdAbstractRTPPeer;
                       LocalDescription,
                       RemoteDescription: TIdSdpMediaDescription); reintroduce;

    procedure SendPacket(const Host: String;
                         Port: Cardinal;
                         Packet: TIdRTPBasePacket); override;

    property LocalDescription:  TIdSdpMediaDescription read fLocalDescription;
    property RemoteDescription: TIdSdpMediaDescription read fRemoteDescription;
    property Peer:              IIdAbstractRTPPeer     read GetPeer;
  end;

  // I process SDP (RFC 2327) payloads. This means that I instantiate (RTP)
  // servers on appropriate ports based on a local session description.
  // You can give me a remote session description too, which allows you to
  // use me to send (RTP) data to the remote peer.
  TIdSdpPayloadProcessor = class(TIdInterfacedObject,
                                 IIdRTPListener)
  private
    DataListenerLock:          TCriticalSection;
    DataListeners:             TList;
    fBasePort:                 Integer;
    fHost:                     String;
    Filters:                   TObjectList;
    fUsername:                 String;
    fProfile:                  TIdRTPProfile;
    fRemoteSessionDescription: String;
    fTransportType:            TIdIPVersion;
    RTPClients:                TObjectList;
    RTPClientLock:             TCriticalSection;
    RTPServerLock:             TCriticalSection;
    RTPServers:                TObjectList;

    procedure ActivateServerOnNextFreePort(Server: TIdRTPServer;
                                           StartFrom: Cardinal);
    function  AddPeer(MediaDesc: TIdSdpMediaDescription;
                      List: TObjectList): TIdRTPServer;
    function  DefaultBasePort: Integer;
    function  DefaultHost: String;
    function  DefaultUsername: String;
    procedure NotifyOfNewRTPData(Data: TIdRTPPayload;
                                 Binding: TIdSocketHandle);
    procedure OnRTCP(Packet: TIdRTCPPacket;
                     Binding: TIdSocketHandle);
    procedure OnRTP(Packet: TIdRTPPacket;
                    Binding: TIdSocketHandle);
    function  PeerAt(Index: Integer): TIdFilteredRTPPeer;
    function  ServerAt(Index: Integer): TIdRTPServer;
    procedure SetRemoteSessionDescription(const Value: String);
    procedure SetUpMediaStreams(RemoteDescription: TIdSdpPayload);
    procedure SetUpSingleStream(MediaDesc: TIdSdpMediaDescription);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddDataListener(const Listener: IIdRTPDataListener);
    function  IsListening: Boolean;
    function  LocalSessionDescription: String;
    function  MediaType: String;
    procedure RemoveDataListener(const Listener: IIdRTPDataListener);
    procedure SendData(Payload: TIdRTPPayload);
    function  SessionCount: Integer;
    procedure StartListening(const LocalSessionDescription: String);
    procedure StopListening;

    property BasePort:                 Integer       read fBasePort write fBasePort;
    property Host:                     String        read fHost write fHost;
    property TransportType:            TIdIPVersion  read fTransportType write fTransportType;
    property Profile:                  TIdRTPProfile read fProfile;
    property RemoteSessionDescription: String        read fRemoteSessionDescription write SetRemoteSessionDescription;
    property Username:                 String        read fUsername write fUsername;
  end;

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

const
  SafeChars = Alphabet + Digits + ['''', '-', '.', '/', ':', '?', '#',
               '$', '&', '*', ';', '=', '@', '[', ']', '^', '_', '`', '{', '|',
               '}', '+', '~', '"'];
  EmailSafeChars = SafeChars + [' ', #9];
  IllegalByteStringChars = [#0, #10, #13];
  RTPMapAttribute    = 'rtpmap';
  TimeTypes          = ['d', 'h', 'm', 's'];

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
  // IANA assigned nettype
  Id_SDP_IN = 'IN';
  // IANA assigned addrtype
  Id_SDP_IP4 = 'IP4';
  Id_SDP_IP6 = 'IP6';
  // IANA assigned keytype
  Id_SDP_Clear  = 'clear';
  Id_SDP_Base64 = 'base64';
  Id_SDP_URI    = 'uri';
  Id_SDP_Prompt = 'prompt';
  // IANA assigned protos
  Id_SDP_RTPAVP = 'RTP/AVP';
  Id_SDP_udp    = 'udp';
  Id_SDP_vat    = 'vat';
  Id_SDP_rtp    = 'rtp';
  Id_SDP_UDPTL  = 'UDPTL';
  Id_SDP_TCP    = 'TCP';

// for IdResourceStrings
const
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

function AddressTypeToStr(const Version: TIdIPVersion): String;
function BandwidthTypeToStr(const BwType: TIdSdpBandwidthType): String;
function KeyTypeToStr(const KeyType: TIdSdpKeyType): String;
function MediaTypeToStr(const MediaType: TIdSdpMediaType): String;
function StrToAddressType(const S: String): TIdIPVersion;
function StrToBandwidthType(const S: String): TIdSdpBandwidthType;
function StrToKeyType(const S: String): TIdSDPKeyType;
function StrToMediaType(const S: String): TIdSDPMediaType;

implementation

uses
  IdGlobal, SysUtils;

const
  SessionHeaderOrder = 'vosiuepcbtka';
  MediaHeaderOrder   = 'micbka';  

//******************************************************************************
//* Unit public functions and procedures                                       *
//******************************************************************************

function AddressTypeToStr(const Version: TIdIPVersion): String;
begin
  case Version of
    Id_IPv4: Result := Id_SDP_IP4;
    Id_IPv6: Result := Id_SDP_IP6;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdIPVersion',
                                       Ord(Version),
                                       'String']));
  end;
end;

function BandwidthTypeToStr(const BwType: TIdSdpBandwidthType): String;
begin
  case BwType of
    btConferenceTotal:     Result := Id_SDP_ConferenceTotal;
    btApplicationSpecific: Result := Id_SDP_ApplicationSpecific;
    btRS:                  Result := Id_SDP_RS;
    btRR:                  Result := Id_SDP_RR;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdSdpBandwidthType',
                                       Ord(BwType),
                                       'String']));
  end;
end;

function KeyTypeToStr(const KeyType: TIdSdpKeyType): String;
begin
  case KeyType of
    ktClear:  Result := Id_SDP_Clear;
    ktBase64: Result := Id_SDP_Base64;
    ktURI:    Result := Id_SDP_URI;
    ktPrompt: Result := Id_SDP_Prompt;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdSdpKeyType',
                                       Ord(KeyType),
                                       'String']));
  end;
end;

function MediaTypeToStr(const MediaType: TIdSdpMediaType): String;
begin
  case MediaType of
    mtAudio:       Result := RSSDPMediaTypeAudio;
    mtVideo:       Result := RSSDPMediaTypeVideo;
    mtApplication: Result := RSSDPMediaTypeApplication;
    mtData:        Result := RSSDPMediaTypeData;
    mtControl:     Result := RSSDPMediaTypeControl;
    mtText:        Result := RSSDPMediaTypeText;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg,
                                      ['TIdSdpMediaType',
                                       Ord(MediaType),
                                       'String']));
  end;
end;

function StrToAddressType(const S: String): TIdIPVersion;
begin
       if (S = Id_SDP_IP4) then Result := Id_IPv4
  else if (S = Id_SDP_IP6) then Result := Id_IPv6
  else
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdIPVersion']));
end;

function StrToBandwidthType(const S: String): TIdSdpBandwidthType;
begin
       if (S = Id_SDP_ConferenceTotal)     then Result := btConferenceTotal
  else if (S = Id_SDP_ApplicationSpecific) then Result := btApplicationSpecific
  else if (S = Id_SDP_RS)                  then Result := btRS
  else if (S = Id_SDP_RR)                  then Result := btRR
  else
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdSdpBandwidthType']));
end;

function StrToKeyType(const S: String): TIdSDPKeyType;
begin
       if (S = Id_SDP_Clear)  then Result := ktClear
  else if (S = Id_SDP_Base64) then Result := ktBase64
  else if (S = Id_SDP_URI)    then Result := ktURI
  else if (S = Id_SDP_Prompt) then Result := ktPrompt
  else
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdSdpKeyType']));
end;

function StrToMediaType(const S: String): TIdSDPMediaType;
begin
       if (S = RSSDPMediaTypeAudio)       then Result := mtAudio
  else if (S = RSSDPMediaTypeVideo)       then Result := mtVideo
  else if (S = RSSDPMediaTypeApplication) then Result := mtApplication
  else if (S = RSSDPMediaTypeData)        then Result := mtData
  else if (S = RSSDPMediaTypeControl)     then Result := mtControl
  else if (S = RSSDPMediaTypeText)        then Result := mtText
  else
    raise EConvertError.Create(Format(ConvertStrErrorMsg,
                                      [S, 'TIdSdpMediaType']));
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

function TIdSdpAttribute.Copy: TIdSdpAttribute;
begin
  Result := TIdSdpAttributeClass(Self.ClassType).Create;
  try
    Result.Name  := Self.Name;
    Result.Value := Self.Value;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSdpAttribute.IsRTPMap: Boolean;
begin
  Result := false;
end;

procedure TIdSdpAttribute.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := #13#10'a=' + Self.Name;

  if (Self.Value <> '') then
    S := S + ':' + Self.Value;

  Dest.Write(PChar(S)^, Length(S));
end;

//* TIdSdpAttribute Private methods ********************************************

function TIdSdpAttribute.GetName: String;
begin
  Result := fName;
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

procedure TIdSdpRTPMapAttribute.SetValue(const Value: String);
var
  EncodingDesc: String;
  PayloadType:  String;
  E, N:         Integer;
begin
  inherited SetValue(Value);

  EncodingDesc := Value;
  PayloadType := Fetch(EncodingDesc, ' ');

  Val(PayloadType, N, E);
  if (E <> 0) then
    raise EParserError.Create(Format(MalformedToken, [Value]));
  Self.PayloadType := N;

  Self.SetEncoding(TIdRTPPayload.CreatePayload(EncodingDesc));
end;

//* TIdSdpRTPMapAttribute Private methods **************************************

procedure TIdSdpRTPMapAttribute.SetEncoding(const Value: TIdRTPPayload);
begin
  Self.Encoding.Free;
  Self.fEncoding := Value;
end;

//******************************************************************************
//* TIdSdpBandwidth                                                            *
//******************************************************************************
//* TIdSdpBandwidth Public methods *********************************************

procedure TIdSdpBandwidth.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := #13#10'b=' + BandwidthTypeToStr(Self.BandwidthType) + ':'
                  + IntToStr(Self.Bandwidth);

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpConnection                                                           *
//******************************************************************************
//* TIdSdpConnection Public methods ********************************************

function TIdSdpConnection.Copy: TIdSdpConnection;
begin
  Result := TIdSdpConnection.Create;
  try
    Result.AddressType       := Self.AddressType;
    Result.Address           := Self.Address;
    Result.NetType           := Self.NetType;
    Result.NumberOfAddresses := Self.NumberOfAddresses;
    Result.TTL               := Self.TTL;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

procedure TIdSdpConnection.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := S + #13#10'c=' + Self.NetType + ' '
         + AddressTypeToStr(Self.AddressType)
         + ' ' + Self.Address;

  if (Self.TTL > 0) then begin
    S := S + '/' + IntToStr(Self.TTL);

    if (Self.NumberOfAddresses > 0) then begin
      S := S + '/' + IntToStr(Self.NumberOfAddresses);
    end;
  end;

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpKey                                                                  *
//******************************************************************************
//* TIdSdpKey Public methods ***************************************************

procedure TIdSdpKey.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := #13#10'k=' + KeyTypeToStr(Self.KeyType);

  if (Self.KeyType <> ktPrompt) then
    S := S + ':' + Self.Value;

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

  Self.FormatList.Free;  

  inherited Destroy;
end;

procedure TIdSdpMediaDescription.AddAttribute(const Name, Value: String);
begin
  Self.Attributes.Add(Name + ':' + Value);
end;

function TIdSdpMediaDescription.AttributeAt(Index: Integer): TIdSdpAttribute;
begin
  Result := Self.Attributes[Index];
end;

function TIdSdpMediaDescription.AttributeCount: Integer;
begin
  Result := Self.Attributes.Count;
end;

procedure TIdSdpMediaDescription.AddFormat(const Fmt: String);
begin
  Self.FormatList.Add(Fmt);
end;

function TIdSdpMediaDescription.FormatCount: Integer;
begin
  Result := Self.FormatList.Count;
end;

procedure TIdSdpMediaDescription.ClearFormats;
begin
  Self.FormatList.Clear;
end;

function TIdSdpMediaDescription.HasConnection: Boolean;
begin
  Result := Self.Connections.Count > 0;
end;

function TIdSdpMediaDescription.HasFormat(Fmt: String): Boolean;
begin
  Result := Self.FormatList.IndexOf(Fmt) <> -1;
end;

function TIdSdpMediaDescription.HasKey: Boolean;
begin
  Result := Assigned(fKey);
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

  Self.Attributes.PrintOn(Dest);
end;

//* TIdSdpMediaDescription Private methods *************************************

function TIdSdpMediaDescription.GetAttributes: TIdSdpAttributes;
begin
  if not Assigned(fAttributes) then
    fAttributes := TIdSdpAttributes.Create;

  Result := fAttributes;
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
    S := #13#10'i=' + Self.Info;

    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpMediaDescription.PrintMediaField(Dest: TStream);
var
  I: Integer;
  S: String;
begin
  S := #13#10
     + 'm=' + MediaTypeToStr(Self.MediaType) + ' '
     + IntToStr(Self.Port);

  if (Self.PortCount > 1) then
    S := S + '/' + IntToStr(PortCount);

  S := S + ' ' + Self.Transport;

  for I := 0 to Self.FormatCount - 1 do
    S := S + ' ' + Self.Formats[I];

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpOrigin                                                               *
//******************************************************************************
//* TIdSdpOrigin Public methods ************************************************

procedure TIdSdpOrigin.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := #13#10
     + 'o=' + Self.Username + ' '
     + Self.SessionID + ' '
     + Self.SessionVersion + ' '
     + Self.NetType + ' '
     + AddressTypeToStr(Self.AddressType) + ' '
     + Self.Address;

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpRepeat                                                               *
//******************************************************************************
//* TIdSdpRepeat Public methods ************************************************

procedure TIdSdpRepeat.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := #13#10
     + 'r=' + Self.Value;

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpZoneAdjustment                                                       *
//******************************************************************************
//* TIdSdpZoneAdjustment Public methods ****************************************

procedure TIdSdpZoneAdjustment.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := #13#10
     + 'z=' + Self.Value;

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

procedure TIdSdpTime.PrintOn(Dest: TStream);
var
  S: String;
begin
  S := #13#10't=' + IntToStr(Self.StartTime) + ' ' + IntToStr(Self.EndTime);
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

function TIdSdpList.Count: Integer;
begin
  Result := Self.List.Count;
end;

function TIdSdpList.Contains(O: TObject): Boolean;
begin
  Result := Self.List.IndexOf(O) <> -1;
end;

procedure TIdSdpList.PrintOn(Dest: TStream);
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    (Self.List[I] as TIdPrintable).PrintOn(Dest);
end;

procedure TIdSdpList.Remove(O: TObject);
begin
  Self.List.Remove(O);
end;

//******************************************************************************
//* TIdSdpAttributes                                                           *
//******************************************************************************
//* TIdSdpAttributes Public methods ********************************************

procedure TIdSdpAttributes.Add(Att: TIdSdpAttribute);
begin
  Self.List.Add(Att);
end;

procedure TIdSdpAttributes.Add(A: TIdSdpAttributes);
var
  I:       Integer;
  NewAtt: TIdSdpAttribute;
begin
  for I := 0 to A.Count - 1 do begin
    NewAtt := TIdSdpAttribute.Create;
    try
      Self.Add(NewAtt);
      NewAtt.Name  := A[I].Name;
      NewAtt.Value := A[I].Value;
    except
      if Self.Contains(NewAtt) then
        Self.Remove(NewAtt)
      else
        FreeAndNil(NewAtt);
        
      raise;
    end;
  end;
end;

procedure TIdSdpAttributes.Add(const NameAndValue: String);
begin
  Self.Add(TIdSdpAttribute.CreateAttribute(NameAndValue));
end;

//* TIdSdpAttributes Private methods *******************************************

function TIdSdpAttributes.GetItems(Index: Integer): TIdSdpAttribute;
begin
  Result := Self.List[Index] as TIdSdpAttribute;
end;

//******************************************************************************
//* TIdSdpRTPMapAttributes                                                     *
//******************************************************************************
//* TIdSdpRTPMapAttributes Public methods **************************************

procedure TIdSdpRTPMapAttributes.Add(Att: TIdSdpRTPMapAttribute);
begin
  Self.List.Add(Att);
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

procedure TIdSdpBandwidths.Add(BW: TIdSdpBandwidth);
begin
  Self.List.Add(BW);
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

procedure TIdSdpConnections.Add(C: TIdSdpConnection);
begin
  Self.List.Add(C);
end;

procedure TIdSdpConnections.Add(C: TIdSdpConnections);
var
  I:       Integer;
  NewConn: TIdSdpConnection;
begin
  for I := 0 to C.Count - 1 do begin
    NewConn := TIdSdpConnection.Create;
    try
      NewConn.AddressType       := C[I].AddressType;
      NewConn.Address           := C[I].Address;
      NewConn.NetType           := C[I].NetType;
      NewConn.NumberOfAddresses := C[I].NumberOfAddresses;
      NewConn.TTL               := C[I].TTL;

      Self.Add(NewConn);      
    except
      if Self.Contains(NewConn) then
        Self.Remove(NewConn)
      else
        FreeAndNil(NewConn);
      raise;
    end;
  end;
end;

procedure TIdSdpConnections.AddConnection(NetType: String;
                                          AddrType: TIdIPVersion;
                                          Addr: String;
                                          TTL: Byte);
var
  NewConnection: TIdSdpConnection;
begin
  NewConnection := TIdSdpConnection.Create;
  try
    NewConnection.NetType     := NetType;
    NewConnection.AddressType := AddrType;

    NewConnection.Address := Addr;
    NewConnection.TTL := TTL;
    Self.Add(NewConnection);
  except
    if Self.Contains(NewConnection) then
      Self.Remove(NewConnection)
    else
      FreeAndNil(NewConnection);

    raise;
  end;
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

procedure TIdSdpMediaDescriptions.Add(Desc: TIdSdpMediaDescription);
begin
  Self.List.Add(Desc);
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

//* TIdSdpMediaDescriptions Private methods ************************************

function TIdSdpMediaDescriptions.GetItems(Index: Integer): TIdSdpMediaDescription;
begin
  Result := Self.List[Index] as TIdSdpMediaDescription;
end;

//******************************************************************************
//* TIdSdpRepeats                                                              *
//******************************************************************************
//* TIdSdpRepeats Public methods ***********************************************

procedure TIdSdpRepeats.Add(R: TIdSdpRepeat);
begin
  Self.List.Add(R);
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

procedure TIdSdpTimes.Add(T: TIdSdpTime);
begin
  Self.List.Add(T);
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

procedure TIdSdpZoneAdjustments.Add(Adj: TIdSdpZoneAdjustment);
begin
  Self.List.Add(Adj);
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
  fTimes.Free;

  inherited Destroy;
end;

procedure TIdSdpPayload.AddAttribute(const Name, Value: String);
begin
  Self.Attributes.Add(Name + ':' + Value);
end;

procedure TIdSdpPayload.AddConnection(NewConnection: TIdSdpConnection);
var
  I: Integer;
begin
  Self.Connections.Add(NewConnection);
  for I := 0 to Self.MediaDescriptionCount - 1 do
    Self.MediaDescriptionAt(I).Connections.Add(NewConnection.Copy);
end;

function TIdSdpPayload.AddConnection: TIdSdpConnection;
begin
  Result := TIdSdpConnection.Create;
  try
    Self.AddConnection(Result);
  except
    Self.Connections.Remove(Result);
    FreeAndNil(Result);

    raise;
  end;
end;

procedure TIdSdpPayload.AddMediaDescription(NewDesc: TIdSdpMediaDescription);
begin
  Self.MediaDescriptions.Add(NewDesc);
  NewDesc.Attributes.Add(Self.Attributes);
  NewDesc.Connections.Add(Self.Connections);
end;

function TIdSdpPayload.AddMediaDescription: TIdSdpMediaDescription;
begin
  Result := TIdSdpMediaDescription.Create;
  try
    Self.AddMediaDescription(Result);
  except
    Self.MediaDescriptions.Remove(Result);
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSdpPayload.AllDescriptionsHaveConnections: Boolean;
begin
  Result := Self.MediaDescriptions.AllDescriptionsHaveConnections;
end;

function TIdSdpPayload.AttributeAt(Index: Integer): TIdSdpAttribute;
begin
  Result := Self.Attributes[Index];
end;

function TIdSdpPayload.AttributeCount: Integer;
begin
  Result := Self.Attributes.Count;
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

procedure TIdSdpPayload.GetRtpMapAttributes(Atts: TIdSdpRTPMapAttributes);
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to Self.AttributeCount - 1 do
    if (Self.AttributeAt(I).IsRTPMap) then
      Atts.Add(Self.AttributeAt(I).Copy as TIdSdpRTPMapAttribute);

  for I := 0 to Self.MediaDescriptionCount - 1 do
    for J := 0 to Self.MediaDescriptionAt(I).AttributeCount - 1 do
      if (Self.MediaDescriptionAt(I).AttributeAt(J).IsRTPMap) then
        Atts.Add(Self.MediaDescriptionAt(I).AttributeAt(J).Copy as TIdSdpRTPMapAttribute);
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
                          RTPMaps[I].PayloadType);
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

procedure TIdSdpPayload.PrintEmailAddressField(Dest: TStream);
var
  S: String;
begin
  if (Self.EmailAddress.Address <> '') then begin
    S := #13#10'e=' + Self.EmailAddress.Address;
    
    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpPayload.PrintInfo(Dest: TStream);
var
  S: String;
begin
  if (Self.Info <> '') then begin
    S := #13#10'i=' + Self.Info;

    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpPayload.PrintPhoneNumber(Dest: TStream);
var
  S: String;
begin
  if (Self.PhoneNumber <> '') then begin
    S := #13#10'p=' + Self.PhoneNumber;
    
    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpPayload.PrintSessionNameField(Dest: TStream);
var
  S: String;
begin
  if (Self.SessionName <> '') then begin
    S := #13#10's=' + Self.SessionName;

    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpPayload.PrintUriField(Dest: TStream);
var
  S: String;
begin
  // TODO: I hate stinking TIdURI
  if (Self.URI <> '') then begin
    S := #13#10'u=' + Self.URI;

    Dest.Write(PChar(S)^, Length(S));
  end;
end;

procedure TIdSdpPayload.PrintVersionField(Dest: TStream);
var
  S: String;
begin
  S := 'v=' + IntToStr(Self.Version);

  Dest.Write(PChar(S)^, Length(S));
end;

//******************************************************************************
//* TIdSdpParser                                                               *
//******************************************************************************
//* TIdSdpParser Public methods ************************************************

class function TIdSdpParser.IsAddressType(const Token: String): Boolean;
begin
  try
    StrToAddressType(Token);
    Result := true;
  except
    on EConvertError do Result := false;
  end;
end;

class function TIdSdpParser.IsBandwidthType(const Token: String): Boolean;
begin
  try
    StrToBandwidthType(Token);
    Result := true;
  except
    on EConvertError do Result := false;
  end;
end;

class function TIdSdpParser.IsByteString(const Token: String): Boolean;
var
  I: Integer;
begin
  Result := Token <> '';

  if Result then
    for I := 1 to Length(Token) do
      Result := Result and not (Token[I] in IllegalByteStringChars);
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
  try
    StrToKeyType(Token);
    Result := true;
  except
    on EConvertError do Result := false;
  end;
end;

class function TIdSdpParser.IsMediaType(const Token: String): Boolean;
begin
  try
    StrToMediaType(Token);
    Result := true;
  except
    on EConvertError do Result := false;
  end;
end;

class function TIdSdpParser.IsMulticastAddress(IpVersion: TIdIPVersion; const Token: String): Boolean;
var
  Address: String;
  N:       String;
begin
  Address := Token;

  case IpVersion of
    Id_IPv4: begin
      Result := TIdIPAddressParser.IsIPv4Address(Address);
      N := Fetch(Address, '.');
      Result := Result and (StrToInt(N) = 224);
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
var
  I: Integer;
begin
  Result := Length(Token) >= 3;

  if Result then begin
    Result := Result and (Token[1] = '+');
    Result := Result and (Token[2] in ['1'..'9']);

    for I := 3 to Length(Token) do
      Result := Result and (Token[I] in ['0'..'9', '-', ' ']);
  end;
end;

class function TIdSdpParser.IsPhoneNumber(const Header: String): Boolean;
var
  Token, S: String;
  I:        Integer;
begin
  Result := true;

  S := Header;
  if (IndyPos('<', S) > 0) then begin
    Token := Fetch(S, '<');
    for I := 1 to Length(Token) do
      Result := Result and (Token[I] in EmailSafeChars);

    Token := Fetch(S, '>');
    Result := Result and Self.IsPhone(Token);

    Result := Result and (S = '');
  end else begin
    if (IndyPos('(', S) > 0) then begin
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
    Result := Result and (E = 0) and (0 <= N) and (N < 65536);
  end;
end;

class function TIdSdpParser.IsText(const Token: String): Boolean;
var
  I: Integer;
begin
  Result := (Token <> '');

  if Result then
    for I := 1 to Length(Token) do begin
      Result := Result and not (Token[I] in [#0, #10, #13]);
    end;
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

class function TIdSdpParser.IsTransport(const Token: String): Boolean;
begin
  Result := (Token = Id_SDP_RTPAVP)
         or (Token = Id_SDP_vat)
         or (Token = Id_SDP_rtp)
         or (Token = Id_SDP_UDPTL)
         or (Token = Id_SDP_TCP);
end;

procedure TIdSdpParser.Parse(Payload: TIdSdpPayload);
begin
  Self.ParseSessionHeaders(Payload);

  while not Self.Eof do
    Self.ParseMediaDescription(Payload);

  if (Payload.Connections.Count = 0)
     and not ((Payload.MediaDescriptionCount > 0)
              and Payload.AllDescriptionsHaveConnections) then
    raise EParserError.Create(MissingConnection);
end;

//* TIdSdpParser Private methods ***********************************************

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

  if (IndyPos(LastHeader, HeaderOrder) > IndyPos(CurrentHeader, HeaderOrder)) then
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
                                [RSSDPInformationName, Name + '=' + Value]));
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
                                  [RSSDPAttributeName, Name + '=' + OriginalValue]));

    if (Att.Value <> '') and not Self.IsByteString(Att.Value) then
      raise EParserError.Create(Format(MalformedToken,
                                  [RSSDPAttributeName, Name + '=' + OriginalValue]));

    Attributes.Add(Att);
  except
    if not Attributes.Contains(Att) then
      Att.Free;

    raise;
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

  BW := TIdSdpBandwidth.Create;
  try
    Token := Fetch(Value, ':');

    if not Self.IsBandwidthType(Token) then
      raise EParserError.Create(Format(MalformedToken,
                                  [RSSDPBandwidthName, Name + '=' + OriginalValue]));

    BW.BandwidthType := StrToBandwidthType(Token);

    // We should just be able to take the rest of the string. However, as of
    // this change, there's at least one SIP stack that uses a space in the
    // bandwidth. Bastards.
    Token := Fetch(Value, ' ');
    if not Self.IsNumber(Token) then
      raise EParserError.Create(Format(MalformedToken,
                                  [RSSDPBandwidthName, Name + '=' + OriginalValue]));
    BW.Bandwidth := StrToInt(Token);

    Bandwidths.Add(BW);
  except
    if not Bandwidths.Contains(BW) then
      BW.Free;

    raise;
  end;

  if Self.ParsingSessionHeaders then
    Self.LastSessionHeader := RSSDPBandwidthName
  else
    Self.LastMediaHeader := RSSDPBandwidthName;
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
                                [RSSDPConnectionName, Name + '=' + OriginalValue]));

  AddrType := Fetch(Value, ' ');
  if not Self.IsAddressType(AddrType) then
    raise EParserError.Create(Format(MalformedToken,
                                [RSSDPConnectionName, Name + '=' + OriginalValue]));

  Multicast := IndyPos('/', Value) > 0;

  if Multicast then begin
    Addr := Fetch(Value, '/');
    if not Self.IsMulticastAddress(StrToAddressType(AddrType), Addr)
      and not Self.IsFQDN(Addr) then
      raise EParserError.Create(Format(MalformedToken,
                                  [RSSDPConnectionName, Name + '=' + OriginalValue]));

    TTL := Fetch(Value, '/');
    if not Self.IsByte(TTL) then
      raise EParserError.Create(Format(MalformedToken,
                                  [RSSDPConnectionName, Name + '=' + OriginalValue]));

    NumAddrs := Value;
  end
  else begin
    Addr     := Value;
    NumAddrs := '';
    TTL      := '0';    

    if not TIdIPAddressParser.IsIPAddress(StrToAddressType(AddrType), Value)
      and not Self.IsFQDN(Value) then
      raise EParserError.Create(Format(MalformedToken,
                                  [RSSDPConnectionName, Name + '=' + OriginalValue]));
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

  if (IndyPos(':', Value) > 0) then
    Token := Fetch(Value, ':')
  else begin
    Token := Value;
    Value := '';
  end;

  if not Self.IsKeyType(Token) then
    raise EParserError.Create(Format(MalformedToken, [RSSDPKeyName, Name + '=' + OriginalValue]));

  Key.KeyType := StrToKeyType(Token);

  if (Key.KeyType = ktPrompt) then begin
    if (Value <> '') then
      raise EParserError.Create(Format(MalformedToken, [RSSDPKeyName, Name + '=' + OriginalValue]))
  end
  else begin
    if Self.IsKeyData(Value) then
      Key.Value := Value
    else
      raise EParserError.Create(Format(MalformedToken, [RSSDPKeyName, Name + '=' + OriginalValue]));
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
      raise EParserError.Create(Format(MalformedToken, [RSSDPMediaDescriptionName,
                                                   Name + '=' + OriginalValue]));
    NewMediaDesc.MediaType := StrToMediaType(Token);

    Token := Fetch(Value, ' ');
    if (IndyPos('/', Token) > 0) then begin
      Count := Token;
      Token := Fetch(Count, '/');
    end;

    if not Self.IsPort(Token) then
      raise EParserError.Create(Format(MalformedToken, [RSSDPMediaDescriptionName,
                                                   Name + '=' + OriginalValue]));
      NewMediaDesc.Port := StrToInt(Token);

    if (Count <> '') and not Self.IsNumber(Count) then
      raise EParserError.Create(Format(MalformedToken, [RSSDPMediaDescriptionName,
                                                   Name + '=' + OriginalValue]));
    NewMediaDesc.PortCount := StrToIntDef(Count, 1);

    Token := Fetch(Value, ' ');
    if not Self.IsTransport(Token) then
      raise EParserError.Create(Format(MalformedToken, [RSSDPMediaDescriptionName,
                                                   Name + '=' + OriginalValue]));
    NewMediaDesc.Transport := Token;

    while (Value <> '') do begin
      Token := Fetch(Value, ' ');
      if not Self.IsAlphaNumeric(Token) then
        raise EParserError.Create(Format(MalformedToken, [RSSDPMediaDescriptionName,
                                                     Name + '=' + OriginalValue]));
      NewMediaDesc.AddFormat(Token);
    end;

    if (NewMediaDesc.FormatCount = 0) then
      raise EParserError.Create(Format(MalformedToken, [RSSDPMediaDescriptionName,
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
      RSSDPAttributeName:   Self.ParseAttribute(MediaDescription.Attributes);
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

  // Cf RFC 2327 Appendix A and meditate on the production "username = safe".
  // Note, please, that the SDP examples clearly show that username has more
  // than one character, normally, so username SHOULD be either 1*(safe) or
  // *(safe). We don't know, ergo 'o= 467752 467752 IN IP4 192.168.1.41' might
  // be legal (meaning username = '').
//  if (Payload.Origin.Username = '') then
//    raise EParserError.Create(Format(MalformedToken, [RSSDPOriginName, Name + '=' + OriginalValue]));

  Token := Fetch(Value, ' ');
  if not Self.IsNumber(Token) then
    raise EParserError.Create(Format(MalformedToken, [RSSDPOriginName, Name + '=' + OriginalValue]));
  Payload.Origin.SessionID := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsNumber(Token) then
    raise EParserError.Create(Format(MalformedToken, [RSSDPOriginName, Name + '=' + OriginalValue]));
  Payload.Origin.SessionVersion := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsNetType(Token) then
    raise EParserError.Create(Format(MalformedToken, [RSSDPOriginName, Name + '=' + OriginalValue]));
  Payload.Origin.NetType := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsAddressType(Token) then
    raise EParserError.Create(Format(MalformedToken, [RSSDPOriginName, Name + '=' + OriginalValue]));

  Payload.Origin.AddressType := StrToAddressType(Token);

  Payload.Origin.Address := Value;
  if (Payload.Origin.Address = '') then
    raise EParserError.Create(Format(MalformedToken, [RSSDPOriginName, Name + '=' + OriginalValue]));

  Self.LastSessionHeader := RSSDPOriginName;
end;

procedure TIdSdpParser.ParsePhone(Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

  if not Self.IsPhoneNumber(Value) then
    raise EParserError.Create(Format(MalformedToken, [RSSDPPhoneName, Name + '=' + Value]));

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
        raise EParserError.Create(Format(MalformedToken, [RSSDPRepeatName, Name + '=' + OriginalValue]));
    end;

    Time.Repeats.Add(Rpt);
  except
    if not Time.Repeats.Contains(Rpt) then
      Rpt.Free;

    raise;
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
      RSSDPAttributeName:   Self.ParseAttribute(Payload.Attributes);
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
    raise EParserError.Create(Format(MalformedToken, [RSSDPSessionName, Name + '=' + Value]));

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
      raise EParserError.Create(Format(MalformedToken, [RSSDPTimeName, Name + '=' + OriginalValue]));
    Time.StartTime := StrToInt64(Token);

    Token := Fetch(Value, ' ');
    if not Self.IsNumber(Token) then
      raise EParserError.Create(Format(MalformedToken, [RSSDPTimeName, Name + '=' + OriginalValue]));
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
//        raise EParserError.Create(Format(MalformedToken, [RSSDPRepeatName, Name + '=' + OriginalValue]));
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
//    raise EParserError.Create(Format(MalformedToken, [RSSDPUriName, Name + '=' + Value]));

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
                                     [RSSDPVersionName, Name + '=' + Value]));

  Payload.Version := N;
  Self.LastSessionHeader := RSSDPVersionName;
end;

//******************************************************************************
//* TIdFilteredRTPPeer                                                         *
//******************************************************************************
//* TIdFilteredRTPPeer Public methods ******************************************

constructor TIdFilteredRTPPeer.Create(Peer: IIdAbstractRTPPeer;
                                      LocalDescription,
                                      RemoteDescription: TIdSdpMediaDescription);
begin
  inherited Create;

  fLocalDescription  := LocalDescription;
  fRemoteDescription := RemoteDescription;
  fPeer              := Pointer(Peer);

  Self.Peer.AddListener(Self);
end;

procedure TIdFilteredRTPPeer.SendPacket(const Host: String;
                                        Port: Cardinal;
                                        Packet: TIdRTPBasePacket);
begin
  Self.Peer.SendPacket(Host, Port, Packet);
end;

//* TIdFilteredRTPPeer Private methods *****************************************

function TIdFilteredRTPPeer.GetPeer: IIdAbstractRTPPeer;
begin
  Result := IIdAbstractRTPPeer(Self.fPeer);
end;

procedure TIdFilteredRTPPeer.OnRTCP(Packet: TIdRTCPPacket;
                                    Binding: TIdSocketHandle);
begin
  Self.NotifyListenersOfRTCP(Packet, Binding);
end;

procedure TIdFilteredRTPPeer.OnRTP(Packet: TIdRTPPacket;
                                   Binding: TIdSocketHandle);
begin
  if Self.LocalDescription.HasFormat(IntToStr(Packet.PayloadType)) then
    Self.NotifyListenersOfRTP(Packet, Binding);
end;

//******************************************************************************
//* TIdSdpPayloadProcessor                                                     *
//******************************************************************************
//* TIdSdpPayloadProcessor Public methods **************************************

constructor TIdSdpPayloadProcessor.Create;
begin
  inherited Create;

  Self.BasePort := Self.DefaultBasePort;
  Self.Host     := Self.DefaultHost;
  Self.Username := Self.DefaultUsername;

  Self.DataListeners := TList.Create;
  Self.DataListenerLock := TCriticalSection.Create;

  Self.RTPClients    := TObjectList.Create(true);
  Self.RTPClientLock := TCriticalSection.Create;

  Self.RTPServers    := TObjectList.Create(true);
  Self.Filters       := TObjectList.Create(true);
  Self.RTPServerLock := TCriticalSection.Create;

  Self.fProfile      := TIdAudioVisualProfile.Create;
  Self.TransportType := Id_IPv4;
end;

destructor TIdSdpPayloadProcessor.Destroy;
begin
  Self.Profile.Free;

  Self.Filters.Free;
  Self.RTPServers.Free;
  Self.RTPServerLock.Free;

  Self.RTPClients.Free;
  Self.RTPClientLock.Free;

  Self.DataListeners.Free;
  Self.DataListenerLock.Free;

  inherited Destroy;
end;

procedure TIdSdpPayloadProcessor.AddDataListener(const Listener: IIdRTPDataListener);
begin
  Self.DataListenerLock.Acquire;
  try
    Self.DataListeners.Add(Pointer(Listener));
  finally
    Self.DataListenerLock.Release;
  end;
end;

function TIdSdpPayloadProcessor.IsListening: Boolean;
begin
  Self.RTPServerLock.Acquire;
  try
    Result := Self.RTPServers.Count > 0;

    if Result then
      Result := Result and Self.ServerAt(0).Active
  finally
    Self.RTPServerLock.Release;
  end;
end;

function TIdSdpPayloadProcessor.LocalSessionDescription: String;
const
  OLine = 'o=%s %u %u IN %s %s'#13#10;
var
  I: Integer;
begin
  Result := 'v=0'#13#10
          + Format(OLine, [Self.Username,
                           DateTimeToNtpSeconds(Now),
                           DateTimeToNtpSeconds(Now),
                           AddressTypeToStr(Self.TransportType),
                           Self.Host])
          + 's=-'#13#10
          + 'c=IN IP4 ' + Self.Host + #13#10;

  Self.RTPServerLock.Acquire;
  try
    for I := 0 to Self.RTPServers.Count - 1 do begin
      Result := Result + 'm=audio ';

      if (Self.ServerAt(I).Bindings.Count > 0) then
        Result := Result + IntToStr(Self.ServerAt(I).Bindings[0].Port)
      else
        Result := Result + IntToStr(Self.ServerAt(I).DefaultPort);

      Result := Result + ' ' + Self.Profile.TransportDesc + ' 0'#13#10;
    end;
  finally
    Self.RTPServerLock.Release;
  end;
end;

function TIdSdpPayloadProcessor.MediaType: String;
begin
  Result := SdpMimeType;
end;

procedure TIdSdpPayloadProcessor.RemoveDataListener(const Listener: IIdRTPDataListener);
begin
  Self.DataListenerLock.Acquire;
  try
    Self.DataListeners.Remove(Pointer(Listener));
  finally
    Self.DataListenerLock.Release;
  end;
end;

procedure TIdSdpPayloadProcessor.SendData(Payload: TIdRTPPayload);
begin
  Self.RTPClientLock.Acquire;
  try
    (Self.RTPClients[0] as TIdRTPServer).Session.SendData(Payload);
  finally
    Self.RTPClientLock.Release;
  end;
end;

function TIdSdpPayloadProcessor.SessionCount: Integer;
begin
  Self.RTPServerLock.Acquire;
  try
    Result := Self.RTPServers.Count;
  finally
    Self.RTPServerLock.Release;
  end;
end;

procedure TIdSdpPayloadProcessor.StartListening(const LocalSessionDescription: String);
var
  Description: TIdSdpPayload;
  S:           TStringStream;
begin
  Self.StopListening;

  if (LocalSessionDescription <> '') then begin
    S := TStringStream.Create(LocalSessionDescription);
    try
      Description := TIdSdpPayload.CreateFrom(S);
      try
        Self.TransportType := Description.Origin.AddressType;
        Description.InitializeProfile(Self.Profile);
        Self.SetUpMediaStreams(Description);
      finally
        Description.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TIdSdpPayloadProcessor.StopListening;
begin
  Self.RTPServerLock.Acquire;
  try
    Self.RTPServers.Clear;
  finally
    Self.RTPServerLock.Release;
  end;
end;

//* TIdSdpPayloadProcessor Public methods **************************************

procedure TIdSdpPayloadProcessor.ActivateServerOnNextFreePort(Server: TIdRTPServer;
                                                              StartFrom: Cardinal);
var
  RTCPBinding: TIdSocketHandle;
  RTPBinding:  TIdSocketHandle;
  Bound:       Boolean;
begin
  RTPBinding := Server.Bindings.Add;

  Bound := false;
  while not Bound do begin
    RTPBinding.Port := StartFrom;
    try
      Server.Active := false;
      Server.Active := true;
      Bound := true;

      if Bound then begin
        // Grab RTCP port
        RTCPBinding := Server.Bindings.Add;
        RTCPBinding.Port := StartFrom + 1;
        Server.Active := false;
        Server.Active := true;
      end;
    except
      on EIdCouldNotBindSocket do
        Bound := false;
      else raise;
    end;
    Inc(StartFrom, 2);
  end;
end;

function TIdSdpPayloadProcessor.AddPeer(MediaDesc: TIdSdpMediaDescription;
                                         List: TObjectList): TIdRTPServer;
var
  I:    Cardinal;
begin
  // Precondition: If necessary, a critical section has already protected
  // List.

  Result := TIdRTPServer.Create(nil);
  try
    List.Add(Result);
    Result.Profile := Self.Profile;

    Self.ActivateServerOnNextFreePort(Result,
                                      MediaDesc.Port);

    // Result.Bindings.Count - 2 >= 0 since
    // ActivateServerOnNextFreePort adds 2 bindings.
    for I := 2 to MediaDesc.PortCount do
      Self.ActivateServerOnNextFreePort(Result,
                                        Result.Bindings[Result.Bindings.Count - 2].Port);

    Result.DefaultPort := Result.Bindings[0].Port;
  except
    if (List.IndexOf(Result) <> -1) then
      List.Remove(Result)
    else
      FreeAndNil(Result);

    raise;
  end;
end;

function TIdSdpPayloadProcessor.DefaultBasePort: Integer;
begin
  Result := 8000;
end;

function TIdSdpPayloadProcessor.DefaultHost: String;
begin
  Result := 'localhost';
end;

function TIdSdpPayloadProcessor.DefaultUsername: String;
begin
  Result := 'unknown';
end;

procedure TIdSdpPayloadProcessor.NotifyOfNewRTPData(Data: TIdRTPPayload;
                                                    Binding: TIdSocketHandle);
var
  I: Integer;
begin
  Self.DataListenerLock.Acquire;
  try
    for I := 0 to Self.DataListeners.Count - 1 do
      IIdRTPDataListener(Self.DataListeners[I]).OnNewData(Data, Binding);
  finally
    Self.DataListenerLock.Release;
  end;
end;

procedure TIdSdpPayloadProcessor.OnRTCP(Packet: TIdRTCPPacket;
                                        Binding: TIdSocketHandle);
begin
end;

procedure TIdSdpPayloadProcessor.OnRTP(Packet: TIdRTPPacket;
                                       Binding: TIdSocketHandle);
begin
  Self.NotifyOfNewRTPData(Packet.Payload,
                          Binding);
end;

function TIdSdpPayloadProcessor.PeerAt(Index: Integer): TIdFilteredRTPPeer;
begin
  Result := Self.Filters[Index] as TIdFilteredRTPPeer;
end;

function TIdSdpPayloadProcessor.ServerAt(Index: Integer): TIdRTPServer;
begin
  Result := Self.RTPServers[Index] as TIdRTPServer;
end;

procedure TIdSdpPayloadProcessor.SetRemoteSessionDescription(const Value: String);
var
  I:          Integer;
  NewClient:  TIdRTPServer;
  RemoteDesc: TIdSdpPayload;
  S:          TStringStream;
begin
  Self.RTPClientLock.Acquire;
  try
    Self.fRemoteSessionDescription := Value;

    S := TStringStream.Create(Self.RemoteSessionDescription);
    try
      RemoteDesc := TIdSdpPayload.CreateFrom(S);
      try
        for I := 0 to RemoteDesc.MediaDescriptionCount - 1 do begin
          NewClient := Self.AddPeer(RemoteDesc.MediaDescriptionAt(I),
                                    Self.RTPClients);
          NewClient.Session.AddReceiver(RemoteDesc.MediaDescriptionAt(I).Connections[0].Address,
                                        RemoteDesc.MediaDescriptionAt(I).Port);
        end;
      finally
        RemoteDesc.Free;
      end;
    finally
      S.Free;
    end;
  finally
    Self.RTPClientLock.Release;
  end;
end;

procedure TIdSdpPayloadProcessor.SetUpMediaStreams(RemoteDescription: TIdSdpPayload);
var
  I: Integer;
begin
  for I := 0 to RemoteDescription.MediaDescriptionCount - 1 do
    Self.SetUpSingleStream(RemoteDescription.MediaDescriptionAt(I));
end;

procedure TIdSdpPayloadProcessor.SetUpSingleStream(MediaDesc: TIdSdpMediaDescription);
var
  NewPeer: TIdFilteredRTPPeer;
begin
  // Realise that the MediaDesc contains a Port value. We TRY to allocate that
  // port, but if that port's already bound we just bind to the lowest free
  // port number greater than MediaDesc.Port.

  Self.RTPServerLock.Acquire;
  try
    NewPeer := TIdFilteredRTPPeer.Create(Self.AddPeer(MediaDesc,
                                                      Self.RTPServers),
                                         nil,
                                         nil);
    try
      NewPeer.AddListener(Self);
    except
      if (Self.Filters.IndexOf(NewPeer) <> -1) then
        Self.Filters.Remove(NewPeer)
      else
        FreeAndNil(NewPeer);

      raise;
    end;
  finally
    Self.RTPServerLock.Release;
  end;
end;

end.
