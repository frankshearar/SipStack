unit IdSdpParser;

interface

uses
  Contnrs, IdSNTP, IdAssignedNumbers, IdEmailAddress, IdSimpleParser,
  IdURI;

type
  TIdNtpTimestamp = Int64;
  TIdSdpBandwidthType = (btConferenceTotal, btApplicationSpecific, btRS, btRR);
  TIdSdpKeyType = (ktClear, ktBase64, ktURI, ktPrompt);
  TIdSdpMediaType = (mtAudio, mtVideo, mtApplication, mtData, mtControl);

  TIdSdpAttribute = class(TObject)
  private
    fName:  String;
    fValue: String;
  public
    property Name:  String read fName write fName;
    property Value: String read fValue write fValue;
  end;

  TIdSdpBandwidth = class(TObject)
  private
    fBandwidth:     Cardinal;
    fBandwidthType: TIdSdpBandwidthType;
  public
    property Bandwidth:     Cardinal            read fBandwidth write fBandwidth;
    property BandwidthType: TIdSdpBandwidthType read fBandwidthType write fBandwidthType;
  end;

  TIdSdpConnection = class(TObject)
  private
    fAddress:     String;
    fAddressType: TIdIPVersion;
    fNetType:     String;
    fTTL:         Byte;
  public
    property AddressType: TIdIPVersion read fAddressType write fAddressType;
    property Address:     String       read fAddress write fAddress;
    property NetType:     String       read fNetType write fNetType;
    property TTL:         Byte         read fTTL write fTTL;
  end;

  TIdSdpKey = class(TObject)
  private
    fKeyType: TIdSdpKeyType;
    fValue:   String;
  public
    property KeyType: TIdSdpKeyType read fKeyType write fKeyType;
    property Value:   String        read fValue write fValue;
  end;

  TIdSdpAttributes = class;
  TIdSdpBandwidths = class;
  TIdSdpMediaDescription = class(TObject)
  private
    fAttributes: TIdSdpAttributes;
    fBandwidths: TIdSdpBandwidths;
    fConnection: TIdSdpConnection;
    fInfo:       String;
    fKey:        TIdSdpKey;
    fMediaType:  TIdSdpMediaType;
    fPort:       Cardinal;
    fPortCount:  Cardinal;
    fTransport:  String;
    List:        array of String;

    function GetAttributes: TIdSdpAttributes;
    function GetBandwidths: TIdSdpBandwidths;
    function GetConnection: TIdSdpConnection;
    function GetFormats(Index: Integer): String;
    function GetKey: TIdSdpKey;
  public
    destructor  Destroy; override;

    procedure AddFormat(const Fmt: String);
    function  FormatCount: Integer;
    procedure ClearFormats;
    function  HasConnection: Boolean;

    property Attributes:              TIdSdpAttributes read GetAttributes;
    property Bandwidths:              TIdSdpBandwidths read GetBandwidths;
    property Connection:              TIdSdpConnection read GetConnection;
    property Formats[Index: Integer]: String           read GetFormats;
    property Info:                    String           read fInfo write fInfo;
    property Key:                     TIdSdpKey        read GetKey;
    property MediaType:               TIdSdpMediaType  read fMediaType write fMediaType;
    property Port:                    Cardinal         read fPort write fPort;
    property PortCount:               Cardinal         read fPortCount write fPortCount;
    property Transport:               String           read fTransport write fTransport;
  end;

  TIdSdpOrigin = class(TObject)
  private
    fAddress:        String;
    fAddressType:    TIdIPVersion;
    fNetType:        String;
    fSessionID:      String;
    fSessionVersion: String;
    fUsername:       String;
  public
    property Address:        String       read fAddress write fAddress;
    property AddressType:    TIdIPVersion read fAddressType write fAddressType;
    property NetType:        String       read fNetType write fNetType;
    property SessionID:      String       read fSessionID write fSessionID;
    property SessionVersion: String       read fSessionVersion write fSessionVersion;
    property Username:       String       read fUsername write fUsername;
  end;

  TIdSdpRepeat = class(TObject)
  private
    fValue: String;
  public
    property Value: String read fValue write fValue;
  end;

  TIdSdpZoneAdjustment = class(TObject)
  private
    fValue: String;
  public
    property Value: String read fValue write fValue;
  end;

  TIdSdpRepeats = class;
  TIdSdpZoneAdjustments = class;
  TIdSdpTime = class(TObject)
  private
    fEndTime:         TIdNtpTimestamp;
    fStartTime:       TIdNtpTimestamp;
    fRepeats:         TIdSdpRepeats;
    fZoneAdjustments: TIdSdpZoneAdjustments;

    function GetRepeats: TIdSdpRepeats;
    function GetZoneAdjustments: TIdSdpZoneAdjustments;
  public
    destructor Destroy; override;

    property EndTime:         TIdNtpTimestamp       read fEndTime write fEndTime;
    property Repeats:         TIdSdpRepeats         read GetRepeats;
    property StartTime:       TIdNtpTimestamp       read fStartTime write fStartTime;
    property ZoneAdjustments: TIdSdpZoneAdjustments read GetZoneAdjustments;
  end;

  TIdSdpList = class(TObject)
  protected
    List: TObjectList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    function  Count: Integer;
    function  Contains(const O: TObject): Boolean;
  end;

  TIdSdpAttributes = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpAttribute;
  public
    procedure Add(Att: TIdSdpAttribute);

    property Items[Index: Integer]: TIdSdpAttribute read GetItems; default;
  end;

  TIdSdpBandwidths = class(TIdSdpList)
  private
    function GetItems(Index: Integer): TIdSdpBandwidth;
  public
    procedure Add(BW: TIdSdpBandwidth);

    property Items[Index: Integer]: TIdSdpBandwidth read GetItems; default;
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
    fConnection:        TIdSdpConnection;
    fEmailAddress:      TIdEmailAddressItem;
    fInfo:              String;
    fKey:               TIdSdpKey;
    fMediaDescriptions: TIdSdpMediaDescriptions;
    fOrigin:            TIdSdpOrigin;
    fPhoneNumber:       String;
    fSessionName:       String;
    fTimes:             TIdSdpTimes;
    fURI:               TIdURI;
    fVersion:           Cardinal;

    function GetAttributes: TIdSdpAttributes;
    function GetBandwidths: TIdSdpBandwidths;
    function GetConnection: TIdSdpConnection;
    function GetEmailAddress: TIdEmailAddressItem;
    function GetKey: TIdSdpKey;
    function GetMediaDescriptions: TIdSdpMediaDescriptions;
    function GetOrigin: TIdSdpOrigin;
    function GetTimes: TIdSdpTimes;
    function GetURI: TIdURI;
  public
    destructor Destroy; override;

    function AllDescriptionsHaveConnections: Boolean;
    function HasConnection: Boolean;

    property Attributes:        TIdSdpAttributes        read GetAttributes;
    property Bandwidths:        TIdSdpBandwidths        read GetBandwidths;
    property Connection:        TIdSdpConnection        read GetConnection;
    property EmailAddress:      TIdEMailAddressItem     read GetEmailAddress;
    property Info:              String                  read fInfo write fInfo;
    property Key:               TIdSdpKey               read GetKey;
    property MediaDescriptions: TIdSdpMediaDescriptions read GetMediaDescriptions;
    property Origin:            TIdSdpOrigin            read GetOrigin;
    property PhoneNumber:       String                  read fPhoneNumber write fPhoneNumber;
    property SessionName:       String                  read fSessionName write fSessionName;
    property Times:             TIdSdpTimes             read GetTimes;
    property URI:               TIdURI                  read GetURI;
    property Version:           Cardinal                read fVersion write fVersion;
  end;

  TIdSdpParser = class(TIdSimpleParser)
  private
    LastMediaHeader:       Char;
    LastSessionHeader:     Char;
    ParsingSessionHeaders: Boolean;

    procedure AssertHeaderOrder;
    function  GetAndCheckInfo: String;
    procedure ParseAttribute(const Attributes: TIdSdpAttributes);
    procedure ParseBandwidth(const Bandwidths: TIdSdpBandwidths);
    procedure ParseConnection(const Connection: TIdSdpConnection);
    procedure ParseEmail(const Payload: TIdSdpPayload);
    procedure ParseHeader(var Name, Value: String);
    procedure ParseInfo(const MediaDescription: TIdSdpMediaDescription); overload;
    procedure ParseInfo(const Payload: TIdSdpPayload); overload;
    procedure ParseKey(const Key: TIdSdpKey);
    procedure ParseMediaDescription(const Payload: TIdSdpPayload);
    procedure ParseMediaOptionalHeaders(const MediaDescription: TIdSdpMediaDescription);
    procedure ParseOrigin(const Payload: TIdSdpPayload);
    procedure ParsePhone(const Payload: TIdSdpPayload);
    procedure ParseRepeat(const Time: TIdSdpTime);
    procedure ParseSessionHeaders(const Payload: TIdSdpPayload);
    procedure ParseSessionOptionalHeaders(const Payload: TIdSdpPayload);
    procedure ParseSessionName(const Payload: TIdSdpPayload);
    procedure ParseTime(const Payload: TIdSdpPayload);
    procedure ParseZoneAdjustment(const Time: TIdSdpTime);
    procedure ParseURI(const Payload: TIdSdpPayload);
    procedure ParseVersion(const Payload: TIdSdpPayload);
  public
    class function IsAddressType(const Token: String): Boolean;
    class function IsBandwidthType(const Token: String): Boolean;
    class function IsByteString(const Token: String): Boolean;
    class function IsKeyData(const Token: String): Boolean;
    class function IsKeyType(const Token: String): Boolean;
    class function IsMediaType(const Token: String): Boolean;
    class function IsMulticastAddress(const IpVersion: TIdIPVersion; const Token: String): Boolean;
    class function IsNetType(const Token: String): Boolean;
    class function IsPhone(const Token: String): Boolean;
    class function IsPhoneNumber(const Header: String): Boolean;
    class function IsPort(const Token: String): Boolean;
    class function IsText(const Token: String): Boolean;
    class function IsTime(const Token: String): Boolean;
    class function IsTransport(const Token: String): Boolean;

    procedure Parse(const Payload: TIdSdpPayload);
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
  SessionHeaderOrder = 'vosiuepcbtka';
  //                            ** * <-- * indicates that this header can occur multiple times
  MediaHeaderOrder   = 'micbka';
  //                      ** * <-- * indicates that this header can occur multiple times
  TimeTypes = ['d', 'h', 'm', 's'];

// for IdAssignedNumbers
const
  // IANA assigned bwtype
  Id_SDP_ConferenceTotal = 'CT';
  Id_SDP_ApplicationSpecific = 'AS';
  Id_SDP_RS = 'RS';
  Id_SDP_RR = 'RR';
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

//******************************************************************************
//* Unit public functions and procedures                                       *
//******************************************************************************

function AddressTypeToStr(const Version: TIdIPVersion): String;
begin
  case Version of
    Id_IPv4: Result := Id_SDP_IP4;
    Id_IPv6: Result := Id_SDP_IP6;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg, ['TIdIPVersion', Ord(Version), 'String']));
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
    raise EConvertError.Create(Format(ConvertEnumErrorMsg, ['TIdSdpBandwidthType', Ord(BwType), 'String']));
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
    raise EConvertError.Create(Format(ConvertEnumErrorMsg, ['TIdSdpKeyType', Ord(KeyType), 'String']));
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
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg, ['TIdSdpMediaType', Ord(MediaType), 'String']));
  end;
end;

function StrToAddressType(const S: String): TIdIPVersion;
begin
       if (S = Id_SDP_IP4) then Result := Id_IPv4
  else if (S = Id_SDP_IP6) then Result := Id_IPv6
  else
    raise EConvertError.Create(Format(ConvertStrErrorMsg, [S, 'TIdIPVersion']));
end;

function StrToBandwidthType(const S: String): TIdSdpBandwidthType;
begin
       if (S = Id_SDP_ConferenceTotal)     then Result := btConferenceTotal
  else if (S = Id_SDP_ApplicationSpecific) then Result := btApplicationSpecific
  else if (S = Id_SDP_RS)                  then Result := btRS
  else if (S = Id_SDP_RR)                  then Result := btRR
  else
    raise EConvertError.Create(Format(ConvertStrErrorMsg, [S, 'TIdSdpBandwidthType']));
end;

function StrToKeyType(const S: String): TIdSDPKeyType;
begin
       if (S = Id_SDP_Clear)  then Result := ktClear
  else if (S = Id_SDP_Base64) then Result := ktBase64
  else if (S = Id_SDP_URI)    then Result := ktURI
  else if (S = Id_SDP_Prompt) then Result := ktPrompt
  else
    raise EConvertError.Create(Format(ConvertStrErrorMsg, [S, 'TIdSdpKeyType']));
end;

function StrToMediaType(const S: String): TIdSDPMediaType;
begin
       if (S = RSSDPMediaTypeAudio)       then Result := mtAudio
  else if (S = RSSDPMediaTypeVideo)       then Result := mtVideo
  else if (S = RSSDPMediaTypeApplication) then Result := mtApplication
  else if (S = RSSDPMediaTypeData)        then Result := mtData
  else if (S = RSSDPMediaTypeControl)     then Result := mtControl
  else
    raise EConvertError.Create(Format(ConvertStrErrorMsg, [S, 'TIdSdpMediaType']));
end;

//******************************************************************************
//* TIdSdpMediaDescription                                                     *
//******************************************************************************
//* TIdSdpMediaDescription Public methods **************************************

destructor TIdSdpMediaDescription.Destroy;
begin
  fAttributes.Free;
  fBandwidths.Free;
  fConnection.Free;
  fKey.Free;

  inherited Destroy;
end;

procedure TIdSdpMediaDescription.AddFormat(const Fmt: String);
var
  InsertPos: Integer;
begin
  InsertPos := Self.FormatCount;
  SetLength(Self.List, InsertPos + 1);
  Self.List[InsertPos] := Fmt;
end;

function TIdSdpMediaDescription.FormatCount: Integer;
begin
  Result := Length(Self.List);
end;

procedure TIdSdpMediaDescription.ClearFormats;
begin
  SetLength(Self.List, 0);
end;

function TIdSdpMediaDescription.HasConnection: Boolean;
begin
  Result := Assigned(fConnection);
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

function TIdSdpMediaDescription.GetConnection: TIdSdpConnection;
begin
  if not Assigned(fConnection) then
    fConnection := TIdSdpConnection.Create;

  Result := fConnection;
end;

function TIdSdpMediaDescription.GetFormats(Index: Integer): String;
begin
  Result := Self.List[Index];
end;

function TIdSdpMediaDescription.GetKey: TIdSdpKey;
begin
  if not Assigned(fKey) then
    fKey := TIdSdpKey.Create;

  Result := fKey;
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

function TIdSdpList.Contains(const O: TObject): Boolean;
begin
  Result := Self.List.IndexOf(O) <> -1;
end;

//******************************************************************************
//* TIdSdpAttributes                                                           *
//******************************************************************************
//* TIdSdpAttributes Public methods ********************************************

procedure TIdSdpAttributes.Add(Att: TIdSdpAttribute);
begin
  Self.List.Add(Att);
end;

//* TIdSdpAttributes Private methods *******************************************

function TIdSdpAttributes.GetItems(Index: Integer): TIdSdpAttribute;
begin
  Result := Self.List[Index] as TIdSdpAttribute;
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

destructor TIdSdpPayload.Destroy;
begin
  fAttributes.Free;
  fBandwidths.Free;
  fConnection.Free;
  fEmailAddress.Free;
  fKey.Free;
  fMediaDescriptions.Free;
  fOrigin.Free;
  fTimes.Free;
  fURI.Free;

  inherited Destroy;
end;

function TIdSdpPayload.AllDescriptionsHaveConnections: Boolean;
begin
  Result := Self.MediaDescriptions.AllDescriptionsHaveConnections;
end;

function TIdSdpPayload.HasConnection: Boolean;
begin
  Result := Assigned(fConnection);
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

function TIdSdpPayload.GetConnection: TIdSdpConnection;
begin
  if not Assigned(fConnection) then
    fConnection := TIdSdpConnection.Create;

  Result := fConnection;
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

function TIdSdpPayload.GetURI: TIdURI;
begin
  if not Assigned(fURI) then
    fURI := TIdURI.Create('');

  Result := fURI;
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

class function TIdSdpParser.IsMulticastAddress(const IpVersion: TIdIPVersion; const Token: String): Boolean;
var
  Address: String;
  N:       String;
begin
  Address := Token;

  case IpVersion of
    Id_IPv4: begin
      Result := Self.IsIPv4Address(Address);
      N := Fetch(Address, '.');
      Result := Result and (StrToInt(N) = 224);
    end;
    Id_IPv6: begin
      Result := Self.IsIPv6Address(Address);
      Result := Result and (Lowercase(Copy(Address, 1, 2)) = 'ff');
    end;
  else
    raise EParser.Create('Unknown TIdIPVersion in IsMulticastAddress');
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

procedure TIdSdpParser.Parse(const Payload: TIdSdpPayload);
begin
  Self.ParseSessionHeaders(Payload);

  while not Self.Eof do
    Self.ParseMediaDescription(Payload);

  if not Payload.HasConnection
     and not ((Payload.MediaDescriptions.Count > 0)
              and Payload.AllDescriptionsHaveConnections) then
    raise EParser.Create(MissingConnection);
end;

//* TIdSdpParser Private methods ***********************************************

procedure TIdSdpParser.AssertHeaderOrder;
var
  CurrentHeader: Char;
  HeaderOrder:   String;
  LastHeader:    Char;
begin
  // Self.PeekChar is the header we have. Call this CurrentHeader.
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
    raise EParser.Create(Format(BadHeaderOrder, [CurrentHeader, LastHeader]));
end;

function TIdSdpParser.GetAndCheckInfo: String;
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

  if (Name <> RSSDPInformationName) then
    raise EParser.Create(BadHeaderOrder);

  if not Self.IsText(Value) then
    raise EParser.Create(Format(MalformedToken, [RSSDPInformationName, Value]));
  Result := Value;

  if Self.ParsingSessionHeaders then
    Self.LastSessionHeader := RSSDPInformationName
  else
    Self.LastMediaHeader := RSSDPInformationName;
end;

procedure TIdSdpParser.ParseAttribute(const Attributes: TIdSdpAttributes);
var
  Att:           TIdSdpAttribute;
  OriginalValue: String;
  Name:          String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  Att := TIdSdpAttribute.Create;
  try
    if (IndyPos(':', Value) > 0) then begin
      Att.Name := Fetch(Value, ':');
      Att.Value := Value;
    end
    else begin
      Att.Name := Value;
      Att.Value := '';
    end;

    if not Self.IsAlphaNumeric(Att.Name) then
      raise EParser.Create(Format(MalformedToken, [RSSDPAttributeName, OriginalValue]));

    if (Att.Value <> '') and not Self.IsByteString(Att.Value) then
      raise EParser.Create(Format(MalformedToken, [RSSDPAttributeName, OriginalValue]));

    Attributes.Add(Att);
  except
    if not Attributes.Contains(Att) then
      Att.Free;

    raise;
  end;
end;

procedure TIdSdpParser.ParseBandwidth(const Bandwidths: TIdSdpBandwidths);
var
  BW:            TIdSdpBandwidth;
  Name:          String;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

  BW := TIdSdpBandwidth.Create;
  try
    Token := Fetch(Value, ':');

    if not Self.IsBandwidthType(Token) then
      raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));

    BW.BandwidthType := StrToBandwidthType(Token);

    if not Self.IsNumber(Value) then
      raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));
    BW.Bandwidth := StrToInt(Value);

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

procedure TIdSdpParser.ParseConnection(const Connection: TIdSdpConnection);
var
  Name:          String;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  Token := Fetch(Value, ' ');
  if not Self.IsNetType(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));
  Connection.NetType := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsAddressType(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));

  Connection.AddressType := StrToAddressType(Token);

  // need to check that unicast addies never have '/'
  if (IndyPos('/', Value) > 0) then begin
    // multicast
    Token := Fetch(Value, '/');
    if not Self.IsMulticastAddress(Connection.AddressType, Token) and not Self.IsFQDN(Token) then
      raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));
    Connection.Address := Token;

    if not Self.IsByte(Value) then
      raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));
    Connection.TTL     := StrToInt(Value);
  end
  else begin
    // unicast
    if not Self.IsIPAddress(Connection.AddressType, Value) and not Self.IsFQDN(Token) then
      raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));

    Connection.Address := Value;
  end;

  if Self.ParsingSessionHeaders then
    Self.LastSessionHeader := RSSDPConnectionName
  else
    Self.LastMediaHeader := RSSDPConnectionName
end;

procedure TIdSdpParser.ParseEmail(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

//  if not Self.IsEmailAddress(Value) then
//    raise EParser.Create(Format(MalformedToken, [RSSDPEmailName, Value]));

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
    raise EParser.Create(Format(MalformedToken, ['Header', Line]));

  if (Value = '') then
    raise EParser.Create(Format(MalformedToken, [Name, Line]));

  if (Name <> Trim(Name)) or (Value <> Trim(Value)) then
    raise EParser.Create(Format(MalformedToken, [Trim(Name), Line]));
end;

procedure TIdSdpParser.ParseInfo(const MediaDescription: TIdSdpMediaDescription);
begin
  MediaDescription.Info := Self.GetAndCheckInfo;
end;

procedure TIdSdpParser.ParseInfo(const Payload: TIdSdpPayload);
begin
  Payload.Info := Self.GetAndCheckInfo;
end;

procedure TIdSdpParser.ParseKey(const Key: TIdSdpKey);
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
    raise EParser.Create(Format(MalformedToken, [RSSDPKeyName, OriginalValue]));

  Key.KeyType := StrToKeyType(Token);

  if (Key.KeyType = ktPrompt) then begin
    if (Value <> '') then
      raise EParser.Create(Format(MalformedToken, [RSSDPKeyName, OriginalValue]))
  end
  else begin
    if Self.IsKeyData(Value) then
      Key.Value := Value
    else
      raise EParser.Create(Format(MalformedToken, [RSSDPKeyName, OriginalValue]));
  end;

  if Self.ParsingSessionHeaders then
    Self.LastSessionHeader := RSSDPKeyName
  else
    Self.LastMediaHeader := RSSDPKeyName;
end;

procedure TIdSdpParser.ParseMediaDescription(const Payload: TIdSdpPayload);
var
  Count:         String;
  Name:          String;
  NewMediaDesc:  TIdSdpMediaDescription;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  NewMediaDesc := TIdSdpMediaDescription.Create;
  try
    Token := Fetch(Value, ' ');
    if not Self.IsMediaType(Token) then
      raise EParser.Create(Format(MalformedToken, [RSSDPMediaDescriptionName, OriginalValue]));
    NewMediaDesc.MediaType := StrToMediaType(Token);

    Token := Fetch(Value, ' ');
    if (IndyPos('/', Token) > 0) then begin
      Count := Token;
      Token := Fetch(Count, '/');
    end;

    if not Self.IsPort(Token) then
      raise EParser.Create(Format(MalformedToken, [RSSDPMediaDescriptionName, OriginalValue]));
    NewMediaDesc.Port := StrToInt(Token);

    if (Count <> '') and not Self.IsNumber(Count) then
      raise EParser.Create(Format(MalformedToken, [RSSDPMediaDescriptionName, OriginalValue]));
    NewMediaDesc.PortCount := StrToIntDef(Count, 0);

    Token := Fetch(Value, ' ');
    if not Self.IsTransport(Token) then
      raise EParser.Create(Format(MalformedToken, [RSSDPMediaDescriptionName, OriginalValue]));
    NewMediaDesc.Transport := Token;

    while (Value <> '') do begin
      Token := Fetch(Value, ' ');
      if not Self.IsAlphaNumeric(Token) then
        raise EParser.Create(Format(MalformedToken, [RSSDPMediaDescriptionName, OriginalValue]));
      NewMediaDesc.AddFormat(Token);
    end;

    if (NewMediaDesc.FormatCount = 0) then
      raise EParser.Create(Format(MalformedToken, [RSSDPMediaDescriptionName, OriginalValue]));

    Self.ParseInfo(NewMediaDesc);

    Self.ParseMediaOptionalHeaders(NewMediaDesc);

    Payload.MediaDescriptions.Add(NewMediaDesc);
  except
    if not Payload.MediaDescriptions.Contains(NewMediaDesc) then
      NewMediaDesc.Free;

    raise;
  end;

  Self.LastSessionHeader := RSSDPMediaDescriptionName;
  LastMediaHeader := RSSDPMediaDescriptionName;
end;

procedure TIdSdpParser.ParseMediaOptionalHeaders(const MediaDescription: TIdSdpMediaDescription);
var
  NextHeader: String;
begin
  NextHeader := Self.PeekLine;
  while not Self.Eof and (NextHeader <> '') and (NextHeader[1] <> RSSDPMediaDescriptionName) do begin
    case NextHeader[1] of
      RSSDPConnectionName: Self.ParseConnection(MediaDescription.Connection);
      RSSDPBandwidthName:  Self.ParseBandwidth(MediaDescription.Bandwidths);
      RSSDPKeyName:        Self.ParseKey(MediaDescription.Key);
      RSSDPAttributeName:  Self.ParseAttribute(MediaDescription.Attributes);
    else
      raise EParser.Create(Format(UnknownOptionalHeader, [NextHeader]));
    end;

    NextHeader := Self.PeekLine;
  end;
end;

procedure TIdSdpParser.ParseOrigin(const Payload: TIdSdpPayload);
var
  Name:          String;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  if (Name <> RSSDPOriginName) then
    raise EParser.Create(MissingOrigin);

  Payload.Origin.Username := Fetch(Value, ' ');
  if (Payload.Origin.Username = '') then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));

  Token := Fetch(Value, ' ');
  if not Self.IsNumber(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));
  Payload.Origin.SessionID := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsNumber(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));
  Payload.Origin.SessionVersion := Token;    

  Token := Fetch(Value, ' ');
  if not Self.IsNetType(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));
  Payload.Origin.NetType := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsAddressType(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));

  Payload.Origin.AddressType := StrToAddressType(Token);

  Payload.Origin.Address := Value;
  if (Payload.Origin.Address = '') then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));

  Self.LastSessionHeader := RSSDPOriginName;
end;

procedure TIdSdpParser.ParsePhone(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

  if not Self.IsPhoneNumber(Value) then
    raise EParser.Create(Format(MalformedToken, [RSSDPPhoneName, Value]));

  if (Payload.PhoneNumber <> '') then
    raise EParser.Create(Format(TooManyHeaders, [RSSDPPhoneName]));

  Payload.PhoneNumber := Value;
  Self.LastSessionHeader := RSSDPPhoneName;
end;

procedure TIdSdpParser.ParseRepeat(const Time: TIdSdpTime);
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
    raise EParser.Create(BadHeaderOrder);

  Rpt := TIdSdpRepeat.Create;
  try
    Rpt.Value := Value;

    while (Value <> '') do begin
      Token := Fetch(Value, ' ');

      if not Self.IsTime(Token) then
        raise EParser.Create(Format(MalformedToken, [RSSDPRepeatName, OriginalValue]));
    end;

    Time.Repeats.Add(Rpt);
  except
    if not Time.Repeats.Contains(Rpt) then
      Rpt.Free;

    raise;
  end;
end;

procedure TIdSdpParser.ParseSessionHeaders(const Payload: TIdSdpPayload);
begin
  Self.ParsingSessionHeaders := true;

  Self.ParseVersion(Payload);
  Self.ParseOrigin(Payload);
  Self.ParseSessionName(Payload);
  Self.ParseSessionOptionalHeaders(Payload);

  Self.ParsingSessionHeaders := false;
end;

procedure TIdSdpParser.ParseSessionOptionalHeaders(const Payload: TIdSdpPayload);
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
      RSSDPConnectionName:  Self.ParseConnection(Payload.Connection);
      RSSDPEmailName:       Self.ParseEmail(Payload);
      RSSDPKeyName:         Self.ParseKey(Payload.Key);
      RSSDPInformationName: Self.ParseInfo(Payload);
      RSSDPPhoneName:       Self.ParsePhone(Payload);
      RSSDPTimeName:        Self.ParseTime(Payload);
      RSSDPUriName:         Self.ParseUri(Payload);
    else
      raise EParser.Create(Format(UnknownOptionalHeader, [NextHeader]));
    end;

    NextHeader := Self.PeekLine;
  end;
end;

procedure TIdSdpParser.ParseSessionName(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.ParseHeader(Name, Value);

  if (Name <> RSSDPSessionName) then
    raise EParser.Create(MissingSessionName);

  if not Self.IsText(Value) then
    raise EParser.Create(Format(MalformedToken, [RSSDPSessionName, Value]));

  Payload.SessionName := Value;
  Self.LastSessionHeader := RSSDPSessionName;
end;

procedure TIdSdpParser.ParseTime(const Payload: TIdSdpPayload);
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
      raise EParser.Create(Format(MalformedToken, [RSSDPTimeName, OriginalValue]));
    Time.StartTime := StrToInt64(Token);

    Token := Fetch(Value, ' ');
    if not Self.IsNumber(Token) then
      raise EParser.Create(Format(MalformedToken, [RSSDPTimeName, OriginalValue]));
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

procedure TIdSdpParser.ParseZoneAdjustment(const Time: TIdSdpTime);
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
//        raise EParser.Create(Format(MalformedToken, [RSSDPRepeatName, OriginalValue]));
    end;

    Time.ZoneAdjustments.Add(Zone);
  except
    if not Time.ZoneAdjustments.Contains(Zone) then
      Zone.Free;

    raise;
  end;
end;

procedure TIdSdpParser.ParseURI(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

//  if not Self.IsUri(Value) then
//    raise EParser.Create(Format(MalformedToken, [RSSDPUriName, Value]));

  Payload.URI.URI := Value;
  Self.LastSessionHeader := RSSDPUriName;
end;

procedure TIdSdpParser.ParseVersion(const Payload: TIdSdpPayload);
var
  E:     Integer;
  N:     Cardinal;
  Name:  String;
  Value: String;
begin
  if Self.Eof then
    raise EParser.Create(EmptyInputStream);

  Self.ParseHeader(Name, Value);

  if (Name <> RSSDPVersionName) then
    raise EParser.Create(MissingVersion);

  Val(Value, N, E);

  if (E <> 0) then
    raise EParser.Create(Format(MalformedToken, [RSSDPVersionName, Value]));

  Payload.Version := N;
  Self.LastSessionHeader := RSSDPVersionName;
end;

end.
