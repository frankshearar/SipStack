{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipMessage;

interface

uses
  Classes, Contnrs, IdDateTimeStamp, IdInterfacedObject,
  IdSimpleParser, IdSipConsts, SyncObjs, SysUtils;

type
  TIdSipQValue = 0..1000;
  TIdSipChars = set of Char;

type
  TIdSipRequest = class;
  TIdSipResponse = class;

  TIdSipConnectionBindings = record
    LocalIP:   String;
    LocalPort: Integer;
    PeerIP:    String;
    PeerPort:  Integer;
  end;

  IIdSipMessageListener = interface
    ['{941E4681-89F9-4491-825C-F6458F7E663C}']
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
  end;

  TIdSipNotifyEvent = TNotifyEvent;
  TIdSipRequestEvent = procedure(Sender: TObject;
                                 R: TIdSipRequest) of object;
  TIdSipResponseEvent = procedure(Sender: TObject;
                                  R: TIdSipResponse;
                                  ReceivedFrom: TIdSipConnectionBindings) of object;

  IIdSipMessageVisitor = interface
    ['{E2900B55-A1CA-47F1-9DB0-D72D6A846EA0}']
    procedure VisitRequest(Request: TIdSipRequest);
    procedure VisitResponse(Response: TIdSipResponse);
  end;

  TIdSipHostAndPort = class(TObject)
  private
    fDefaultPort:     Cardinal;
    fHost:            String;
    fPort:            Cardinal;
    fPortIsSpecified: Boolean;

    function  GetValue: String;
    procedure SetPort(const Value: Cardinal);
    procedure SetValue(Value: String);
  public
    class function CouldContainIPv6Reference(const Token: String): Boolean;

    property DefaultPort:     Cardinal read fDefaultPort write fDefaultPort;
    property Host:            String   read fHost write fHost;
    property Port:            Cardinal read fPort write SetPort;
    property PortIsSpecified: Boolean  read fPortIsSpecified write fPortIsSpecified;
    property Value:           String   read GetValue write SetValue;
  end;

  // I represent some sort've URI. My subclasses implement URIs like SIP or SIPS
  //  or TEL URIs. My subclasses do all parsing. My constructor is a Template
  // Method - my subclasses need only override Initialize to instantiate
  // any private variables they define, and Parse to actually parse a string.
  TIdUri = class(TObject)
  private
    fScheme:     String;
    HostAndPort: TIdSipHostAndPort;

    function  GetHost: String;
    function  GetPort: Cardinal;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Cardinal);
  protected
    procedure Initialize; virtual;
    procedure Parse(Uri: String); virtual;
    procedure SetScheme(const Value: String); virtual;
  public
    constructor Create(URI: String = ''); virtual;
    destructor  Destroy; override;

    function AsString: String; virtual;
    function IsSipUri: Boolean; virtual;

    property Host:   String   read GetHost write SetHost;
    property Port:   Cardinal read GetPort write SetPort;
    property Scheme: String   read fScheme write SetScheme;
  end;

  TIdSipHeader = class;
  TIdSipHeaders = class;
  TIdSipRouteHeader = class;

  // I represent URIs defined in RFC 3261, namely SIP and SIPS URIs. The sole
  // difference between SIP and SIPS URIs, at least as far as their structure is
  // concerned, is that SIP URIs use the 'sip' scheme and SIPS URIs the 'sips'
  // schemes. 
  TIdSipUri = class(TIdUri)
  private
    fHeaders:   TIdSipHeaders;
    fPassword:  String;
    fUsername:  String;
    Parameters: TStrings;

    class function IsEscapedOrInSet(const Token: String;
                                    AcceptableChars: TIdSipChars): Boolean;

    function  EqualParameters(const Uri: TIdSipUri): Boolean;
    function  GetMaddr: String;
    function  GetMethod: String;
    function  GetTransport: String;
    function  GetTTL: Cardinal;
    function  GetUri: String;
    function  GetUserParameter: String;
    function  HasValidHostInfo: Boolean;
    function  HasValidParameters: Boolean;
    function  HasValidScheme: Boolean;
    function  HasValidUserInfo: Boolean;
    function  HeadersAsString: String;
    function  IsKnownParameter(const Name: String): Boolean;
    function  ParamsAsString: String;
    procedure ParseHeaders(HeaderList: String);
    procedure ParseHost(HostAndPort: String);
    procedure ParseParams(ParamList: String);
    procedure ParseUserInfo(UserInfo: String);
    procedure Reset;
    procedure SetMaddr(const Value: String);
    procedure SetMethod(const Value: String);
    procedure SetTransport(const Value: String);
    procedure SetTTL(const Value: Cardinal);
    procedure SetUri(const Value: String);
    procedure SetUserParameter(const Value: String);
    function  UnidirectionalParameterCompare(ThisUri, ThatUri: TIdSipUri): Boolean;
  protected
    procedure Initialize; override;
    procedure Parse(Uri: String); override;
    procedure SetScheme(const Value: String); override;
  public
    class function CreateUri(URI: String = ''): TIdUri;
    class function Decode(const Src: String): String;
    class function Encode(const Src: String;
                          const SafeChars: TIdSipChars): String;
    class function HeaderEncode(const NameOrValue: String): String;
    class function IsParamNameOrValue(const Token: String): Boolean;
    class function IsPassword(const Token: String): Boolean;
    class function IsUser(const Token: String): Boolean;
    class function ParameterEncode(const Parameter: String): String; //; override;
    class function UsernameEncode(const Username: String): String;

    destructor Destroy; override;

    procedure AddParameter(const Name: String;
                           const Value: String = '');
    function  AsRouteHeader: TIdSipRouteHeader;
    function  AsString: String; override;
    function  CanonicaliseAsAddressOfRecord: String;
    procedure ClearHeaders;
    procedure ClearParameters;
    function  CreateRequest: TIdSipRequest;
    function  DefaultPort: Cardinal; virtual;
    function  DefaultTransport: String; virtual;
    function  Equals(Uri: TIdSipUri): Boolean;
    procedure EraseUserInfo;
    function  HasValidSyntax: Boolean;
    function  HasHeaders: Boolean;
    function  HasMaddr: Boolean;
    function  HasParameter(const Name: String): Boolean;
    function  IsLooseRoutable: Boolean;
    function  IsSecure: Boolean; virtual;
    function  ParamCount: Integer;
    function  ParamName(Index: Cardinal): String;
    function  ParamValue(Index: Cardinal): String; overload;
    function  ParamValue(const Name: String): String; overload;
    function  PortIsSpecified: Boolean;
    procedure RemoveParameter(const Name: String);
    function  TransportIsSpecified: Boolean;
    function  UserIsIp: Boolean;
    function  UserIsPhoneNumber: Boolean;

    property Headers:       TIdSipHeaders read fHeaders;
    property Maddr:         String        read GetMaddr write SetMaddr;
    property Method:        String        read GetMethod write SetMethod;
    property Password:      String        read fPassword write fPassword;
    property Transport:     String        read GetTransport write SetTransport;
    property TTL:           Cardinal      read GetTTL write SetTTL;
    property Uri:           String        read GetUri write SetUri;
    property Username:      String        read fUsername write fUsername;
    property UserParameter: String        read GetUserParameter write SetUserParameter;
  end;

  // I represent a header in a SIP message.
  // Say I hold Contact information:
  //   '"Count Zero" <sip:countzero@jacksbar.com;paranoid>;very'.
  // The Value property will return everything except the parameters, i.e.,
  //   '"Count Zero" <sip:countzero@jacksbar.com;paranoid>'
  // ParamsAsString returns just my parameters, that is,
  //   ';very'.
  // FullValue will return absolutely everything, that is,
  //   '"Count Zero" <sip:countzero@jacksbar.com;paranoid>;very'
  // Lastly, AsString returns what will go down the wire, so to speak:
  //   'Contact: "Count Zero" <sip:countzero@jacksbar.com;paranoid>;very'
  TIdSipHeader = class(TPersistent)
  private
    fIsMalformed:     Boolean;
    fName:            String;
    fParams:          TStrings;
    fParseFailReason: String;
    fValue:           String;
    fUnparsedValue:   String;

    function  GetParam(const Name: String): String;
    function  GetParameters: TStrings;
    procedure SetParam(const Name, Value: String);
    procedure SetParameters(Value: TStrings);

    property Parameters: TStrings read GetParameters write SetParameters;
  protected
    procedure FailParse(const Reason: String);
    function  GetName: String; virtual;
    function  GetValue: String; virtual;
    procedure MarkAsInvalid(const Reason: String);
    procedure Parse(const Value: String); virtual;
    procedure ParseParameters(Value: String;
                              Parameters: TStrings;
                              Delimiter: String = ';');
    procedure SetName(const Value: String); virtual;
    procedure SetValue(const Value: String);
  public
    class function EncodeQuotedStr(const S: String): String;

    constructor Create; virtual;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    function  FullValue: String;
    function  HasParam(Name: String): Boolean;
    function  IndexOfParam(Name: String): Integer;
    function  IsContact: Boolean; virtual;
    function  Equals(Header: TIdSipHeader): Boolean; virtual;
    function  ParamCount: Integer;
    function  ParamsAsString: String; virtual;
    procedure RemoveParameter(const ParamName: String);

    property IsMalformed:                Boolean read fIsMalformed;
    property Name:                       String  read GetName write SetName;
    property Value:                      String  read GetValue write SetValue;
    property Params[const Name: String]: String  read GetParam write SetParam;
    property ParseFailReason:            String  read fParseFailReason;
    property UnparsedValue:              String  read fUnparsedValue;
  end;

  TIdSipHeaderClass = class of TIdSipHeader;

  TIdSipUriHeader = class(TIdSipHeader)
  private
    fAddress: TIdSipUri;

    procedure SetAddress(Value: TIdSipUri);
  protected
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Address: TIdSipUri read fAddress write SetAddress;
  end;

  TIdSipToHeader = class;

  TIdSipAddressHeader = class(TIdSipUriHeader)
  private
    fDisplayName: String;
  protected
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    function AsAddressOfRecord: String;
    function AsToHeader: TIdSipToHeader;
    function HasSipsUri: Boolean;

    property DisplayName: String read fDisplayName write fDisplayName;
  end;

  TIdSipHttpAuthHeader = class(TIdSipHeader)
  private
    fAuthorizationScheme: String;

    function  GetAlgorithm: String;
    function  GetNonce: String;
    function  GetOpaque: String;
    function  GetQop: String;
    function  GetRealm: String;
    function  GetUnknownResponses(const Name: String): String;
    procedure ParseDigestResponses(Value: String);
    procedure SetAlgorithm(const Value: String);
    procedure SetNonce(const Value: String);
    procedure SetOpaque(const Value: String);
    procedure SetQop(const Value: String);
    procedure SetRealm(const Value: String);
    procedure SetUnknownResponses(const Name: String;
                                  const Value: String);
  protected
    DigestResponses:   TStringList;
    fUnknownResponses: TStringList;

    procedure CheckDigestResponses(Responses: TStrings); virtual;
    function  DigestResponseValue(const Name: String): String;
    function  GetValue: String; override;
    function  KnownResponse(const Name: String): Boolean; virtual;
    procedure Parse(const Value: String); override;
    function  QuoteIfNecessary(const ParamName, ParamValue: String): String; virtual;
  public
    class function IsNonce(const Token: String): Boolean;

    constructor Create; override;
    destructor  Destroy; override;

    property Algorithm:           String   read GetAlgorithm write SetAlgorithm;
    property AuthorizationScheme: String   read fAuthorizationScheme write fAuthorizationScheme;
    property Nonce:               String   read GetNonce write SetNonce;
    property Opaque:              String   read GetOpaque write SetOpaque;
    property Qop:                 String   read GetQop write SetQop;
    property Realm:               String   read GetRealm write SetRealm;
    property UnknownResponses[const Name: String]: String read GetUnknownResponses write SetUnknownResponses;
  end;

  TIdSipAuthNonceHeader = class(TIdSipHttpAuthHeader)
  private
    function  GetCNonce: String;
    function  GetNonceCount: Cardinal;
    procedure SetCNonce(const Value: String);
    procedure SetNonceCount(Value: Cardinal);
  public
    function NC: String;

    property CNonce:     String   read GetCNonce write SetCNonce;
    property NonceCount: Cardinal read GetNonceCount write SetNonceCount;
  end;

  // I represent the credentials a client offers to a server to authenticate
  // itself.
  TIdSipAuthorizationHeader = class(TIdSipAuthNonceHeader)
    function  GetDigestUri: String;
    function  GetResponse: String; // The digest with which we authenticate
    function  GetUsername: String;
    procedure SetDigestUri(const Value: String);
    procedure SetResponse(const Value: String);
    procedure SetUsername(const Value: String);
  protected
    procedure CheckDigestResponses(Responses: TStrings); override;
    function  GetName: String; override;
    function  GetValue: String; override;
    function  KnownResponse(const Name: String): Boolean; override;
    function  QuoteIfNecessary(const ParamName, ParamValue: String): String; override;
  public
    function IsBasic: Boolean;
    function IsDigest: Boolean;

    property Response:   String   read GetResponse write SetResponse;
    property DigestUri:  String   read GetDigestUri write SetDigestUri; // This should be a TIdURI
    property Username:   String   read GetUsername write SetUsername;
  end;

  TIdSipAuthorizationHeaderClass = class of TIdSipAuthorizationHeader;

  TIdSipCallIdHeader = class(TIdSipHeader)
  protected
    function  GetName: String; override;
    procedure Parse(const Value: String); override;
  public
    function Equals(Header: TIdSipHeader): Boolean; override;
  end;

  TIdSipCommaSeparatedHeader = class(TIdSipHeader)
  private
    fValues: TStrings;
  protected
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RemoveValues(Header: TIdSipCommaSeparatedHeader);

    property Values: TStrings read fValues;
  end;

  TIdSipWeightedValue = class(TObject)
  private
    fParameters: TStrings;
    fValue:      String;
    fWeight:     TIdSipQValue;

    function GetParameters: TStrings;
  public
    destructor Destroy; override;

    function AsString: String;

    property Parameters: TStrings     read GetParameters;
    property Value:      String       read fValue write fValue;
    property Weight:     TIdSipQValue read fWeight write fWeight;
  end;

  TIdSipWeightedCommaSeparatedHeader = class(TIdSipHeader)
  private
    fValues: TObjectList;

    function  GetValues(Index: Integer): TIdSipWeightedValue;
    procedure SetValues(Index: Integer;
                        Value: TIdSipWeightedValue);
  protected
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddValue(const Value: String;
                       Weight: TIdSipQValue = High(TIdSipQValue));
    procedure ClearValues;
    function  ValueCount: Integer;

    property Values[Index: Integer]: TIdSipWeightedValue read GetValues write SetValues;
  end;

  TIdSipContactHeader = class(TIdSipAddressHeader)
  private
    fIsWildCard: Boolean;

    function  GetExpires: Cardinal;
    function  GetQ: TIdSipQValue;
    procedure SetExpires(Value: Cardinal);
    procedure SetQ(Value: TIdSipQValue);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    procedure RemoveExpires;
    function  WillExpire: Boolean;

    property Expires:    Cardinal     read GetExpires write SetExpires;
    property IsWildCard: Boolean      read fIsWildCard write fIsWildCard;
    property Q:          TIdSipQValue read GetQ write SetQ;
  end;

  TIdSipContentDispositionHeader = class(TIdSipHeader)
  private
    function  GetHandling: String;
    procedure SetHandling(const Value: String);
  protected
    function GetName: String; override;
  public
    function IsSession: Boolean;

    property Handling: String read GetHandling write SetHandling;
  end;

  TIdSipCSeqHeader = class(TIdSipHeader)
  private
    fMethod:     String;
    fSequenceNo: Cardinal;
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    procedure Increment;

    property Method:     String   read fMethod     write fMethod;
    property SequenceNo: Cardinal read fSequenceNo write fSequenceNo;
  end;

  TIdSipDateHeader = class(TIdSipHeader)
  private
    fAbsoluteTime: TIdDateTimeStamp;

    function  GetAbsoluteTime: TIdDateTimeStamp;
    procedure SetAbsoluteTime(Value: String);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    destructor Destroy; override;

    function ZeroDateTime: String;

    property Time: TIdDateTimeStamp read GetAbsoluteTime;
  end;

  TIdSipFromToHeader = class(TIdSipAddressHeader)
  private
    function  GetTag: String;
    procedure SetTag(const Value: String);
  protected
    procedure Parse(const Value: String); override;
  public
    function HasTag: Boolean;
    function Equals(Header: TIdSipHeader): Boolean; override;

    property Tag: String read GetTag write SetTag;
  end;

  TIdSipFromHeader = class(TIdSipFromToHeader)
  protected
    function GetName: String; override;
  end;

  TIdSipNumericHeader = class(TIdSipHeader)
  private
    fNumericValue: Cardinal;
  protected
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    property NumericValue: Cardinal read fNumericValue write fNumericValue;
  end;

  TIdSipMaxForwardsHeader = class(TIdSipNumericHeader)
  protected
    function  GetName: String; override;
    procedure Parse(const Value: String); override;
  end;

  // I represent a challenge a server gives a client before accepting a
  // request.
  TIdSipAuthenticateHeader = class(TIdSipHttpAuthHeader)
  private
    function  GetDomain: String;
    function  GetStale: Boolean;
    procedure SetDomain(const Value: String);
    procedure SetStale(const Value: Boolean);
  protected
    function KnownResponse(const Name: String): Boolean; override;
  public
    function  CredentialHeaderType: TIdSipAuthorizationHeaderClass; virtual;
    procedure RemoveStaleResponse;

    property Domain: String  read GetDomain write SetDomain;
    property Stale:  Boolean read GetStale write SetStale;
  end;

  TIdSipProxyAuthenticateHeader = class(TIdSipAuthenticateHeader)
  protected
    function GetName: String; override;
  public
    function CredentialHeaderType: TIdSipAuthorizationHeaderClass; override;
  end;

  TIdSipAuthenticationInfoHeader = class(TIdSipAuthNonceHeader)
  private
    function  GetNextNonce: String;
    function  GetResponseDigest: String;
    procedure SetNextNonce(const Value: String);
    procedure SetResponseDigest(const Value: String);
  protected
    procedure CheckDigestResponses(Responses: TStrings); override;
    function  GetName: String; override;
    function  KnownResponse(const Name: String): Boolean; override;
    procedure Parse(const Value: String); override;
  public
    function HasNextNonce: Boolean;

    property NextNonce:      String read GetNextNonce write SetNextNonce;
    property ResponseDigest: String read GetResponseDigest write SetResponseDigest;
  end;

  TIdSipProxyAuthorizationHeader = class(TIdSipAuthorizationHeader)
  protected
    function GetName: String; override;
  end;

  TIdSipRetryAfterHeader = class(TIdSipNumericHeader)
  private
    fComment:  WideString;

    procedure EatLeadingWhitespace(var S: String); overload;
    procedure EatLeadingWhitespace(var W: WideString); overload;
    function  GetDuration: Cardinal;
    procedure ParseComment(CommentString: WideString);
    procedure SetDuration(const Value: Cardinal);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    function HasDuration: Boolean;

    property Comment:  WideString read fComment write fComment;
    property Duration: Cardinal   read GetDuration write SetDuration;
  end;

  TIdSipRouteHeader = class(TIdSipHeader)
  private
    fAddress:     TIdSipURI;
    fDisplayName: String;

    function  GetIsLooseRoutable: Boolean;
    procedure SetAddress(Value: TIdSipURI);
    procedure SetIsLooseRoutable(Value: Boolean);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function EncodeQuotedStr(const S: String): String;
    function HasSipsUri: Boolean;

    property Address:         TIdSipURI read fAddress write SetAddress;
    property DisplayName:     String    read fDisplayName write fDisplayName;
    property IsLooseRoutable: Boolean   read GetIsLooseRoutable write SetIsLooseRoutable;
  end;

  TIdSipRecordRouteHeader = class(TIdSipRouteHeader)
  protected
    function GetName: String; override;
  end;

  TIdSipTimestamp = class(TObject)
  private
    fFractionalPart: Cardinal;
    fIntegerPart:    Cardinal;
  public
    property FractionalPart: Cardinal read fFractionalPart write fFractionalPart;
    property IntegerPart:    Cardinal read fIntegerPart write fIntegerPart;
  end;

  TIdSipTimestampHeader = class(TIdSipHeader)
  private
    fDelay:     TIdSipTimestamp;
    fTimestamp: TIdSipTimestamp;
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function NormalizeLWS(const S: String): String;
    function ReadNumber(var Src: String): Cardinal;

    property Delay:     TIdSipTimestamp read fDelay;
    property Timestamp: TIdSipTimestamp read fTimestamp;
  end;

  TIdSipToHeader = class(TIdSipFromToHeader)
  protected
    function GetName: String; override;
  end;

  TIdSipViaHeader = class(TIdSipHeader)
  private
    fSipVersion: String;
    fTransport:  String;
    HostAndPort: TIdSipHostAndPort;

    procedure AssertBranchWellFormed;
    procedure AssertMaddrWellFormed;
    procedure AssertReceivedWellFormed;
    procedure AssertTTLWellFormed;
    function  GetBranch: String;
    function  GetMaddr: String;
    function  GetPort: Cardinal;
    function  GetReceived: String;
    function  GetRport: Cardinal;
    function  GetSentBy: String;
    function  GetTTL: Byte;
    procedure SetBranch(const Value: String);
    procedure SetMaddr(const Value: String);
    procedure SetPort(Value: Cardinal);
    procedure SetReceived(const Value: String);
    procedure SetRport(Value: Cardinal);
    procedure SetSentBy(const Value: String);
    procedure SetTransport(const Value: String);
    procedure SetTTL(Value: Byte);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;
    function  AsUri: String;
    function  DefaultPortForTransport(const Transport: String): Cardinal;
    function  HasBranch: Boolean;
    function  HasMaddr: Boolean;
    function  HasReceived: Boolean;
    function  HasRport: Boolean;
    function  IsDefaultPortForTransport(Port: Cardinal;
                                        const Transport: String): Boolean;
    function  IsRFC3261Branch: Boolean;
    procedure RemoveBranch;

    property Branch:     String              read GetBranch write SetBranch;
    property SentBy:     String              read GetSentBy write SetSentBy;
    property Maddr:      String              read GetMaddr write SetMaddr;
    property Port:       Cardinal            read GetPort write SetPort;
    property Received:   String              read GetReceived write SetReceived;
    property Rport:      Cardinal            read GetRport write SetRport;
    property SipVersion: String              read fSipVersion write fSipVersion;
    property Transport:  String              read fTransport write SetTransport;
    property TTL:        Byte                read GetTTL write SetTTL;
  end;

  TIdSipWarningHeader = class(TIdSipHeader)
  private
    fCode:       Cardinal;
    fText:       String;
    HostAndPort: TIdSipHostAndPort;

    function  GetAgent: String;
    procedure SetAgent(const Value: String);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure Parse(const Value: String); override;
  public
    class function IsHostPort(Token: String): Boolean;

    constructor Create; override;
    destructor  Destroy; override;

    property Agent: String   read GetAgent write SetAgent;
    property Code:  Cardinal read fCode write fCode;
    property Text:  String   read fText write fText;
  end;

  TIdSipChallengeHeader = class(TIdSipHeader)
  end;

  TIdSipWWWAuthenticateHeader = class(TIdSipAuthenticateHeader)
  protected
    function GetName: String; override;
  end;

  TIdSipHeaderMap = class(TObject)
  private
    fHeaderName:  String;
    fHeaderClass: TIdSipHeaderClass;
  public
    constructor Create(HeaderName: String; HeaderClass: TIdSipHeaderClass);

    property HeaderName:  String            read fHeaderName;
    property HeaderClass: TIdSipHeaderClass read fHeaderClass;
  end;

  // I represent a set of headers. I may or may not be ordered. That depends
  // on what sort've headers I contain. You may iterate over me using my
  // External Iterator methods.
  TIdSipHeaderList = class(TObject)
  protected
    class function HeaderTypes: TObjectList;
    class function IsHeader(const Header,
                            ExpectedHeaderName: String): Boolean;

    function ConstructHeader(HeaderName: String): TIdSipHeader;
    function GetItems(I: Integer): TIdSipHeader; virtual; abstract;
  public
    class function CanonicaliseName(HeaderName: String): String;
    class function GetHeaderName(Header: String): String;

    function  Add(const HeaderName: String): TIdSipHeader; overload; virtual; abstract;
    procedure Add(Copy: TIdSipHeader); overload; virtual; abstract;
    procedure Add(Headers: TIdSipHeaderList); overload; virtual; abstract;
    procedure AddInReverseOrder(Headers: TIdSipHeaderList);
    function  AsString: String;
    procedure Clear; virtual; abstract;
    function  Count: Integer; virtual; abstract;
    function  CurrentHeader: TIdSipHeader; virtual; abstract;
    procedure First; virtual; abstract;
    function  FirstMalformedHeader: TIdSipHeader;
    function  HasEqualValues(const OtherHeaders: TIdSipHeaderList): Boolean;
    function  IsMalformed: Boolean;
    function  HasNext: Boolean; virtual; abstract;
    function  Equals(OtherHeaders: TIdSipHeaderList): Boolean;
    function  IsEmpty: Boolean;
    procedure Next; virtual; abstract;
    procedure Remove(Header: TIdSipHeader); virtual; abstract;

    property Items[I: Integer]: TIdSipHeader read GetItems;
  end;

  // I am a filter over an underlying set of headers. If you iterate over me
  // you will only find HeaderName headers. You can still add arbitrary
  // headers to me (i.e., to the set of headers I filter).
  TIdSipHeadersFilter = class(TIdSipHeaderList)
  private
    CurrentIndex: Integer;
    fHeaderName:  String;
    Headers:      TIdSipHeaders;

  protected
    function GetItems(Index: Integer): TIdSipHeader; override;
  public
    constructor Create(Headers: TIdSipHeaders;
                       const HeaderName: String);

    function  Add(const HeaderName: String): TIdSipHeader; overload; override;
    procedure Add(Copy: TIdSipHeader); overload; override;
    procedure Add(Headers: TIdSipHeaderList); overload; override;
    procedure Clear; override;
    function  Count: Integer; override;
    function  CurrentHeader: TIdSipHeader; override;
    procedure First; override;
    function  HasNext: Boolean; override;
    procedure Next; override;
    procedure Remove(Header: TIdSipHeader); override;
    procedure RemoveAll;

    property HeaderName: String read fHeaderName;
  end;

  TIdSipHeaders = class(TIdSipHeaderList)
  private
    CurrentIndex: Integer;
    List:         TObjectList;

    function FindFirst(const HeaderName: String): TIdSipHeader;
    function GetHeaders(const Name: String): TIdSipHeader;
  protected
    function GetItems(I: Integer): TIdSipHeader; override;
  public
    class function IsCallID(const Header: String): Boolean;
    class function IsCompoundHeader(const Header: String): Boolean;
    class function IsContact(const Header: String): Boolean;
    class function IsContentLength(const Header: String): Boolean;
    class function IsCSeq(const Header: String): Boolean;
    class function IsErrorInfo(const Header: String): Boolean;
    class function IsFrom(const Header: String): Boolean;
    class function IsMaxForwards(const Header: String): Boolean;
    class function IsRecordRoute(const Header: String): Boolean;
    class function IsRoute(const Header: String): Boolean;
    class function IsTo(const Header: String): Boolean;
    class function IsVia(const Header: String): Boolean;
    class function IsWarning(const Header: String): Boolean;

    constructor Create; virtual;
    destructor  Destroy; override;

    function  Add(const HeaderName: String): TIdSipHeader; overload; override;
    procedure Add(Copy: TIdSipHeader); overload; override;
    procedure Add(Headers: TIdSipHeaderList); overload; override;
    procedure Clear; override;
    function  CurrentHeader: TIdSipHeader; override;
    procedure Delete(I: Integer);
    function  Count: Integer; override;
    procedure First; override;
    function  GetAllButFirst: TIdSipHeaderList;
    function  HasHeader(const HeaderName: String): Boolean;
    function  HasNext: Boolean; override;
    procedure Next; override;
    procedure Remove(Header: TIdSipHeader); override;
    procedure RemoveAll(const HeaderName: String);

    // This returns the FIRST MATCHING header
    property Headers[const Name: String]: TIdSipHeader read GetHeaders; default;
  end;

  TIdSipAuthorizations = class(TIdSipHeadersFilter)
  private
    BlankHeaders: TIdSipHeaders;
  public
    constructor Create(Headers: TIdSipHeaders); overload;
    constructor Create; overload;
    destructor  Destroy; override;

    function CurrentAuthorization: TIdSipAuthorizationHeader;
  end;

  TIdSipContacts = class(TIdSipHeadersFilter)
  private
    BlankHeaders: TIdSipHeaders;
  public
    constructor Create(Headers: TIdSipHeaders); overload;
    constructor Create; overload;
    destructor  Destroy; override;

    function ContactFor(Address: TIdSipAddressHeader): TIdSipContactHeader;
    function CurrentContact: TIdSipContactHeader;
    function HasContact(Address: TIdSipAddressHeader): Boolean;
  end;

  TIdSipExpiresHeaders = class(TIdSipHeadersFilter)
  public
    constructor Create(Headers: TIdSipHeaders);

    function CurrentExpires: Cardinal;
  end;

  TIdSipProxyAuthorizations = class(TIdSipHeadersFilter)
  private
    BlankHeaders: TIdSipHeaders;
  public
    constructor Create(Headers: TIdSipHeaders); overload;
    constructor Create; overload;
    destructor  Destroy; override;

    function CurrentProxyAuthorization: TIdSipProxyAuthorizationHeader;
  end;

  TIdSipRecordRoutePath = class(TIdSipHeadersFilter)
  private
    BlankHeaders: TIdSipHeaders;
  public
    constructor Create(Headers: TIdSipHeaders); overload;
    constructor Create; overload;
    destructor  Destroy; override;

    function CurrentRecordRoute: TIdSipRecordRouteHeader;
  end;

  TIdSipRoutePath = class(TIdSipHeadersFilter)
  private
    BlankHeaders: TIdSipHeaders;
  public
    constructor Create(Headers: TIdSipHeaders); overload;
    constructor Create; overload;
    destructor  Destroy; override;

    procedure AddRoute(RouteUri: TIdSipUri);
    function  CurrentRoute: TIdSipRouteHeader;
    function  GetAllButFirst: TIdSipRoutePath;
  end;

  TIdSipViaPath = class(TIdSipHeadersFilter)
  public
    constructor Create(Headers: TIdSipHeaders);

    function  LastHop: TIdSipViaHeader;
    function  Length: Integer;
    procedure RemoveLastHop;
  end;

  EBadMessage = class;
  EBadMessageClass = class of EBadMessage;

  TIdSipParser = class;
  TIdSipMessage = class;
  TIdSipMessageClass = class of TIdSipMessage;

  TIdSipMessage = class(TPersistent)
  private
    fBody:            String;
    fContacts:        TIdSipContacts;
    fPath:            TIdSipViaPath;
    fRecordRoute:     TIdSipRecordRoutePath;
    fIsMalformed:     Boolean;
    fHeaders:         TIdSipHeaders;
    fParseFailReason: String;
    fRawFirstLine:    String;
    fRawMessage:      String;
    fSIPVersion:      String;

    class function CreateAndReadMessageFrom(RawData: TStream;
                                            ClassType: TIdSipMessageClass): TIdSipMessage;

    function  ContentLengthEqualsBodyLength: Boolean;
    function  FirstHeaderValue(const HeaderName: String): String;
    function  FirstMalformedHeader: TIdSipHeader;
    function  GetCallID: String;
    function  GetContentDisposition: TIdSipContentDispositionHeader;
    function  GetContentLanguage: String;
    function  GetContentLength: Integer;
    function  GetContentType: String;
    function  GetCSeq: TIdSipCSeqHeader;
    function  GetFrom: TIdSipFromHeader;
    function  GetTo: TIdSipToHeader;
    function  HasBodyButMissingContentType: Boolean;
    procedure Initialize;
    function  Minimum(A, B: Cardinal): Cardinal;
    function  QuickestContactExpiry: Cardinal;
    function  QuickestExpiresHeader: Cardinal;
    procedure SetCallID(const Value: String);
    procedure SetContacts(Value: TIdSipContacts);
    procedure SetContentDisposition(Value: TIdSipContentDispositionHeader);
    procedure SetContentLanguage(const Value: String);
    procedure SetContentLength(Value: Integer);
    procedure SetContentType(const Value: String);
    procedure SetCSeq(Value: TIdSipCSeqHeader);
    procedure SetFrom(Value: TIdSipFromHeader);
    procedure SetPath(Value: TIdSipViaPath);
    procedure SetRecordRoute(Value: TIdSipRecordRoutePath);
    procedure SetTo(Value: TIdSipToHeader);
  protected
    procedure FailParse(const Reason: String);
    function  FirstLine: String; virtual; abstract;
    function  HasMalformedHeaders: Boolean;
    function  HasMalformedFirstLine: Boolean; virtual;
    function  MatchRequest(InitialRequest: TIdSipRequest;
                           UseCSeqMethod: Boolean): Boolean;
    function  MatchRFC2543Request(InitialRequest: TIdSipRequest;
                                     UseCSeqMethod: Boolean): Boolean; virtual; abstract;
    function  MatchRFC3261Request(InitialRequest: TIdSipRequest;
                                     UseCSeqMethod: Boolean): Boolean; virtual; abstract;
    function  MissingRequiredHeaders: Boolean; virtual;
    procedure ParseCompoundHeader(const Header: String;
                                  Parms: String);
    procedure ParseFirstLine(Parser: TIdSipParser);
    procedure ParseHeader(Parser: TIdSipParser;
                          const RawHeader: String);
    procedure ParseHeaders(Parser: TIdSipParser);
    procedure ParseStartLine(Parser: TIdSipParser); virtual; abstract;

    property RawFirstLine: String read fRawFirstLine;
  public
    class function MessageType(const FirstLine: String): TIdSipMessageClass;
    class function ReadMessageFrom(const RawData: String): TIdSipMessage; overload;
    class function ReadMessageFrom(RawData: TStream): TIdSipMessage; overload; virtual;
    class function ReadRequestFrom(const RawData: String): TIdSipRequest;
    class function ReadResponseFrom(const RawData: String): TIdSipResponse;
    class function WillEstablishDialog(Request: TIdSipRequest;
                                       Response: TIdSipResponse): Boolean; overload;

    constructor Create; virtual;
    destructor  Destroy; override;

    procedure Accept(Visitor: IIdSipMessageVisitor); virtual;
    function  AddHeader(const HeaderName: String): TIdSipHeader; overload;
    procedure AddHeader(Copy: TIdSipHeader); overload;
    procedure AddHeaders(Headers: TIdSipHeaderList);
    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    procedure ClearHeaders;
    function  ContactCount: Cardinal;
    function  Copy: TIdSipMessage;
    procedure CopyHeaders(Src: TIdSipMessage;
                          const HeaderName: String);
    function  FirstContact: TIdSipContactHeader;
    function  FirstExpires: TIdSipNumericHeader;
    function  FirstHeader(const HeaderName: String): TIdSipHeader;
    function  FirstMinExpires: TIdSipNumericHeader;
    function  FirstRequire: TIdSipCommaSeparatedHeader;
    function  FirstRetryAfter: TIdSipRetryAfterHeader;
    function  HasBody: Boolean;
    function  HasExpiry: Boolean;
    function  HasHeader(const HeaderName: String): Boolean;
    function  InSameDialogAs(Msg: TIdSipMessage): Boolean;
    function  IsMalformed: Boolean; virtual;
    function  HeaderCount: Integer;
    function  QuickestExpiry: Cardinal;
    function  Equals(Msg: TIdSipMessage): Boolean; virtual; abstract;
    function  IsRequest: Boolean; virtual; abstract;
    function  LastHop: TIdSipViaHeader;
    function  MalformedException: EBadMessageClass; virtual; abstract;
    procedure MarkAsInvalid(const Reason: String);
    function  ParseFailReason: String;
    procedure Parse(Parser: TIdSipParser); virtual;
    procedure ReadBody(Src: TStream);
    procedure RemoveHeader(Header: TIdSipHeader);
    procedure RemoveAllHeadersNamed(const Name: String);

    property Body:               String                         read fBody write fBody;
    property CallID:             String                         read GetCallID write SetCallID;
    property Contacts:           TIdSipContacts                 read fContacts write SetContacts;
    property ContentDisposition: TIdSipContentDispositionHeader read GetContentDisposition write SetContentDisposition;
    property ContentLanguage:    String                         read GetContentLanguage write SetContentLanguage;
    property ContentLength:      Integer                        read GetContentLength write SetContentLength;
    property ContentType:        String                         read GetContentType write SetContentType;
    property CSeq:               TIdSipCSeqHeader               read GetCSeq write SetCSeq;
    property From:               TIdSipFromHeader               read GetFrom write SetFrom;
    property Headers:            TIdSipHeaders                  read fHeaders;
    property Path:               TIdSipViaPath                  read fPath write SetPath;
    property RawMessage:         String                         read fRawMessage;
    property RecordRoute:        TIdSipRecordRoutePath          read fRecordRoute write SetRecordRoute;
    property SIPVersion:         String                         read fSIPVersion write fSIPVersion;
    property ToHeader:           TIdSipToHeader                 read GetTo write SetTo;
  end;

  TIdSipRequest = class(TIdSipMessage)
  private
    fMethod:     String;
    fRequestUri: TIdSipURI;
    fRoute:      TIdSipRoutePath;

    function  CSeqMatchesMethod: Boolean;
    function  FindAuthorizationHeader(const Realm: String;
                                      const HeaderType: String): TIdSipHeader;
    function  GetMaxForwards: Byte;
    procedure SetMaxForwards(Value: Byte);
    procedure SetRequestUri(Value: TIdSipURI);
    procedure SetRoute(Value: TIdSipRoutePath);
  protected
    function  FirstLine: String; override;
    function  HasMalformedFirstLine: Boolean; override;
    function  MatchRFC2543Request(InitialRequest: TIdSipRequest;
                                  UseCSeqMethod: Boolean): Boolean; override;
    function  MatchRFC3261Request(InitialRequest: TIdSipRequest;
                                  UseCSeqMethod: Boolean): Boolean; override;
    function  MissingRequiredHeaders: Boolean; override;
    procedure ParseStartLine(Parser: TIdSipParser); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Accept(Visitor: IIdSipMessageVisitor); override;
    function  AckFor(Response: TIdSipResponse): TIdSipRequest;
    function  AddressOfRecord: String;
    procedure Assign(Src: TPersistent); override;
    function  AuthorizationFor(const Realm: String): TIdSipAuthorizationHeader;
    function  CreateCancel: TIdSipRequest;
    function  DefaultMaxForwards: Cardinal;
    function  DestinationUri: String;
    function  FirstAuthorization: TIdSipAuthorizationHeader;
    function  FirstProxyAuthorization: TIdSipProxyAuthorizationHeader;
    function  FirstProxyRequire: TIdSipCommaSeparatedHeader;
    function  FirstRoute: TIdSipRouteHeader;
    function  HasAuthorization: Boolean;
    function  HasAuthorizationFor(const Realm: String): Boolean;
    function  HasProxyAuthorization: Boolean;
    function  HasProxyAuthorizationFor(const Realm: String): Boolean;
    function  HasRoute: Boolean;
    function  HasSipsUri: Boolean;
    function  IsAck: Boolean;
    function  IsBye: Boolean;
    function  IsCancel: Boolean;
    function  Equals(Msg: TIdSipMessage): Boolean; override;
    function  IsInvite: Boolean;
    function  IsMalformed: Boolean; override;
    function  IsOptions: Boolean;
    function  IsRegister: Boolean;
    function  IsRequest: Boolean; override;
    function  MalformedException: EBadMessageClass; override;
    function  Match(Msg: TIdSipMessage): Boolean;
    function  MatchCancel(Cancel: TIdSipRequest): Boolean;
    function  ProxyAuthorizationFor(const Realm: String): TIdSipProxyAuthorizationHeader;
    function  RequiresResponse: Boolean;

    property MaxForwards: Byte            read GetMaxForwards write SetMaxForwards;
    property Method:      String          read fMethod write fMethod;
    property RequestUri:  TIdSipURI       read fRequestUri write SetRequestUri;
    property Route:       TIdSipRoutePath read fRoute write SetRoute;
  end;

  // My RequestRequestUri property deserves some explanation: According to
  // RFC 3261 section 17.2.3, because RFC 2543 requests use their Request-URIs
  // to match transactions, you have to pass around the actual Transaction
  // object when you want to send a response. However, by storing the request's
  // Request-URI in RequestRequestUri, you can match responses against requests.
  // keeping knowledge of Transactions where they belong - in the Transaction
  // layer. Both TIdSipResponse.InResponseTo methods set RequestRequestUri for
  // you.
  TIdSipResponse = class(TIdSipMessage)
  private
    fRequestRequestUri: TIdSipUri;
    fStatusCode:        Integer;
    fStatusText:        String;

    procedure SetRequestRequestUri(Value: TIdSipUri);
    procedure SetStatusCode(Value: Integer);
  protected
    function  FirstLine: String; override;
    function  HasMalformedFirstLine: Boolean; override;
    function  MatchRFC2543Request(InitialRequest: TIdSipRequest;
                                  UseCSeqMethod: Boolean): Boolean; override;
    function  MatchRFC3261Request(InitialRequest: TIdSipRequest;
                                  UseCSeqMethod: Boolean): Boolean; override;
    procedure ParseStartLine(Parser: TIdSipParser); override;
  public
    class function InResponseTo(Request: TIdSipRequest;
                                StatusCode: Cardinal): TIdSipResponse; overload;
    class function InResponseTo(Request: TIdSipRequest;
                                StatusCode: Cardinal;
                                Contact: TIdSipContactHeader): TIdSipResponse; overload;

    constructor Create; override;
    destructor  Destroy; override;

    procedure Accept(Visitor: IIdSipMessageVisitor); override;
    procedure Assign(Src: TPersistent); override;
    function  AuthenticateHeader: TIdSipAuthenticateHeader;
    function  Description: String;
    function  FirstAuthenticationInfo: TIdSipAuthenticationInfoHeader;
    function  FirstProxyAuthenticate: TIdSipProxyAuthenticateHeader;
    function  FirstUnsupported: TIdSipCommaSeparatedHeader;
    function  FirstWarning: TIdSipWarningHeader;
    function  FirstWWWAuthenticate: TIdSipWWWAuthenticateHeader;
    function  Equals(Msg: TIdSipMessage): Boolean; override;
    function  HasAuthenticationInfo: Boolean;
    function  HasProxyAuthenticate: Boolean;
    function  HasWarning: Boolean;
    function  HasWWWAuthenticate: Boolean;
    function  IsAuthenticationChallenge: Boolean;
    function  IsFinal: Boolean;
    function  IsOK: Boolean;
    function  IsProvisional: Boolean;
    function  IsRedirect: Boolean;
    function  IsRequest: Boolean; override;
    function  IsTrying: Boolean;
    function  MalformedException: EBadMessageClass; override;
    function  WillEstablishDialog(Request: TIdSipRequest): Boolean; overload;

    property RequestRequestUri: TIdSipUri read fRequestRequestUri write SetRequestRequestUri;
    property StatusCode:        Integer   read fStatusCode write SetStatusCode;
    property StatusText:        String    read fStatusText write fStatusText;
  end;

  TIdSipRequestList = class(TObject)
  private
    List: TObjectList;

    function FromTheFront(Offset: Integer): TIdSipRequest;
    function GetItems(Index: Integer): TIdSipRequest;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddCopy(Request: TIdSipRequest);
    function  Count: Integer;
    procedure Delete(Index: Integer);
    function  First: TIdSipRequest;
    function  IsEmpty: Boolean;
    function  Last: TIdSipRequest;
    function  SecondLast: TIdSipRequest;
    function  ThirdLast: TIdSipRequest;

    property Items[Index: Integer]: TIdSipRequest read GetItems; default;
  end;

  TIdSipResponseList = class(TObject)
  private
    List: TObjectList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddCopy(Response: TIdSipResponse);
    function  Count: Integer;
    procedure Delete(Index: Integer);
    function  First: TIdSipResponse;
    function  IsEmpty: Boolean;
    function  Last: TIdSipResponse;
    function  SecondLast: TIdSipResponse;
  end;

  TIdSipParserError = procedure(const RawMessage, Reason: String) of object;

  {*
   * Some implementation principles we follow:
   *  * The original headers may arrived folded, may contain all manner of guff.
   *    We make no attempt to store the raw header - we unfold it (storing that
   *    unparsed), we parse it, and when we write out the headers we write them
   *    in the simplest possible way. As a result we CANNOT duplicate the exact
   *    form of the original message, even though the new message remains
   *    identical, semantically speaking.
   *  * We do (because we have to) keep the order of headers. We simple append
   *    any newly created headers.
   *  * New headers can be created that the original message didn't have.
   *    These messages will, by default, have the empty string as value. For example,
   *    querying the value of Content-Type will create a TIdSipHeader with Value ''.
   *  * We regard each header as using a value from a particular grammar, and the
   *    header classes each contain parsers for that language (in the SetValue
   *    method).
   *  * We always separate compound headers (that is, headers like Contactor Via)
   *    into separate headers. We do not do this for headers like Accept.
   *}
  TIdSipParser = class(TIdSimpleParser)
  public
    class function IsIPv6Reference(const Token: String): Boolean;
    class function IsMethod(Method: String): Boolean;
    class function IsQuotedString(const Token: String): Boolean;
    class function IsQValue(const Token: String): Boolean;
    class function IsRequest(FirstLine: String): Boolean;
    class function IsScheme(const Scheme: String): Boolean;
    class function IsSipVersion(Version: String): Boolean;
    class function IsToken(const Token: String): Boolean;
    class function IsTransport(const Token: String): Boolean;
    class function IsWord(const Token: String): Boolean;

    function GetHeaderName(Header: String): String;
    function GetHeaderValue(Header: String): String;
  end;

  EBadHeader = class(EParserError);
  EBadMessage = class(EParserError)
  private
    fRawMessage: String;
  public
    constructor Create(const Msg: String;
                       const RawMessage: String);
    property RawMessage: String read fRawMessage write fRawMessage;
  end;

  EBadRequest = class(EBadMessage);
  EBadResponse = class(EBadMessage);
  ESchemeNotSupported = class(Exception);

function DecodeQuotedStr(const S: String; var Dest: String): Boolean;
function EncodeQuotedStr(const S: String): String;
function FirstChar(const S: String): String;
function HalfQuoted(const S: String): Boolean;
function IsEqual(const S1, S2: String): Boolean;
function IsQuoted(const S: String): Boolean;
function LastChar(const S: String): String;
function NeedsQuotes(Name: String): Boolean;
function ParamToTransport(const TransportParam: String): String;
function ParseNameAddr(NameAddr: String; var DisplayName, AddrSpec: String): Boolean;
function ReadDigit(var Src: String): String;
function QuoteStringIfNecessary(const S: String): String;
function QValueToStr(const Q: TIdSipQValue): String;
function ShortMonthToInt(const Month: String): Integer;
function StreamToStr(Data: TStream): String;
function StrToQValue(const S: String): TIdSipQValue;
function StrToQValueDef(const S: String; const DefaultValue: TIdSipQValue): TIdSipQValue;
function WithoutFirstAndLastCharsW(const W: WideString): WideString;

// Widely known constants. Don't localise them.
const
  SipName    = 'SIP';
  SIPVersion = SipName + '/2.0';

// Header and parameter names. Don't localise them.
const
  AcceptHeader               = 'Accept';
  AcceptEncodingHeader       = 'Accept-Encoding';
  AcceptLanguageHeader       = 'Accept-Language';
  AlertInfoHeader            = 'Alert-Info';
  AlgorithmParam             = 'algorithm';
  AllowHeader                = 'Allow';
  AuthenticationInfoHeader   = 'Authentication-Info';
  AuthorizationHeader        = 'Authorization';
  BasicAuthorizationScheme   = 'Basic';
  BranchParam                = 'branch';
  CallIDHeaderFull           = 'Call-ID';
  CallIDHeaderShort          = 'i';
  CallInfoHeader             = 'Call-Info';
  CNonceParam                = 'cnonce';
  ContactHeaderFull          = 'Contact';
  ContactHeaderShort         = 'm';
  ContactWildCard            = '*';
  ContentDispositionHeader   = 'Content-Disposition';
  ContentEncodingHeaderFull  = 'Content-Encoding';
  ContentEncodingHeaderShort = 'e';
  ContentLanguageHeader      = 'Content-Language';
  ContentLengthHeaderFull    = 'Content-Length';
  ContentLengthHeaderShort   = 'l';
  ContentTypeHeaderFull      = 'Content-Type';
  ContentTypeHeaderShort     = 'c';
  CSeqHeader                 = 'CSeq';
  DateHeader                 = 'Date';
  DigestAuthorizationScheme  = 'Digest';
  DigestResponseParam        = 'response';
  DigestUriParam             = 'uri';
  DispositionAlert           = 'alert';
  DispositionIcon            = 'icon';
  DispositionRender          = 'render';
  DispositionSession         = 'session';
  DomainParam                = 'domain';
  DurationParam              = 'duration';
  ErrorInfoHeader            = 'Error-Info';
  ExpireNow                  = 0;
  ExpiresHeader              = 'Expires';
  ExpiresParam               = 'expires';
  FromHeaderFull             = 'From';
  FromHeaderShort            = 'f';
  HandlingOptional           = 'optional';
  HandlingParam              = 'handling';
  HandlingRequired           = 'required';
  InReplyToHeader            = 'In-Reply-To';
  LooseRoutableParam         = 'lr';
  MaddrParam                 = 'maddr';
  MaxForwardsHeader          = 'Max-Forwards';
  MD5Name                    = 'MD5';
  MD5SessionName             = 'MD5-sess';
  MethodAck                  = 'ACK';
  MethodBye                  = 'BYE';
  MethodCancel               = 'CANCEL';
  MethodInvite               = 'INVITE';
  MethodOptions              = 'OPTIONS';
  MethodParam                = 'method';
  MethodRegister             = 'REGISTER';
  MIMEVersionHeader          = 'MIME-Version';
  MinExpiresHeader           = 'Min-Expires';
  NextNonceParam             = 'nextnonce';
  NonceCountParam            = 'nc';
  NonceParam                 = 'nonce';
  OpaqueParam                = 'opaque';
  OrganizationHeader         = 'Organization';
  PriorityHeader             = 'Priority';
  ProxyAuthenticateHeader    = 'Proxy-Authenticate';
  ProxyAuthorizationHeader   = 'Proxy-Authorization';
  ProxyRequireHeader         = 'Proxy-Require';
  QopAuth                    = 'auth';
  QopAuthInt                 = 'auth-int';
  QopParam                   = 'qop';
  QParam                     = 'q';
  RealmParam                 = 'realm';
  ReceivedParam              = 'received';
  RecordRouteHeader          = 'Record-Route';
  ReplyToHeader              = 'Reply-To';
  RequireHeader              = 'Require';
  ResponseDigestParam        = 'rspauth';
  RetryAfterHeader           = 'Retry-After';
  RouteHeader                = 'Route';
  RportParam                 = 'rport';
  ServerHeader               = 'Server';
  SipScheme                  = 'sip';
  SipsScheme                 = 'sips';
  StaleParam                 = 'stale';
  SubjectHeaderFull          = 'Subject';
  SubjectHeaderShort         = 's';
  SupportedHeaderFull        = 'Supported';
  SupportedHeaderShort       = 'k';
  TagParam                   = 'tag';
  TimestampHeader            = 'Timestamp';
  ToHeaderFull               = 'To';
  ToHeaderShort              = 't';
  TransportParam             = 'transport';
  TransportParamSCTP         = 'sctp';
  TransportParamTCP          = 'tcp';
  TransportParamTLS          = 'tls';
  TransportParamUDP          = 'udp';
  TTLParam                   = 'ttl';
  UnsupportedHeader          = 'Unsupported';
  UserAgentHeader            = 'User-Agent';
  UsernameParam              = 'username';
  UserParam                  = 'user';
  UserParamIp                = 'ip';
  UserParamPhone             = 'phone';
  ViaHeaderFull              = 'Via';
  ViaHeaderShort             = 'v';
  WarningHeader              = 'Warning';
  WWWAuthenticateHeader      = 'WWW-Authenticate';

// Standard Status-Text messages for response Status-Codes
const
  RSSIPTrying                           = 'Trying';
  RSSIPRinging                          = 'Ringing';
  RSSIPCallIsBeingForwarded             = 'Call Is Being Forwarded';
  RSSIPQueued                           = 'Queued';
  RSSIPSessionProgress                  = 'Session Progress';
  RSSIPOK                               = 'OK';
  RSSIPMultipleChoices                  = 'Multiple Choices';
  RSSIPMovedPermanently                 = 'Moved Permanently';
  RSSIPMovedTemporarily                 = 'Moved Temporarily';
  RSSIPUseProxy                         = 'Use Proxy';
  RSSIPAlternativeService               = 'Alternative Service';
  RSSIPBadRequest                       = 'Bad Request';
  RSSIPUnauthorized                     = 'Unauthorized';
  RSSIPPaymentRequired                  = 'Payment Required';
  RSSIPForbidden                        = 'Forbidden';
  RSSIPNotFound                         = 'Not Found';
  RSSIPMethodNotAllowed                 = 'Method Not Allowed';
  RSSIPNotAcceptableClient              = 'Not Acceptable';
  RSSIPProxyAuthenticationRequired      = 'Proxy Authentication Required';
  RSSIPRequestTimeout                   = 'Request Timeout';
  RSSIPGone                             = 'Gone';
  RSSIPRequestEntityTooLarge            = 'Request Entity Too Large';
  RSSIPRequestURITooLarge               = 'Request-URI Too Large';
  RSSIPUnsupportedMediaType             = 'Unsupported Media Type';
  RSSIPUnsupportedURIScheme             = 'Unsupported URI Scheme';
  RSSIPBadExtension                     = 'Bad Extension';
  RSSIPExtensionRequired                = 'Extension Required';
  RSSIPIntervalTooBrief                 = 'Interval Too Brief';
  RSSIPTemporarilyUnavailable           = 'Temporarily unavailable';
  RSSIPCallLegOrTransactionDoesNotExist = 'Call Leg/Transaction Does Not Exist';
  RSSIPLoopDetected                     = 'Loop Detected';
  RSSIPTooManyHops                      = 'Too Many Hops';
  RSSIPAddressIncomplete                = 'Address Incomplete';
  RSSIPAmbiguous                        = 'Ambiguous';
  RSSIPBusyHere                         = 'Busy Here';
  RSSIPRequestTerminated                = 'Request Terminated';
  RSSIPNotAcceptableHere                = 'Not Acceptable Here';
  RSSIPRequestPending                   = 'Request Pending';
  RSSIPUndecipherable                   = 'Undecipherable';
  RSSIPInternalServerError              = 'Internal Server Error';
  RSSIPNotImplemented                   = 'Not Implemented';
  RSSIPBadGateway                       = 'Bad Gateway';
  RSSIPServiceUnavailable               = 'Service Unavailable';
  RSSIPServerTimeOut                    = 'Server Time-out';
  RSSIPSIPVersionNotSupported           = 'SIP Version not supported';
  RSSIPMessageTooLarge                  = 'Message Too Large';
  RSSIPBusyEverywhere                   = 'Busy Everywhere';
  RSSIPDecline                          = 'Decline';
  RSSIPDoesNotExistAnywhere             = 'Does not exist anywhere';
  RSSIPNotAcceptableGlobal              = RSSIPNotAcceptableClient;
  RSSIPUnknownResponseCode              = 'Unknown Response Code';

  RSSIPRequestOutOfOrder = 'Request out of order';

const
  SIPProvisionalResponseClass   = 1;
  SIPOKResponseClass            = 2;
  SIPRedirectionResponseClass   = 3;
  SIPFailureResponseClass       = 4;
  SIPServerFailureResponseClass = 5;
  SIPGlobalFailureResponseClass = 6;

// Well-known response Status-Codes
const
  SIPTrying                           = 100;
  SIPRinging                          = 180;
  SIPCallIsBeingForwarded             = 181;
  SIPQueued                           = 182;
  SIPSessionProgress                  = 183;
  SIPOK                               = 200;
  SIPMultipleChoices                  = 300;
  SIPMovedPermanently                 = 301;
  SIPMovedTemporarily                 = 302;
  SIPUseProxy                         = 305;
  SIPAlternativeService               = 380;
  SIPBadRequest                       = 400;
  SIPUnauthorized                     = 401;
  SIPPaymentRequired                  = 402;
  SIPForbidden                        = 403;
  SIPNotFound                         = 404;
  SIPMethodNotAllowed                 = 405;
  SIPNotAcceptableClient              = 406;
  SIPProxyAuthenticationRequired      = 407;
  SIPRequestTimeout                   = 408;
  SIPGone                             = 410;
  SIPRequestEntityTooLarge            = 413;
  SIPRequestURITooLarge               = 414;
  SIPUnsupportedMediaType             = 415;
  SIPUnsupportedURIScheme             = 416;
  SIPBadExtension                     = 420;
  SIPExtensionRequired                = 421;
  SIPIntervalTooBrief                 = 423;
  SIPTemporarilyUnavailable           = 480;
  SIPCallLegOrTransactionDoesNotExist = 481;
  SIPLoopDetected                     = 482;
  SIPTooManyHops                      = 483;
  SIPAddressIncomplete                = 484;
  SIPAmbiguous                        = 485;
  SIPBusyHere                         = 486;
  SIPRequestTerminated                = 487;
  SIPNotAcceptableHere                = 488;
  SIPRequestPending                   = 491;
  SIPUndecipherable                   = 493;
  SIPInternalServerError              = 500;
  SIPNotImplemented                   = 501;
  SIPBadGateway                       = 502;
  SIPServiceUnavailable               = 503;
  SIPServerTimeOut                    = 504;
  SIPSIPVersionNotSupported           = 505;
  SIPMessageTooLarge                  = 513;
  SIPBusyEverywhere                   = 600;
  SIPDecline                          = 603;
  SIPDoesNotExistAnywhere             = 604;
  SIPNotAcceptableGlobal              = 606;

// Standard warning codes
const
  WarningIncompatibleNetworkProtocol              = 300;
  WarningIncompatibleNetworkAddressFormats        = 301;
  WarningIncompatibleTransportProtocol            = 302;
  WarningIncompatibleBandwidthUnits               = 303;
  WarningMediaTypeNotAvailable                    = 304;
  WarningIncompatibleMediaFormat                  = 305;
  WarningAttributeNotUnderstood                   = 306;
  WarningSessionDescriptionParameterNotUnderstood = 307;
  WarningMulticastNotAvailable                    = 330;
  WarniningUnicastNotAvailable                    = 331;
  WarningInsufficientBandwidth                    = 370;
  WarningMisc                                     = 399;

// Standard warning strings
const
  RSWarningIncompatibleNetworkProtocol =
      'Incompatible network protocol: One or more network protocols contained '
    + 'in the session description are not available.';
  RSWarningIncompatibleNetworkAddressFormats =
      'Incompatible network address formats: One or more network address '
    + 'formats contained in the session description are not available.';
  RSWarningIncompatibleTransportProtocol =
      'Incompatible transport protocol: One or more transport protocols '
    + 'described in the session description are not available.';
  RSWarningIncompatibleBandwidthUnits =
      'Incompatible bandwidth units: One or more bandwidth measurement units '
    + 'contained in the session description were not understood.';
  RSWarningMediaTypeNotAvailable =
      'Media type not available: One or more media types contained in the '
    + 'session description are not available.';
  RSWarningIncompatibleMediaFormat =
      'Incompatible media format: One or more media formats contained in the '
    + 'session description are not available.';
  RSWarningAttributeNotUnderstood =
      'Attribute not understood: One or more of the media attributes in the '
    + 'session description are not supported.';
  RSWarningSessionDescriptionParameterNotUnderstood =
      'Session description parameter not understood: A parameter other than '
    + 'those listed above was not understood.';
  RSWarningMulticastNotAvailable =
      'Multicast not available: The site where the user is located does not '
    + 'support multicast.';
  RSWarniningUnicastNotAvailable =
      'Unicast not available: The site where the user is located does not '
    + 'support unicast communication (usually due to the presence of a '
    + 'firewall).';
  RSWarningInsufficientBandwidth =
      'Insufficient bandwidth: The bandwidth specified in the session '
    + 'description or defined by the media exceeds that known to be '
    + 'available.';
  RSWarningMisc =
      'Miscellaneous warning';

// Transport types. Don't localise them.
const
  NullTransport = 'NULL';
  SctpTransport = 'SCTP';
  TcpTransport  = 'TCP';
  TlsTransport  = 'TLS';
  UdpTransport  = 'UDP';

// Grammar definitions. Don't localise them.
const
  CRLF                  = #$D#$A;
  HeaderUnreservedChars = ['[', ']', '/', '?', ':', '+', '$'];
  HeaderChars           = HeaderUnreservedChars + UnreservedChars;
  LegalTokenChars       = Alphabet + Digits
                        + ['-', '.', '!', '%', '*', '_',
                           '+', '`', '''', '~'];
  LegalWordChars        = LegalTokenChars
                        + ['(', ')', '<', '>', ':', '\', '"', '/', '[',
                           ']', '?', '{', '}'];
  LWSChars              = [' ', #9, #10, #13];
  ParamUnreservedChars  = ['[', ']', '/', ':', '&', '+', '$'];
  ParamChars            = ParamUnreservedChars + UnreservedChars;
  PasswordChars         = UnreservedChars + ['&', '=', '+', '$', ','];
  UserUnreservedChars   = ['&', '=', '+', '$', ',', ';', '?', '/'];
  UserChars             = Alphabet + Digits + UnreservedChars + UserUnreservedChars;

// Error messages
const
  BadStatusCode               = -1;
  BadContentLength            = 'Content-Length must match body length';
  ConvertErrorMsg             = 'Failed to convert ''%s'' to type %s';
  CSeqMethodMismatch          = 'CSeq header method doesn''t match request method';
  InvalidBranchId             = 'Invalid branch-id';
  InvalidCallID               = 'Invalid Call-ID';
  InvalidComment              = 'Invalid Comment';
  InvalidDecimal              = 'Invalid decimal value';
  InvalidDelay                = 'Invalid delay value';
  InvalidDigestResponse       = 'Invalid digest-response';
  InvalidExpires              = 'Invalid Expires parameter';
  InvalidMaddr                = 'Invalid maddr';
  InvalidMethod               = 'Invalid method';
  InvalidNameAddr             = 'Invalid name-addr';
  InvalidNumber               = 'Invalid number';
  InvalidQuotedString         = 'Invalid quoted-string';
  InvalidQValue               = 'Invalid q-value';
  InvalidReceived             = 'Invalid received';
  InvalidSentProtocol         = 'Invalid sent-protocol';
  InvalidSequenceNumber       = 'Invalid sequence number';
  InvalidSipVersion           = 'Invalid Sip-Version: ''%s''';
  InvalidStatusCode           = 'Invalid Status-Code: ''%s''';
  InvalidTag                  = 'Invalid tag';
  InvalidTime                 = 'Invalid date/time';
  InvalidUri                  = 'Invalid URI';
  InvalidWarnAgent            = 'Invalid warn-agent';
  InvalidWarnCode             = 'Invalid warn-code';
  InvalidWarnText             = 'Invalid warn-text';
  MethodToken                 = 'Method';  
  MissingAngleBrackets        = 'Missing angle brackets';
  MissingCallID               = 'Missing Call-ID header';
  MissingContentType          = 'Missing Content-Type header with a non-empty message-body';
  MissingCSeq                 = 'Missing CSeq header';
  MissingFrom                 = 'Missing From header';
  MissingMaxForwards          = 'Missing Max-Forwards header';
  MissingScheme               = 'Missing URI scheme';
  MissingTo                   = 'Missing To header';
  MissingSipVersion           = 'Missing SIP-Version';
  MissingVia                  = 'Missing Via header';
  RequestLine                 = '%s %s %s' + CRLF;
  RequestUriNoAngleBrackets   = 'Request-URI may not be enclosed in <>';
  RequestUriNoSpaces          = 'Request-URI may not contain spaces';
  StatusLine                  = '%s %d %s' + CRLF;
  UnexpectedDisplayName       = 'Unexpected display-name';
  UnexpectedMessageLength     = 'Expected message-body length of %d but was %d';
  UnmatchedParentheses        = 'Unmatched parentheses';
  UnmatchedQuotes             = 'Unmatched quotes';
  UnmatchedQuotesForParameter = 'Unmatched quotes around a parameter';
  UnsupportedScheme           = 'Unsupported URI scheme';

implementation

uses
  IdGlobal, IdSipDialog, IdUnicode;

const
  OffsetMustBeNonNegative = 'Offset must be greater or equal to zero';

// class variables
var
  GCanonicalHeaderNames: TStrings;
  GIdSipHeadersMap:      TObjectList;

//******************************************************************************
//* Unit procedures & functions                                                *
//******************************************************************************
//* Unit private procedures & functions ****************************************

function DecodeQuotedStr(const S: String; var Dest: String): Boolean;
var
  I: Integer;
  FoundSlash: Boolean;
begin
  Result := true;

  // in summary:
  // '\' is illegal, '%s\' is illegal.

  Dest := S;

  if (Dest <> '') then begin
    if (Dest = '\') or (Dest = '"') then
      Result := false;

    if (Length(Dest) >= 2) and (Dest[Length(Dest)] = '\') and (Dest[Length(Dest) - 1] <> '\') then
      Result := false;

    // We use "<" and not "<=" because if a \ is the last character we have
    // a malformed string. Too, this allows use to Dest[I + 1]
    I := 1;
    while (I < Length(Dest)) and Result do begin
      Result := Dest[I] <> '"';
      FoundSlash := Dest[I] = '\';
      if FoundSlash then begin
        Delete(Dest, I, 1);

        // protect '\\'
        if FoundSlash then
          Inc(I);
      end
      else
        Inc(I);
    end;
  end;
end;

function EncodeQuotedStr(const S: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do begin
    if (S[I] in ['\', '"']) then
      Result := Result + '\';

    Result := Result + S[I];
  end;
end;

function FirstChar(const S: String): String;
begin
  Result := Copy(S, 1, 1);
end;

function HalfQuoted(const S: String): Boolean;
begin
  Result := (FirstChar(S) = '"') xor (LastChar(S) = '"');
end;

function IsEqual(const S1, S2: String): Boolean;
begin
  Result := Lowercase(S1) = Lowercase(S2);
end;

function IsQuoted(const S: String): Boolean;
begin
  Result := (FirstChar(S) = '"') and (LastChar(S) = '"')
end;

function LastChar(const S: String): String;
begin
  Result := Copy(S, Length(S), 1);
end;

function NeedsQuotes(Name: String): Boolean;
var
  Token: String;
begin
  if (Name = '') then
    Result := false
  else begin
    Result := false;

    while (Name <> '') do begin
      Token := Fetch(Name, ' ');
      Result := Result or not TIdSipParser.IsToken(Token);
    end;
  end;
end;

function ParamToTransport(const TransportParam: String): String;
begin
  Result := Uppercase(TransportParam);
end;

function ParseNameAddr(NameAddr: String; var DisplayName, AddrSpec: String): Boolean;
var
  Name: String;
begin
  AddrSpec    := '';
  DisplayName := '';

  NameAddr := Trim(NameAddr);

  Result := IndyPos('<', NameAddr) > 0;

  if Result then begin
    if (NameAddr[1] = '"') then begin
      Name := Trim(Fetch(NameAddr, '<'));
      Delete(Name, 1, 1);

      Result := Result and (IndyPos('"', Name) <> 0);

      Name := Copy(Name, 1, RPos('"', Name, -1) - 1);

      // There was an encoded ", which MUST NOT match the opening "
      Result := Result and not ((Name <> '') and (Name[Length(Name)] = '\'));

      Result := Result and DecodeQuotedStr(Name, DisplayName);
    end else begin
      DisplayName := Trim(Fetch(NameAddr, '<'));

      Result := Result and not NeedsQuotes(DisplayName);
    end;

    AddrSpec := Trim(Fetch(NameAddr, '>'));
  end;
end;

function ReadDigit(var Src: String): String;
var
  I: Integer;
begin
  // Read all (if any) leading digit characters from the string, removing them
  // from the source string in the process.

  I := 1;
  while (I <= Length(Src)) and (Src[I] in ['0'..'9']) do
    Inc(I);

  Result := Copy(Src, 1, I - 1);
  Delete(Src, 1, I - 1);
end;

function QuoteStringIfNecessary(const S: String): String;
begin
  Result := S;
  if NeedsQuotes(S) then
    Result := '"' + Result + '"'
end;

function QValueToStr(const Q: TIdSipQValue): String;
begin
  Result := IntToStr(Q div 1000);

  if (Q mod 1000 > 0) then begin
    Result := Result + '.';

    Result := Result + IntToStr(((Q mod 1000) div 100));
    Result := Result + IntToStr(((Q mod 100)  div 10));
    Result := Result + IntToStr(((Q mod 10)   div 1));

    while (Result[Length(Result)] = '0') do
      Delete(Result, Length(Result), 1);
  end;
end;

function ShortMonthToInt(const Month: String): Integer;
var
  Found: Boolean;
begin
  Found := false;
  for Result := Low(ShortMonthNames) to High(ShortMonthNames) do
    if IsEqual(ShortMonthNames[Result], Month) then begin
      Found := true;
      Break;
    end;

  if not Found then
    raise EConvertError.Create('Failed to convert ''' + Month + ''' to type Integer');
end;

function StreamToStr(Data: TStream): String;
var
  OriginalPosition: Int64;
  S:                TStringStream;
begin
  if not Assigned(Data) then begin
    Result := '';
    Exit;
  end;

  OriginalPosition := Data.Position;
  Data.Seek(0, soFromBeginning);
  try
    S := TStringStream.Create('');
    try
      S.CopyFrom(Data, 0);
      Result := S.DataString;
    finally
      S.Free;
    end;
  finally
    Data.Seek(OriginalPosition, soFromBeginning);
  end;
end;

function StrToQValue(const S: String): TIdSipQValue;
var
  Fraction, Int: String;
  Malformed:     Boolean;
  I:             Cardinal;
  E:             Integer;
  F:             Cardinal;
  Q:             Cardinal;
begin
  Q         := 0;
  F         := 0;
  Fraction  := S;
  Malformed := (Fraction = '') or (IndyPos(' ', S) > 0);

  if not Malformed then begin
    Malformed := (IndyPos('.', Fraction) > 0) and (Fraction[Length(Fraction)] = '.');

    Int := Fetch(Fraction, '.');

    Val(Int, I, E);
    Malformed := Malformed or (E <> 0) or (I > 1);

    Malformed := Malformed or (Length(Fraction) > 3);
    if (Fraction <> '') then begin
      while (Length(Fraction) < 3) do
        Fraction := Fraction + '0';

      Val(Fraction, F, E);
      Malformed := Malformed or (E <> 0) or (F > High(TIdSipQValue));
    end;

    Q := High(TIdSipQValue)*I + Trunc(F);
    Malformed := Malformed or (Q > High(TIdSipQValue));
  end;

  if Malformed then
    raise EConvertError.Create(Format(ConvertErrorMsg, [S, 'TIdSipQValue']));

  Assert(Q <= High(TIdSipQValue),
         'Sanity check assigning a Cardinal to a TIdSipQValue');
  Result := Q;
end;

function StrToQValueDef(const S: String; const DefaultValue: TIdSipQValue): TIdSipQValue;
begin
  try
    Result := StrToQValue(S);
  except
    on EConvertError do
      Result := DefaultValue;
  end;
end;

function WithoutFirstAndLastCharsW(const W: WideString): WideString;
begin
  Result := Copy(W, 2, Length(W) - 2);
end;

//******************************************************************************
//* TIdSipHostAndPort                                                          *
//******************************************************************************

class function TIdSipHostAndPort.CouldContainIPv6Reference(const Token: String): Boolean;
begin
  Result := (Token <> '') and (Token[1] = '[');
end;

//* TIdSipHostAndPort Private methods TIdSipHostAndPort

function TIdSipHostAndPort.GetValue: String;
begin
  Result := Self.Host;

  if (Self.Port <> Self.DefaultPort) or Self.PortIsSpecified then
    Result := Result + ':' + IntToStr(Self.Port);
end;

procedure TIdSipHostAndPort.SetPort(const Value: Cardinal);
begin
  Self.fPort           := Value;
  Self.PortIsSpecified := true;
end;

procedure TIdSipHostAndPort.SetValue(Value: String);
begin
  Self.PortIsSpecified := false;

  if Self.CouldContainIPv6Reference(Value) then begin
    // Re-add the ']' eaten by Fetch
    Self.Host := Fetch(Value, ']') + ']';

    // Eat up to the real host:port delimiter
    Fetch(Value, ':');
  end
  else begin
    // A numeric IPv4 address or a domain name
    Self.Host := Fetch(Value, ':');
  end;

  if (Value = '') then begin
    Self.Port := Self.DefaultPort;
    Self.PortIsSpecified := false;
  end
  else begin
    Self.Port := StrToIntDef(Value, Self.DefaultPort);
    Self.PortIsSpecified := true;
  end;
end;

//******************************************************************************
//* TIdUri                                                                     *
//******************************************************************************
//* TIdUri Public methods ******************************************************

constructor TIdUri.Create(URI: String = '');
begin
  inherited Create;

  Self.Initialize;
  Self.Parse(URI);
end;

destructor TIdUri.Destroy;
begin
  Self.HostAndPort.Free;

  inherited Destroy;
end;

function TIdUri.AsString: String;
begin
  Result := '';
end;

function TIdUri.IsSipUri: Boolean;
begin
  Result := (Lowercase(Self.Scheme) = SipScheme)
         or (Lowercase(Self.Scheme) = SipsScheme);
end;

//* TIdUri Protected methods ***************************************************

procedure TIdUri.Initialize;
begin
  Self.HostAndPort := TIdSipHostAndPort.Create;
end;

procedure TIdUri.Parse(Uri: String);
begin
  // Make no assumptions about how URI schemes parse; do nothing.
end;

procedure TIdUri.SetScheme(const Value: String);
begin
  Self.fScheme := Value;
end;

//* TIdUri Private methods *****************************************************

function TIdUri.GetHost: String;
begin
  Result := Self.HostAndPort.Host;
end;

function TIdUri.GetPort: Cardinal;
begin
  Result := Self.HostAndPort.Port;
end;

procedure TIdUri.SetHost(const Value: String);
begin
  Self.HostAndPort.Host := Value;
end;

procedure TIdUri.SetPort(const Value: Cardinal);
begin
  Self.HostAndPort.Port := Value;
end;

//******************************************************************************
//* TIdSipUri                                                                  *
//******************************************************************************
//* TIdSipUri Public methods ***************************************************

class function TIdSipUri.CreateUri(URI: String = ''): TIdUri;
var
  Scheme: String;
begin
  Result := nil;
  try
    if (URI = '') then
      Result := TIdSipUri.Create(URI)
    else begin
      Scheme := Lowercase(Fetch(URI, ':', false));
      if (Scheme = SipScheme) or (Scheme = SipsScheme) then
        Result := TIdSipUri.Create(URI)
//      else if (Scheme = SipsScheme) then
//        Result := TIdSipsUri.Create(URI)
      else raise ESchemeNotSupported.Create(URI);
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

class function TIdSipUri.Decode(const Src: String): String;
var
  CharCode: Integer;
  ESC:      String[2];
  I:        Integer;
begin
  Result := '';
  // S.G. 27/11/2002: Spaces is NOT to be encoded as "+".
  // S.G. 27/11/2002: "+" is a field separator in query parameter, space is...
  // S.G. 27/11/2002: well, a space

  I := 1;
  while I <= Length(Src) do begin
    if Src[I] <> '%' then begin
      Result := Result + Src[I];
      Inc(I);
    end
    else begin
      Inc(I); // skip the % char
      ESC := Copy(Src, I, 2); // Copy the escape code
      Inc(I, 2); // Then skip it.
      try
        CharCode := StrToInt('$' + ESC);
        if (CharCode > 0) and (CharCode < 256) then
          Result := Result + Char(CharCode);
      except
      end;
    end;
  end;
end;

class function TIdSipUri.Encode(const Src: String;
                                const SafeChars: TIdSipChars): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Src) do begin
    if Src[I] in SafeChars then
      Result := Result + Src[I]
    else
      Result := Result + '%' + IntToHex(Ord(Src[I]), 2);
  end;
end;

class function TIdSipUri.HeaderEncode(const NameOrValue: String): String;
begin
  Result := Self.Encode(NameOrValue, HeaderChars);
end;

class function TIdSipUri.IsParamNameOrValue(const Token: String): Boolean;
begin
  Result := Self.IsEscapedOrInSet(Token, ParamChars);
end;

class function TIdSipUri.IsPassword(const Token: String): Boolean;
begin
  Result := Self.IsEscapedOrInSet(Token, PasswordChars);
end;

class function TIdSipUri.IsUser(const Token: String): Boolean;
begin
  Result := Self.IsEscapedOrInSet(Token, UserChars);
end;

class function TIdSipUri.ParameterEncode(const Parameter: String): String;
begin
  Result := Self.Encode(Parameter, ParamChars);
end;

class function TIdSipUri.UsernameEncode(const Username: String): String;
begin
  Result := Self.Encode(Username, UserChars);
end;

destructor TIdSipUri.Destroy;
begin
  Self.Parameters.Free;
  Self.Headers.Free;

  inherited Destroy;
end;

procedure TIdSipUri.AddParameter(const Name: String;
                                 const Value: String = '');
begin
  Self.Parameters.Add(Name + '=' + Self.Decode(Value));
end;

function TIdSipUri.AsRouteHeader: TIdSipRouteHeader;
begin
  Result := TIdSipRouteHeader.Create;
  Result.Address := Self;
end;

function TIdSipUri.AsString: String;
begin
  Result := Self.Uri;
end;

function TIdSipUri.CanonicaliseAsAddressOfRecord: String;
var
  ResultUri: TIdSipUri;
begin
  ResultUri := TIdSipUri.Create(Self.Uri);
  try
    ResultUri.Password := '';
    ResultUri.Headers.Clear;
    ResultUri.Parameters.Clear;

    Result := TIdSipUri.Decode(ResultUri.Uri);
  finally
    ResultUri.Free;
  end;
end;

procedure TIdSipUri.ClearHeaders;
begin
  Self.Headers.Clear;
end;

procedure TIdSipUri.ClearParameters;
begin
  Self.Parameters.Clear;
end;

function TIdSipUri.CreateRequest: TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  Result.RequestUri := Self;

  if Result.RequestUri.HasParameter(MethodParam) then begin
    Result.Method := Result.RequestUri.ParamValue(MethodParam);
    Result.CSeq.Method := Result.Method;
    Result.RequestUri.RemoveParameter(MethodParam);
  end;

  Result.RequestUri.Headers.Clear;
end;

function TIdSipUri.DefaultPort: Cardinal;
begin
  if Self.IsSecure then
    Result := IdPORT_SIPS
  else
    Result := IdPORT_SIP;
end;

function TIdSipUri.DefaultTransport: String;
begin
  if Self.IsSecure then
    Result := TransportParamTLS
  else
    Result := TransportParamUDP;
end;

function TIdSipUri.Equals(Uri: TIdSipUri): Boolean;
begin
  // a SIP and SIPS URI are never equivalent
  Result := Lowercase(Self.Scheme) = Lowercase(Uri.Scheme);

  Result := Result
        and (Lowercase(Self.Host) = Lowercase(Uri.Host))
        and (Self.Port            = Uri.Port)
        and (Self.PortIsSpecified = Uri.PortIsSpecified)
        and (Self.Username        = Uri.Username)
        and (Self.Password        = Uri.Password)
        and Self.EqualParameters(Uri)
        and Self.Headers.Equals(Uri.Headers);
end;

procedure TIdSipUri.EraseUserInfo;
begin
  Self.Username := '';
  Self.Password := '';
end;

function TIdSipUri.HasValidSyntax: Boolean;
begin
  Result := Self.HasValidScheme
        and Self.HasValidHostInfo
        and Self.HasValidUserInfo
        and Self.HasValidParameters;

  if Self.IsSecure then
    Result := Result and (Self.Transport = TransportParamTLS);
end;

function TIdSipUri.HasHeaders: Boolean;
begin
  Result := not Self.Headers.IsEmpty;
end;

function TIdSipUri.HasMaddr: Boolean;
begin
  Result := Self.HasParameter(MaddrParam)
end;

function TIdSipUri.HasParameter(const Name: String): Boolean;
begin
  Result := Self.Parameters.IndexOfName(Name) <> -1;
end;

function TIdSipUri.IsLooseRoutable: Boolean;
begin
  Result := Self.HasParameter(LooseRoutableParam);
end;

function TIdSipUri.IsSecure: Boolean;
begin
  Result := IsEqual(Self.Scheme, SipsScheme);
end;

function TIdSipUri.ParamCount: Integer;
begin
  Result := Self.Parameters.Count;
end;

function TIdSipUri.ParamName(Index: Cardinal): String;
begin
  Result := Self.Parameters.Names[Index];
end;

function TIdSipUri.ParamValue(Index: Cardinal): String;
begin
  Result := Self.Parameters.Values[Self.ParamName(Index)];
end;

function TIdSipUri.ParamValue(const Name: String): String;
begin
  Result := Self.Parameters.Values[Name];
end;

function TIdSipUri.PortIsSpecified: Boolean;
begin
  Result := Self.HostAndPort.PortIsSpecified;
end;

procedure TIdSipUri.RemoveParameter(const Name: String);
begin
  if Self.HasParameter(Name) then
    Self.Parameters.Delete(Self.Parameters.IndexOfName(Name));
end;

function TIdSipUri.TransportIsSpecified: Boolean;
begin
  Result := Self.HasParameter(TransportParam);
end;

function TIdSipUri.UserIsIp: Boolean;
begin
  Result := IsEqual(Self.UserParameter, UserParamIp);
end;

function TIdSipUri.UserIsPhoneNumber: Boolean;
begin
  Result := IsEqual(Self.UserParameter, UserParamPhone);
end;

//* TIdSipUri Protected methods ************************************************

procedure TIdSipUri.Initialize;
begin
  inherited Initialize;

  Self.fHeaders    := TIdSipHeaders.Create;
  Self.Parameters  := TStringList.Create;
end;

procedure TIdSipUri.Parse(Uri: String);
begin
  Self.Reset;

  if (Uri <> '') then begin
    Self.Scheme := Fetch(Uri, ':');
    if (IndyPos('@', Uri) > 0) then
      Self.ParseUserInfo(Fetch(Uri, '@'));

    if (IndyPos(';', Uri) > 0) then begin
      Self.ParseHost(Fetch(Uri, ';'));
      Self.ParseParams(Fetch(Uri, '?'));

      if (Uri <> '') then
        Self.ParseHeaders(Uri);
    end
    else begin
      if (IndyPos('?', Uri) > 0) then begin
        Self.ParseHost(Fetch(Uri, '?'));
        Self.ParseHeaders(Uri);
      end
      else
        Self.ParseHost(Uri);
    end;
  end;
end;

procedure TIdSipUri.SetScheme(const Value: String);
begin
  inherited SetScheme(Value);

  if (Self.Scheme = SipsScheme) then
    Self.HostAndPort.DefaultPort := IdPORT_SIPS
  else
    Self.HostAndPort.DefaultPort := IdPORT_SIP;
end;

//* TIdSipUri Private methods **************************************************

class function TIdSipUri.IsEscapedOrInSet(const Token: String;
                                          AcceptableChars: TIdSipChars): Boolean;
var
  EndOfString: Integer;
  I:           Integer;
begin
  Result := Token <> '';

  if (Result) then begin
    EndOfString := Length(Token);
    I := 1;
    while (I <= EndOfString) and Result do begin
      if (Token[I] = '%') then begin
        Result := Result and ((EndOfString - I) >= 2)
                         and TIdSimpleParser.IsDigit(Token[I+1])
                         and TIdSimpleParser.IsDigit(Token[I+2]);
        Inc(I,2);
      end;

      Result := Result and (Token[I] in AcceptableChars);
      if not Result then Break;

      Inc(I);
    end;
  end;
end;

function TIdSipUri.EqualParameters(const Uri: TIdSipUri): Boolean;
begin
  Result := (Self.HasParameter(TransportParam) = Uri.HasParameter(TransportParam))
        and (Self.HasParameter(UserParam)      = Uri.HasParameter(UserParam))
        and (Self.HasParameter(TtlParam)       = Uri.HasParameter(TtlParam))
        and (Self.HasParameter(MethodParam)    = Uri.HasParameter(MethodParam))
        and (Self.HasParameter(MaddrParam)     = Uri.HasParameter(MaddrParam));

  // According to RFC 3261 section 19.1.4 if A and B are URIs then (apart
  // from special params - transport, ttl, etc) any parameters in both A.Params
  // and B.Params must have equal value. It's simple to compare one set against
  // another. Thus, we compare all the parameters in A.Params against those in
  // B.Params, and then vice versa.
  if Result then
    Result := Result
          and Self.UnidirectionalParameterCompare(Self, Uri)
          and Self.UnidirectionalParameterCompare(Uri, Self);
end;

function TIdSipUri.GetMaddr: String;
begin
  Result := Self.ParamValue(MaddrParam);
end;

function TIdSipUri.GetMethod: String;
begin
  Result := Self.ParamValue(MethodParam);
end;

function TIdSipUri.GetTransport: String;
begin
  Result := Self.ParamValue(TransportParam);

  if (Result = '') then
    Result := Self.DefaultTransport;
end;

function TIdSipUri.GetTTL: Cardinal;
begin
  Result := StrToInt(Self.ParamValue(TTLParam));
end;

function TIdSipUri.GetUri: String;
begin
  if (Self.Scheme = '') and (Self.Host = '') then
    Result := ''
  else begin
    Result := Self.Scheme + ':';

    if (Self.Username <> '') then begin
      Result := Result + Self.UsernameEncode(Self.Username);

      if (Self.Password <> '') then
        Result := Result + ':' + Self.Password;

      Result := Result + '@';
    end;

    Result := Result + Self.HostAndPort.Value;

    Result := Result + Self.ParamsAsString + Self.HeadersAsString;
  end;
end;

function TIdSipUri.GetUserParameter: String;
begin
  Result := Self.ParamValue(UserParam);
end;

function TIdSipUri.HasValidHostInfo: Boolean;
begin
  Result := TIdSimpleParser.IsFQDN(Self.Host)
         or TIdIPAddressParser.IsIPv4Address(Self.Host)
         or TIdIPAddressParser.IsIPv6Address(Self.Host);
end;

function TIdSipUri.HasValidParameters: Boolean;
var
  I: Integer;
begin
  Result := true;
  for I := 0 to Self.ParamCount - 1 do begin
    Result := Result and Self.IsParamNameOrValue(Self.ParamName(I))
                     and Self.IsParamNameOrValue(Self.ParamValue(I));

    if not Result then Break;
  end;
end;

function TIdSipUri.HasValidScheme: Boolean;
begin
  Result := (Self.Scheme = SipScheme) or (Self.Scheme = SipsScheme);
end;

function TIdSipUri.HasValidUserInfo: Boolean;
begin
  Result := ((Self.Username = '') or Self.IsUser(Self.Username))
        and ((Self.Password = '') or Self.IsPassword(Self.Password))
end;

function TIdSipUri.HeadersAsString: String;
var
  HeaderName:  String;
  HeaderValue: String;
begin
  Result := '';

  if not Self.Headers.IsEmpty then begin
    Self.Headers.First;
    while Self.Headers.HasNext do begin
      HeaderValue := Self.Headers.CurrentHeader.AsString;
      HeaderName  := Trim(Fetch(HeaderValue, ':'));
      HeaderValue := Trim(HeaderValue);

      Result := Result
              + TIdSipUri.HeaderEncode(HeaderName) + '='
              + TIdSipUri.HeaderEncode(HeaderValue) + '&';

      Self.Headers.Next;
    end;

    Result := '?' + Copy(Result, 1, Length(Result) - 1);
  end;
end;

function TIdSipUri.IsKnownParameter(const Name: String): Boolean;
var
  CaselessName: String;
begin
  CaselessName := Lowercase(Name);
  Result := (CaselessName = UserParam)
         or (CaselessName = TtlParam)
         or (CaselessName = MethodParam)
         or (CaselessName = MaddrParam)
end;

function TIdSipUri.ParamsAsString: String;
var
  I:     Integer;
  Param: String;
begin
  Result := '';

  for I := 0 to Self.Parameters.Count - 1 do begin
    Param := Self.ParamName(I);
    if (Self.ParamValue(I) <> '') then
      Param := Param + '=' + TIdSipUri.ParameterEncode(Self.ParamValue(I));

    Result := Result + ';' + Param;
  end;
end;

procedure TIdSipUri.ParseHeaders(HeaderList: String);
var
  HeaderName:  String;
  HeaderValue: String;
begin
  while (HeaderList <> '') do begin
    HeaderValue := Fetch(HeaderList, '&');
    HeaderName := Fetch(HeaderValue, '=');

    Self.Headers.Add(HeaderName).Value := TIdSipURI.Decode(HeaderValue);
  end;
end;

procedure TIdSipUri.ParseHost(HostAndPort: String);
begin
  Self.HostAndPort.Value := HostAndPort;
end;

procedure TIdSipUri.ParseParams(ParamList: String);
var
  ParamName:  String;
  ParamValue: String;
begin
  while (ParamList <> '') do begin
    ParamValue := Fetch(ParamList, ';');
    ParamName := Fetch(ParamValue, '=');

    Self.AddParameter(ParamName, ParamValue);
  end;
end;

procedure TIdSipUri.ParseUserInfo(UserInfo: String);
begin
  Self.Username := TIdSipURI.Decode(Fetch(UserInfo, ':'));
  Self.Password := UserInfo;
end;

procedure TIdSipUri.Reset;
begin
  Self.Headers.Clear;
  Self.Host      := '';
  Self.Parameters.Clear;
  Self.Password  := '';
  Self.Port      := Self.DefaultPort;
  Self.Scheme    := '';
  Self.Username  := '';

  Self.HostAndPort.PortIsSpecified := false;
end;

procedure TIdSipUri.SetMaddr(const Value: String);
begin
  Self.Parameters.Values[MaddrParam] := Value;
end;

procedure TIdSipUri.SetMethod(const Value: String);
begin
  Self.Parameters.Values[MethodParam] := Value;
end;

procedure TIdSipUri.SetTransport(const Value: String);
begin
  Self.Parameters.Values[TransportParam] := Value;
end;

procedure TIdSipUri.SetTTL(const Value: Cardinal);
begin
  Self.Parameters.Values[TTLParam] := IntToStr(Value);
end;

procedure TIdSipUri.SetUri(const Value: String);
begin
  Self.Parse(Value);
end;

procedure TIdSipUri.SetUserParameter(const Value: String);
begin
  Self.Parameters.Values[UserParam] := Value;
end;

function TIdSipUri.UnidirectionalParameterCompare(ThisUri, ThatUri: TIdSipUri): Boolean;
var
  I: Integer;
  OurName: String;
begin
  Result := true;

  for I := 0 to ThisUri.ParamCount - 1 do begin
    OurName := ThisUri.ParamName(I);

    if ThisUri.IsKnownParameter(OurName) then begin
      Result := Result
            and ThatUri.HasParameter(OurName)
            and (ThisUri.ParamValue(OurName) = ThatUri.ParamValue(OurName));
    end
    else begin
      if ThatUri.HasParameter(OurName) then
        Result := Result
              and (ThisUri.ParamValue(OurName) = ThatUri.ParamValue(OurName));
    end;
  end;
end;

//******************************************************************************
//* TIdSipHeader                                                               *
//******************************************************************************
//* TIdSipHeader Public methods ************************************************

class function TIdSipHeader.EncodeQuotedStr(const S: String): String;
begin
  Result := StringReplace(S,      '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll, rfIgnoreCase]);
end;

constructor TIdSipHeader.Create;
begin
  // This has no functional meaning. It just makes inspecting variables a bit
  // more sane for the developer. For instance, TIdSipViaHeader.fName will have
  // the value 'Via', even though a TIdSipViaHeader's fName cannot be read
  // because TIdSipViaHeader overrides GetName.
  Self.Name := Self.GetName;

  Self.fIsMalformed := false;
end;

destructor TIdSipHeader.Destroy;
begin
  fParams.Free;

  inherited Destroy;
end;

procedure TIdSipHeader.Assign(Src: TPersistent);
var
  H: TIdSipHeader;
begin
  if Src is TIdSipHeader then begin
    H := Src as TIdSipHeader;
    Self.Name       := H.Name;
    Self.Value      := H.Value;
    Self.Parameters := H.Parameters;
  end
  else inherited Assign(Src);
end;

function TIdSipHeader.AsString: String;
begin
  Result := Self.Name + ': ' + Self.FullValue;
end;

function TIdSipHeader.FullValue: String;
begin
  Result := Self.Value + Self.ParamsAsString;
end;

function TIdSipHeader.HasParam(Name: String): Boolean;
begin
  Result := Self.IndexOfParam(Name) <> -1;
end;

function TIdSipHeader.IndexOfParam(Name: String): Integer;
var
  Found:      Boolean;
  ParamValue: String;
  ParamName:  String;
begin
  Name := Trim(Name);

  Result := 0;
  Found  := false;
  while (Result < Self.Parameters.Count) and not Found do begin
    ParamValue := Self.Parameters[Result];
    ParamName := Fetch(ParamValue, '=');
    Found := IsEqual(Name, ParamName);
    if not Found then
      Inc(Result);
  end;

  if (Result = Self.Parameters.Count) then
    Result := -1;
end;

function TIdSipHeader.IsContact: Boolean;
begin
  Result := TIdSipHeaders.IsContact(Self.Name);
end;

function TIdSipHeader.Equals(Header: TIdSipHeader): Boolean;
begin
  Result := IsEqual(Self.AsString, Header.AsString);
end;

function TIdSipHeader.ParamCount: Integer;
begin
  if not Assigned(fParams) then
    Result := 0
  else
    Result := Self.Parameters.Count;
end;

function TIdSipHeader.ParamsAsString: String;
var
  I:          Integer;
  ParamName:  String;
  ParamValue: String;
begin
  Result := '';
  for I := 0 to Self.ParamCount - 1 do begin
    ParamValue := Self.Parameters[I];
    ParamName  := Fetch(ParamValue, '=');

    Result := Result + ';' + ParamName;

    ParamValue := QuoteStringIfNecessary(EncodeQuotedStr(ParamValue));

    if (ParamValue <> '') then
      Result := Result + '=' + ParamValue;
  end;
end;

procedure TIdSipHeader.RemoveParameter(const ParamName: String);
var
  Index: Integer;
begin
  Index := Self.Parameters.IndexOfName(ParamName);

  if (Index <> -1) then
    Self.Parameters.Delete(Index);
end;

//* TIdSipHeader Protected methods *********************************************

procedure TIdSipHeader.FailParse(const Reason: String);
begin
//  Self.MarkAsInvalid(Reason);
  raise EBadHeader.Create(Self.Name);
end;

function TIdSipHeader.GetName: String;
begin
  Result := fName;
end;

function TIdSipHeader.GetValue: String;
begin
  Result := fValue;
end;

procedure TIdSipHeader.MarkAsInvalid(const Reason: String);
begin
  Self.fIsMalformed     := true;
  Self.fParseFailReason := Reason;
end;

procedure TIdSipHeader.Parse(const Value: String);
var
  S: String;
begin
  S := Value;

  if (IndyPos(';', S) = 0) then
    fValue := S
  else
    fValue := Trim(Fetch(S, ';', false));

  Self.ParseParameters(S, Self.Parameters);
end;

procedure TIdSipHeader.ParseParameters(Value: String;
                                       Parameters: TStrings;
                                       Delimiter: String = ';');
var
  ParamName:  String;
  ParamValue: String;
  RealValue:  String;
begin
  Parameters.Clear;
  Fetch(Value, Delimiter);

  while (Value <> '') do begin
    ParamValue := Fetch(Value, Delimiter);
    ParamName  := Fetch(ParamValue, '=');

    ParamName  := Trim(ParamName);
    ParamValue := Trim(ParamValue);

    if (ParamValue = '') then
      if (ParamName = '') then
        Self.FailParse(Format(MalformedToken, ['parameter', '']))
      else
        Parameters.Add(ParamName)
    else begin
      if IsQuoted(ParamValue) then begin
        if not DecodeQuotedStr(WithoutFirstAndLastChars(ParamValue), RealValue) then
          Self.FailParse(UnmatchedQuotesForParameter);
        Parameters.Add(ParamName + '=' + RealValue)
      end
      else
        Parameters.Add(ParamName + '=' + ParamValue);
    end;
  end;
end;

procedure TIdSipHeader.SetName(const Value: String);
begin
  fName := Value;
end;

procedure TIdSipHeader.SetValue(const Value: String);
begin
  Self.fIsMalformed   := false;
  Self.fUnparsedValue := Value;

  try
    Self.Parse(Value);
  except
    on E: EBadHeader do
      Self.MarkAsInvalid(E.Message);
  end;
end;

//* TIdSipHeader Private methods ***********************************************

function TIdSipHeader.GetParam(const Name: String): String;
var
  I: Integer;
begin
  I := Self.IndexOfParam(Name);

  if (I > -1) then begin
    Result := Self.Parameters[I];
    Fetch(Result, '=');
    Result := Trim(Result)
  end
  else
    Result := '';
end;

function TIdSipHeader.GetParameters: TStrings;
begin
  if not Assigned(fParams) then
    fParams := TStringList.Create;

  Result := fParams;
end;

procedure TIdSipHeader.SetParam(const Name, Value: String);
var
  I: Integer;
begin
  I := Self.IndexOfParam(Name);


  if (I = -1) then
    Self.Parameters.Add(Trim(Name) + '=' + Value)
  else
    Self.Parameters[I] := Trim(Name) + '=' + Value;
end;

procedure TIdSipHeader.SetParameters(Value: TStrings);
begin
  Self.Parameters.Assign(Value);
end;

//******************************************************************************
//* TIdSipUriHeader                                                            *
//******************************************************************************
//* TIdSipUriHeader Public methods *********************************************

constructor TIdSipUriHeader.Create;
begin
  inherited Create;

  fAddress := TIdSipUri.Create('');
end;

destructor TIdSipUriHeader.Destroy;
begin
  fAddress.Free;

  inherited Destroy;
end;

function TIdSipUriHeader.GetValue: String;
begin
  Result := Self.Address.URI;

  if   (IndyPos(';', Result) > 0)
    or (IndyPos(',', Result) > 0)
    or (IndyPos('?', Result) > 0)
    or (Result <> '') then
    Result := '<' + Result + '>';
end;

procedure TIdSipUriHeader.Parse(const Value: String);
var
  AddrSpec:    String;
  DisplayName: String;
  S:           String;
begin
  Self.Address.URI := '';

  S := Trim(Value);
  if (IndyPos('<', Value) = 0) then
    Self.FailParse(MissingAngleBrackets);

  if not ParseNameAddr(Value, DisplayName, AddrSpec) then
    Self.FailParse(InvalidUri);
  if (DisplayName <> '') then
    Self.FailParse(UnexpectedDisplayName);

  Self.Address.URI := AddrSpec;
  Fetch(S, '>');

  inherited Parse(S);
end;

//* TIdSipUriHeader Private methods ********************************************

procedure TIdSipUriHeader.SetAddress(Value: TIdSipUri);
begin
  fAddress.URI := Value.URI;
end;

//******************************************************************************
//* TIdSipAddressHeader                                                        *
//******************************************************************************
//* TIdSipAddressHeader Public methods *****************************************

function TIdSipAddressHeader.AsAddressOfRecord: String;
begin
  Result := Self.Address.CanonicaliseAsAddressOfRecord;
end;

function TIdSipAddressHeader.AsToHeader: TIdSipToHeader;
begin
  Result := TIdSipToHeader.Create;
  Result.Value := Self.FullValue
end;

function TIdSipAddressHeader.HasSipsUri: Boolean;
begin
  Result := Self.Address.IsSecure;
end;

//* TIdSipAddressHeader Protected methods **************************************

function TIdSipAddressHeader.GetValue: String;
var
  URI: String;
begin
  Result := Self.DisplayName;
  if (IndyPos('"', Result) > 0) or (IndyPos('\', Result) > 0) then
    Result := Self.EncodeQuotedStr(Result);

  Result := QuoteStringIfNecessary(Result);

  URI := Self.Address.URI;
  if (IndyPos(';', URI) > 0) or (IndyPos(',', URI) > 0) or (IndyPos('?', URI) > 0) or (Result <> '') then
    URI := '<' + URI + '>';

  if (Result = '') then
    Result := URI
  else
    Result := Result + ' ' + URI;
end;

procedure TIdSipAddressHeader.Parse(const Value: String);
var
  AddrSpec:    String;
  DisplayName: String;
  S:           String;
begin
  Self.DisplayName := '';
  Self.Address.URI := '';

  S := Trim(Value);
  if (IndyPos('<', S) > 0) then begin
    if not ParseNameAddr(S, DisplayName, AddrSpec) then
      Self.FailParse(InvalidNameAddr);

    Self.Address.URI := AddrSpec;

    Fetch(S, '>');
    Self.ParseParameters(S, Self.Parameters);
  end
  else begin
    // Any semicolons in a URI not in angle brackets indicate that the
    // HEADER has the parameters, not the URI. Hence we separate the
    // parameters first to prevent the TIdSipUri from adding those
    // parameters to itself.
    Self.Address.URI := Trim(Fetch(S, ';'));

    if (S <> '') then
      S := ';' + S;

    Self.ParseParameters(S, Self.Parameters);
  end;

  if (Self.Address.Uri <> '')
    and not Self.Address.IsSipUri then
    Self.FailParse(UnsupportedScheme);

  Self.DisplayName := DisplayName;
end;

//******************************************************************************
//* TIdSipHttpAuthHeader                                                       *
//******************************************************************************
//* TIdSipHttpAuthHeader Public methods ****************************************

class function TIdSipHttpAuthHeader.IsNonce(const Token: String): Boolean;
var
  Unused: String;
begin
  Result := DecodeQuotedStr(Token, Unused);
end;

constructor TIdSipHttpAuthHeader.Create;
begin
  inherited Create;

  Self.DigestResponses   := TStringList.Create;
  Self.fUnknownResponses := TStringList.Create;
end;

destructor TIdSipHttpAuthHeader.Destroy;
begin
  Self.fUnknownResponses.Free;
  Self.DigestResponses.Free;

  inherited Destroy;
end;

//* TIdSipHttpAuthHeader Protected methods *************************************

procedure TIdSipHttpAuthHeader.CheckDigestResponses(Responses: TStrings);
begin
end;

function TIdSipHttpAuthHeader.DigestResponseValue(const Name: String): String;
begin
  if (Self.DigestResponses.IndexOfName(Name) = -1) then
    Result := ''
  else
    Result := Self.DigestResponses.Values[Name]
end;

function TIdSipHttpAuthHeader.GetValue: String;
var
  I:     Integer;
  Name:  String;
  Value: String;
begin
  Result := Self.AuthorizationScheme + ' ';

  for I := 0 to Self.DigestResponses.Count - 1 do begin
    Name  := Self.DigestResponses.Names[I];
    Value := Self.DigestResponses.Values[Name];

    Result := Result + Name
            + '=' + Self.QuoteIfNecessary(Name, Value) + ',';
  end;

  for I := 0 to Self.fUnknownResponses.Count - 1 do begin
    Name  := Self.fUnknownResponses.Names[I];
    Value := Self.fUnknownResponses.Values[Name];

    Result := Result + Name
            + '="' + EncodeQuotedStr(Value) + '",';
  end;

  if (LastChar(Result) = ',') then
    Result := Copy(Result, 1, Length(Result) - 1);
end;

function TIdSipHttpAuthHeader.KnownResponse(const Name: String): Boolean;
begin
  Result := (Name = AlgorithmParam)
         or (Name = NonceParam)
         or (Name = OpaqueParam)
         or (Name = QopParam)
         or (Name = RealmParam)
         or (Name = UsernameParam);
end;

procedure TIdSipHttpAuthHeader.Parse(const Value: String);
var
  S: String;

begin
  inherited Parse(Value);

  S := Value;
  Self.AuthorizationScheme := Fetch(S, ' ');

  Self.ParseDigestResponses(S);
  Self.CheckDigestResponses(Self.DigestResponses);
end;

function TIdSipHttpAuthHeader.QuoteIfNecessary(const ParamName, ParamValue: String): String;
begin
  if IsEqual(ParamName, QopParam) then
    Result := ParamValue
  else
    Result := '"' + EncodeQuotedStr(ParamValue) + '"';
end;

//* TIdSipHttpAuthHeader Private methods ***************************************

function TIdSipHttpAuthHeader.GetAlgorithm: String;
begin
  Result := Self.DigestResponseValue(AlgorithmParam);
end;

function TIdSipHttpAuthHeader.GetNonce: String;
begin
  Result := Self.DigestResponseValue(NonceParam);
end;

function TIdSipHttpAuthHeader.GetOpaque: String;
begin
  Result := Self.DigestResponseValue(OpaqueParam);
end;

function TIdSipHttpAuthHeader.GetQop: String;
begin
  Result := Self.DigestResponseValue(QopParam);
end;

function TIdSipHttpAuthHeader.GetRealm: String;
begin
  Result := Self.DigestResponseValue(RealmParam);
end;

function TIdSipHttpAuthHeader.GetUnknownResponses(const Name: String): String;
begin
  if (Self.fUnknownResponses.IndexOfName(Name) = -1) then
    Result := ''
  else
    Result := Self.fUnknownResponses.Values[Name]
end;

procedure TIdSipHttpAuthHeader.ParseDigestResponses(Value: String);
var
  DecodedValue:  String;
  ResponseName:  String;
  ResponseValue: String;
begin
  Self.DigestResponses.Clear;

  while (Value <> '') do begin
    ResponseValue := Fetch(Value, ',');
    ResponseName := Trim(Fetch(ResponseValue, '='));

    if IsQuoted(ResponseValue) then begin
      if not DecodeQuotedStr(WithoutFirstAndLastChars(ResponseValue), DecodedValue) then
        Self.FailParse(InvalidQuotedString);
    end
    else if HalfQuoted(ResponseValue) then
      Self.FailParse(UnmatchedQuotes)
    else
      DecodedValue := ResponseValue;

    if Self.KnownResponse(ResponseName) then
      Self.DigestResponses.Add(ResponseName + '=' + DecodedValue)
    else
      Self.fUnknownResponses.Add(ResponseName + '=' + DecodedValue);
  end;
end;

procedure TIdSipHttpAuthHeader.SetAlgorithm(const Value: String);
begin
  Self.DigestResponses.Values[AlgorithmParam] := Value;
end;

procedure TIdSipHttpAuthHeader.SetNonce(const Value: String);
begin
  Self.DigestResponses.Values[NonceParam] := Value;
end;

procedure TIdSipHttpAuthHeader.SetOpaque(const Value: String);
begin
  Self.DigestResponses.Values[OpaqueParam] := Value;
end;

procedure TIdSipHttpAuthHeader.SetQop(const Value: String);
begin
  Self.DigestResponses.Values[QopParam] := Value;
end;

procedure TIdSipHttpAuthHeader.SetRealm(const Value: String);
begin
  Self.DigestResponses.Values[RealmParam] := Value;
end;

procedure TIdSipHttpAuthHeader.SetUnknownResponses(const Name: String;
                                                        const Value: String);
begin
  Self.fUnknownResponses.Values[Name] := Value;
end;

//******************************************************************************
//* TIdSipAuthNonceHeader                                                      *
//******************************************************************************
//* TIdSipAuthNonceHeader Public methods ***************************************

function TIdSipAuthNonceHeader.NC: String;
begin
  // The encoded "nc" parameter
  Result := Self.DigestResponses.Values[NonceCountParam];
end;

//* TIdSipAuthNonceHeader Private methods **************************************

function TIdSipAuthNonceHeader.GetCNonce: String;
begin
  Result := Self.DigestResponseValue(CNonceParam);
end;

function TIdSipAuthNonceHeader.GetNonceCount: Cardinal;
begin
  Result := HexToInt(Self.DigestResponseValue(NonceCountParam));
end;

procedure TIdSipAuthNonceHeader.SetCNonce(const Value: String);
begin
  Self.DigestResponses.Values[CNonceParam] := Value;
end;

procedure TIdSipAuthNonceHeader.SetNonceCount(Value: Cardinal);
var
  H: String;
begin
  H := Lowercase(IntToHex(Value, 8));

  Self.DigestResponses.Values[NonceCountParam] := H;
end;

//******************************************************************************
//* TIdSipAuthorizationHeader                                                  *
//******************************************************************************
//* TIdSipAuthorizationHeader Public methods ***********************************

function TIdSipAuthorizationHeader.IsBasic: Boolean;
begin
  Result := IsEqual(Self.AuthorizationScheme, BasicAuthorizationScheme);
end;

function TIdSipAuthorizationHeader.IsDigest: Boolean;
begin
  Result := IsEqual(Self.AuthorizationScheme, DigestAuthorizationScheme);
end;

//* TIdSipAuthorizationHeader Protected methods ********************************

procedure TIdSipAuthorizationHeader.CheckDigestResponses(Responses: TStrings);
begin
  inherited CheckDigestResponses(Responses);

  if (Self.Response <> '')
    and not TIdSimpleParser.IsHexNumber(Self.Response) then
    Self.FailParse(InvalidDigestResponse);
end;

function TIdSipAuthorizationHeader.GetName: String;
begin
  Result := AuthorizationHeader;
end;

function TIdSipAuthorizationHeader.GetValue: String;
begin
  Result := inherited GetValue;
end;

//* TIdSipAuthorizationHeader Private methods **********************************

function TIdSipAuthorizationHeader.GetDigestUri: String;
begin
  Result := Self.DigestResponseValue(DigestUriParam);
end;

function TIdSipAuthorizationHeader.GetResponse: String;
begin
  Result := Self.DigestResponseValue(DigestResponseParam);
end;

function TIdSipAuthorizationHeader.GetUsername: String;
begin
  Result := Self.DigestResponseValue(UsernameParam);
end;

function TIdSipAuthorizationHeader.KnownResponse(const Name: String): Boolean;
begin
  Result := inherited KnownResponse(Name)
         or (Name = CNonceParam)
         or (Name = DigestResponseParam)
         or (Name = DigestUriParam)
         or (Name = NonceCountParam)
         or (Name = UsernameParam);
end;

function TIdSipAuthorizationHeader.QuoteIfNecessary(const ParamName, ParamValue: String): String;
begin
  if IsEqual(ParamName, NonceCountParam) then
    Result := ParamValue // We don't need to EncodeQuotedStr
  else
    Result := inherited QuoteIfNecessary(ParamName, ParamValue);
end;

procedure TIdSipAuthorizationHeader.SetDigestUri(const Value: String);
begin
  Self.DigestResponses.Values[DigestUriParam] := Value;
end;

procedure TIdSipAuthorizationHeader.SetResponse(const Value: String);
begin
  Self.DigestResponses.Values[DigestResponseParam] := Value;
end;

procedure TIdSipAuthorizationHeader.SetUsername(const Value: String);
begin
  Self.DigestResponses.Values[UsernameParam] := Value;
end;

//******************************************************************************
//* TIdSipCallIdHeader                                                         *
//******************************************************************************
//* TIdSipCallIdHeader Public methods ******************************************

function TIdSipCallIdHeader.Equals(Header: TIdSipHeader): Boolean;
begin
  Result := IsEqual(Header.Name, Self.Name)
        and (Self.Value = Header.Value);
end;

//* TIdSipCallIdHeader Protected methods ***************************************

function TIdSipCallIdHeader.GetName: String;
begin
  Result := CallIDHeaderFull;
end;

procedure TIdSipCallIdHeader.Parse(const Value: String);
var
  Val: String;
  Token: String;
begin
  if (IndyPos('@', Value) > 0) then begin
    Val := Value;
    Token := Fetch(Val, '@');
    if not TIdSipParser.IsWord(Val) or not TIdSipParser.IsWord(Token) then
      Self.FailParse(InvalidCallID);
  end
  else if not TIdSipParser.IsWord(Value) then
    Self.FailParse(InvalidCallID);

  inherited Parse(Value);
end;

//******************************************************************************
//* TIdSipCommaSeparatedHeader                                                 *
//******************************************************************************
//* TIdSipCommaSeparatedHeader Public methods **********************************

constructor TIdSipCommaSeparatedHeader.Create;
begin
  inherited Create;

  fValues := TStringList.Create;
end;

destructor TIdSipCommaSeparatedHeader.Destroy;
begin
  fValues.Free;

  inherited Destroy;
end;

procedure TIdSipCommaSeparatedHeader.RemoveValues(Header: TIdSipCommaSeparatedHeader);
var
  I: Integer;
begin
  // TODO: Find a better way to do this than Schlemiel the Painter!
  for I := 0 to Header.Values.Count - 1 do
    Self.Values.Delete(Self.Values.IndexOf(Header.Values[I]));
end;

//* TIdSipCommaSeparatedHeader Protected methods *******************************

function TIdSipCommaSeparatedHeader.GetValue: String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Self.Values.Count - 2 do
    Result := Result + Self.Values[I] + ', ';

  if (Self.Values.Count > 0) then
    Result := Result + Self.Values[Self.Values.Count - 1];
end;

procedure TIdSipCommaSeparatedHeader.Parse(const Value: String);
var
  S: String;
begin
  S := Value;

  Self.Values.Clear;

  while (S <> '') do begin
    Self.Values.Add(Trim(Fetch(S, ',')));
  end;
end;

//******************************************************************************
//* TIdSipWeightedValue                                                        *
//******************************************************************************
//* TIdSipWeightedValue Public methods *****************************************

destructor TIdSipWeightedValue.Destroy;
begin
  fParameters.Free;

  inherited Destroy;
end;

function TIdSipWeightedValue.AsString: String;
var
  I: Integer;
begin
  Result := Self.Value;

  if (Self.Weight < High(TIdSipQValue)) then
    Result := Result + ';q=' + QValueToStr(Self.Weight);

  for I := 0 to Self.Parameters.Count - 1 do
    Result := Result + ';' + Self.Parameters[I];
end;

//* TIdSipWeightedValue Private methods ****************************************

function TIdSipWeightedValue.GetParameters: TStrings;
begin
  if not Assigned(fParameters) then
    fParameters := TStringList.Create;

  Result := fParameters;
end;

//******************************************************************************
//* TIdSipWeightedCommaSeparatedHeader                                         *
//******************************************************************************
//* TIdSipWeightedCommaSeparatedHeader Public methods **************************

constructor TIdSipWeightedCommaSeparatedHeader.Create;
begin
  inherited Create;

  Self.fValues := TObjectList.Create(true);
end;

destructor TIdSipWeightedCommaSeparatedHeader.Destroy;
begin
  Self.fValues.Free;

  inherited Destroy;
end;

procedure TIdSipWeightedCommaSeparatedHeader.AddValue(const Value: String;
                                                      Weight: TIdSipQValue = High(TIdSipQValue));
var
  NewValue: TIdSipWeightedValue;
  OldCount: Integer;
begin
  OldCount := Self.ValueCount;
  NewValue := TIdSipWeightedValue.Create;
  try
    NewValue.Value := Value;
    NewValue.Weight := Weight;

    Self.fValues.Add(NewValue);
  except
    if (Self.ValueCount = OldCount) then
      NewValue.Free;
  end;
end;

procedure TIdSipWeightedCommaSeparatedHeader.ClearValues;
begin
  Self.fValues.Clear;
end;

function TIdSipWeightedCommaSeparatedHeader.ValueCount: Integer;
begin
  Result := Self.fValues.Count;
end;

//* TIdSipWeightedCommaSeparatedHeader Protected methods ***********************

function TIdSipWeightedCommaSeparatedHeader.GetValue: String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Self.ValueCount - 1 do begin
    Result := Result + Self.Values[I].AsString + ', ';
  end;

  Delete(Result, Length(Result) - 1, 2);
end;

procedure TIdSipWeightedCommaSeparatedHeader.Parse(const Value: String);
var
  S:          String;
  MediaRange: String;
  Params:     String;
  NewParams:  TStrings;
  QValue:     String;
begin
  Self.ClearValues;

  S := Value;

  while (S <> '') do begin
    MediaRange := Trim(Fetch(S, ','));

    if (IndyPos(';', MediaRange) > 0) then begin
      Params     := MediaRange;
      MediaRange := Fetch(Params, ';', false);
    end
    else
      Params := '';

    NewParams := TStringList.Create;
    try
      Self.ParseParameters(Params, NewParams);

      QValue := NewParams.Values[Qparam];

      if (QValue <> '')
        and not TIdSipParser.IsQValue(QValue) then
        Self.FailParse(InvalidQValue);

      Self.AddValue(MediaRange, StrToQValueDef(QValue, High(TIdSipQValue)));

      if (NewParams.IndexOfName(QParam) <> -1) then
        NewParams.Delete(NewParams.IndexOfName(QParam));

      Self.Values[Self.ValueCount - 1].Parameters.AddStrings(NewParams);
    finally
      NewParams.Free;
    end;
  end;
end;

//* TIdSipWeightedCommaSeparatedHeader Private methods *************************

function TIdSipWeightedCommaSeparatedHeader.GetValues(Index: Integer): TIdSipWeightedValue;
begin
  Result := fValues[Index] as TIdSipWeightedValue;
end;

procedure TIdSipWeightedCommaSeparatedHeader.SetValues(Index: Integer;
                                                       Value: TIdSipWeightedValue);
begin
  Self.Values[Index].Value := Value.Value;
  Self.Values[Index].Weight := Value.Weight;
end;

//******************************************************************************
//* TIdSipContactHeader                                                        *
//******************************************************************************
//* TIdSipContactHeader Public methods *****************************************

procedure TIdSipContactHeader.RemoveExpires;
begin
  Self.RemoveParameter(ExpiresParam);
end;

function TIdSipContactHeader.WillExpire: Boolean;
begin
  Result := Self.HasParam(ExpiresParam);
end;

//* TIdSipContactHeader Protected methods **************************************

function TIdSipContactHeader.GetName: String;
begin
  Result := ContactHeaderFull;
end;

function TIdSipContactHeader.GetValue: String;
begin
  if Self.IsWildCard then
    Result := ContactWildCard
  else
    Result := inherited GetValue;
end;

procedure TIdSipContactHeader.Parse(const Value: String);
var
  S: String;
begin
  S := Value;
  Self.IsWildCard := Fetch(S, ';') = ContactWildCard;

  if Self.IsWildCard then
    Self.ParseParameters(Value, Self.Parameters)
  else
    inherited Parse(Value);

  if (Self.IndexOfParam(QParam) > -1) and not TIdSipParser.IsQValue(Self.Params[QParam]) then
    Self.FailParse(InvalidQValue);
  if (Self.IndexOfParam(ExpiresParam) > -1) and not TIdSipParser.IsNumber(Self.Params[ExpiresParam]) then
    Self.FailParse(InvalidExpires);
end;

//* TIdSipContactHeader Private methods ****************************************

function TIdSipContactHeader.GetExpires: Cardinal;
begin
  Result := StrToInt(Self.Params[ExpiresParam]);
end;

function TIdSipContactHeader.GetQ: TIdSipQValue;
begin
  Result := StrToQValue(Self.Params[QParam]);
end;

procedure TIdSipContactHeader.SetExpires(Value: Cardinal);
begin
  Self.Params[ExpiresParam] := IntToStr(Value);
end;

procedure TIdSipContactHeader.SetQ(Value: TIdSipQValue);
begin
  Self.Params[QParam] := QValueToStr(Value);
end;

//******************************************************************************
//* TIdSipContentDispositionHeader                                             *
//******************************************************************************
//* TIdSipContentDispositionHeader Public methods ******************************

function TIdSipContentDispositionHeader.IsSession: Boolean;
begin
  Result := IsEqual(Self.Value, DispositionSession);
end;

//* TIdSipContentDispositionHeader Protected methods ***************************

function TIdSipContentDispositionHeader.GetName: String;
begin
  Result := ContentDispositionHeader;
end;

//* TIdSipContentDispositionHeader Private methods *****************************

function TIdSipContentDispositionHeader.GetHandling: String;
begin
  Result := Self.Params[HandlingParam];
end;

procedure TIdSipContentDispositionHeader.SetHandling(const Value: String);
begin
  Self.Params[HandlingParam] := Value;
end;

//******************************************************************************
//* TIdSipCSeqHeader                                                           *
//******************************************************************************
//* TIdSipCSeqHeader Public methods ********************************************

procedure TIdSipCSeqHeader.Increment;
begin
  if (Self.SequenceNo < High(Self.SequenceNo)) then
    Self.SequenceNo := Self.SequenceNo + 1
  else
    Self.SequenceNo := 0;
end;

//* TIdSipCSeqHeader Protected methods *****************************************

function TIdSipCSeqHeader.GetName: String;
begin
  Result := CSeqHeader;
end;

function TIdSipCSeqHeader.GetValue: String;
begin
  Result := IntToStr(Self.SequenceNo) + ' ' + Self.Method;
end;

procedure TIdSipCSeqHeader.Parse(const Value: String);
var
  Error: Integer;
  N:     Int64;
  S:     String;
  Token: String;
begin
  S := Trim(Value);
  // Yes, sure, there will be no spaces returned from Fetch(S, ' '). But what
  // about other kinds of whitespace? Best to be sure!
  Token := Trim(Fetch(S, ' '));

  // We use an Int64 because Val will raise an erroneous range error if Token
  // contains a number greater than High(Integer) - even if we declare N as a
  // Cardinal.
  Val(Token, N, Error);

  if ((Error <> 0) or (N > High(Self.SequenceNo))) then begin
    Self.FailParse(InvalidSequenceNumber);
  end;

  Self.SequenceNo := N;

  Token := Trim(S);
  if not TIdSipParser.IsMethod(Token) then
    Self.FailParse(InvalidMethod);

    Self.Method := Token;
end;

//******************************************************************************
//* TIdSipDateHeader                                                           *
//******************************************************************************
//* TIdSipDateHeader Public methods ********************************************

destructor TIdSipDateHeader.Destroy;
begin
  fAbsoluteTime.Free;

  inherited Destroy;
end;

function TIdSipDateHeader.ZeroDateTime: String;
begin
  Result := '30 Dec 1899 00:00:00 GMT';
end;

//* TIdSipDateHeader Protected methods *****************************************

function TIdSipDateHeader.GetName: String;
begin
  Result := DateHeader;
end;

function TIdSipDateHeader.GetValue: String;
begin
//  Result := inherited GetValue;
  Result := Self.Time.GetAsRFC822;
end;

procedure TIdSipDateHeader.Parse(const Value: String);
begin
  inherited Parse(Value);

  Self.SetAbsoluteTime(Value);
end;

//* TIdSipDateHeader Private methods *******************************************

function TIdSipDateHeader.GetAbsoluteTime: TIdDateTimeStamp;
begin
  if not Assigned(fAbsoluteTime) then
    fAbsoluteTime := TIdDateTimeStamp.Create(nil);

  Result := fAbsoluteTime;
end;

procedure TIdSipDateHeader.SetAbsoluteTime(Value: String);
begin
  // We cannot differentiate between a malformed date (which returns a zero)
  // and the date Self.ZeroTime
  Self.Time.SetFromRFC822(Value);

  // Therefore we try inspect the string manually. Yes, this method can be
  // fooled - "00:00:00 GMT hahahahahaha 1899-30Dec" will not raise a parse
  // error.
  if ((IndyPos('Dec', Value) = 0)
      or (IndyPos('30', Value) = 0)
      or (IndyPos('1899', Value) = 0)
      or (IndyPos('00:00:00 GMT', Value) = 0))
    and (Self.Time.AsTDateTime = 0) then
    Self.FailParse(InvalidTime);
end;

//******************************************************************************
//* TIdSipFromToHeader                                                         *
//******************************************************************************
//* TIdSipFromToHeader Public methods ******************************************

function TIdSipFromToHeader.HasTag: Boolean;
begin
  Result := Self.IndexOfParam(TagParam) <> -1;
end;

function TIdSipFromToHeader.Equals(Header: TIdSipHeader): Boolean;
var
  From: TIdSipFromToHeader;
begin
  Result := Header is TIdSipFromToHeader;

  if Result then begin
    From := Header as TIdSipFromToHeader;

    Result := (Self.Name = Header.Name)
          and (Self.Address.URI = From.Address.URI)
          and (Self.Tag = From.Tag);
  end;
end;

//* TIdSipFromToHeader Protected methods ***************************************

procedure TIdSipFromToHeader.Parse(const Value: String);
begin
  inherited Parse(Value);

  if (Self.IndexOfParam(TagParam) > -1)
    and not TIdSipParser.IsToken(Self.Params[TagParam]) then
    Self.FailParse(InvalidTag);
end;

//* TIdSipFromToHeader Private methods *****************************************

function TIdSipFromToHeader.GetTag: String;
begin
  Result := Self.Params[TagParam];
end;

procedure TIdSipFromToHeader.SetTag(const Value: String);

begin
  if (Value = '') then begin
    if Self.HasTag then
      Self.Parameters.Delete(Self.IndexOfParam(TagParam))
  end
  else begin
    Self.Params[TagParam] := Value;

    if Self.HasTag and not TIdSipParser.IsToken(Self.Params[TagParam]) then
      Self.FailParse(InvalidTag);
  end;
end;

//******************************************************************************
//* TIdSipFromHeader                                                           *
//******************************************************************************
//* TIdSipFromHeader Protected methods *****************************************

function TIdSipFromHeader.GetName: String;
begin
  Result := FromHeaderFull;
end;

//******************************************************************************
//* TIdSipNumericHeader                                                        *
//******************************************************************************
//* TIdSipNumericHeader Protected methods **************************************

function TIdSipNumericHeader.GetValue: String;
begin
  Result := IntToStr(fNumericValue);
end;

procedure TIdSipNumericHeader.Parse(const Value: String);
var
  Error: Integer;
  N:     Int64;
begin
  if not TIdSipParser.IsNumber(Value) then
    Self.FailParse(InvalidNumber)
  else begin
    Val(Value, N, Error);

    if ((Error <> 0) or (N > High(Self.fNumericValue))) then
      Self.FailParse(InvalidNumber);

    fNumericValue := N;

    inherited Parse(Value);
  end;
end;

//******************************************************************************
//* TIdSipMaxForwardsHeader                                                    *
//******************************************************************************
//* TIdSipMaxForwardsHeader Protected methods **********************************

function TIdSipMaxForwardsHeader.GetName: String;
begin
  Result := MaxForwardsHeader;
end;

procedure TIdSipMaxForwardsHeader.Parse(const Value: String);
var
  N: Cardinal;
  E: Integer;
begin
  Val(Value, N, E);

  if (E <> 0) or (N > 255) then
    Self.FailParse(InvalidNumber);

  inherited Parse(Value);
end;

//******************************************************************************
//* TIdSipAuthenticateHeader                                                   *
//******************************************************************************
//* TIdSipAuthenticateHeader Public methods ************************************

function TIdSipAuthenticateHeader.CredentialHeaderType: TIdSipAuthorizationHeaderClass;
begin
  Result := TIdSipAuthorizationHeader;
end;

procedure TIdSipAuthenticateHeader.RemoveStaleResponse;
var
  Index: Integer;
begin
  Index := Self.DigestResponses.IndexOfName(StaleParam);

  if (Index <> -1) then
    Self.DigestResponses.Delete(Index);
end;

//* TIdSipAuthenticateHeader Protected methods *********************************

function TIdSipAuthenticateHeader.KnownResponse(const Name: String): Boolean;
begin
  Result := inherited KnownResponse(Name)
         or (Name = DomainParam)
         or (Name = StaleParam);
end;

//* TIdSipAuthenticateHeader Private methods ***********************************

function TIdSipAuthenticateHeader.GetDomain: String;
begin
  Result := Self.DigestResponseValue(DomainParam);
end;

function TIdSipAuthenticateHeader.GetStale: Boolean;
begin
  Result := StrToBoolDef(Self.DigestResponseValue(StaleParam), false);
end;

procedure TIdSipAuthenticateHeader.SetDomain(const Value: String);
begin
  Self.DigestResponses.Values[DomainParam] := Value;
end;

procedure TIdSipAuthenticateHeader.SetStale(const Value: Boolean);
begin
    Self.DigestResponses.Values[StaleParam] := Lowercase(BoolToStr(Value));
end;

//******************************************************************************
//* TIdSipProxyAuthenticateHeader                                              *
//******************************************************************************
//* TIdSipProxyAuthenticateHeader Public methods *******************************

function TIdSipProxyAuthenticateHeader.CredentialHeaderType: TIdSipAuthorizationHeaderClass;
begin
  Result := TIdSipProxyAuthorizationHeader;
end;

//* TIdSipProxyAuthenticateHeader Protected methods ****************************

function TIdSipProxyAuthenticateHeader.GetName: String;
begin
  Result := ProxyAuthenticateHeader;
end;

//******************************************************************************
//* TIdSipAuthenticationInfoHeader                                             *
//******************************************************************************
//* TIdSipAuthenticationInfoHeader Public methods ******************************

function TIdSipAuthenticationInfoHeader.HasNextNonce: Boolean;
begin
  Result := Self.DigestResponseValue(NextNonceParam) <> '';
end;

//* TIdSipAuthenticationInfoHeader Protected methods ***************************

procedure TIdSipAuthenticationInfoHeader.CheckDigestResponses(Responses: TStrings);
begin
  if (Self.ResponseDigest <> '')
    and not TIdSimpleParser.IsHexNumber(Self.ResponseDigest) then
    Self.FailParse(InvalidDigestResponse);
end;

function TIdSipAuthenticationInfoHeader.GetName: String;
begin
  Result := AuthenticationInfoHeader;
end;

function TIdSipAuthenticationInfoHeader.KnownResponse(const Name: String): Boolean;
begin
  Result := (Name = NextNonceParam)
         or (Name = CNonceParam)
         or (Name = NonceCountParam)
         or (Name = QopParam)
         or (Name = ResponseDigestParam);
end;

procedure TIdSipAuthenticationInfoHeader.Parse(const Value: String);
begin
  Self.ParseDigestResponses(Value);
  Self.CheckDigestResponses(Self.DigestResponses);
end;

//* TIdSipAuthenticationInfoHeader Private methods *****************************

function TIdSipAuthenticationInfoHeader.GetNextNonce: String;
begin
  Result := Self.DigestResponseValue(NextNonceParam);
end;

function TIdSipAuthenticationInfoHeader.GetResponseDigest: String;
begin
  Result := Self.DigestResponseValue(ResponseDigestParam);
end;

procedure TIdSipAuthenticationInfoHeader.SetNextNonce(const Value: String);
begin
  Self.DigestResponses.Values[NextNonceParam] := Value;
end;

procedure TIdSipAuthenticationInfoHeader.SetResponseDigest(const Value: String);
begin
  Self.DigestResponses.Values[ResponseDigestParam] := Value;
end;

//******************************************************************************
//* TIdSipProxyAuthorizationHeader                                             *
//******************************************************************************
//* TIdSipProxyAuthorizationHeader Protected methods ***************************

function TIdSipProxyAuthorizationHeader.GetName: String;
begin
  Result := ProxyAuthorizationHeader;
end;

//******************************************************************************
//* TIdSipRetryAfterHeader                                                     *
//******************************************************************************
//* TIdSipRetryAfterHeader Public methods **************************************

function TIdSipRetryAfterHeader.HasDuration: Boolean;
begin
  Result := Self.HasParam(DurationParam);
end;

//* TIdSipRetryAfterHeader Protected methods ***********************************

function TIdSipRetryAfterHeader.GetName: String;
begin
  Result := RetryAfterHeader;
end;

function TIdSipRetryAfterHeader.GetValue: String;
begin
  Result := IntToStr(Self.NumericValue);

  if (Self.Comment <> '') then
    Result := Result + ' (' + UTF8Encode(Self.Comment) + ')';

  Result := Result + Self.ParamsAsString;
end;

procedure TIdSipRetryAfterHeader.Parse(const Value: String);
var
  Token: String;
  Raw:   String;
begin
  // Retry-After  =  "Retry-After" HCOLON delta-seconds
  //                 [ comment ] *( SEMI retry-param )

  Raw := Value;
  Token := ReadDigit(Raw);

  try
    Self.NumericValue := StrToInt(Token);
  except
    on EConvertError do
      Self.FailParse(InvalidNumber);
  end;

  if (Raw <> '') then begin
    if (IndyPos('(', Raw) > 0) then begin
      Self.ParseComment(UTF8Decode(Raw));
    end
    else begin
      // If there's anything in Raw, it must be only parameters.
      // If not, then it means there's something between the delta-seconds
      // and the parameters that doesn't match the definition of a comment/
      // Ergo, Raw has junk in it and we must fail.
      Token := Fetch(Raw, ';');
      if (Token <> '') then
        Self.FailParse(InvalidComment);
    end;

    // Parsing the parameters
    Self.ParseParameters(Value, Self.Parameters);
  end;
end;

//* TIdSipRetryAfterHeader Private methods *************************************

procedure TIdSipRetryAfterHeader.EatLeadingWhitespace(var S: String);
var
  I: Integer;
begin
  // Eat leading whitespace
  I := 1;
  while (S[I] in [' ', #9]) do
    Inc(I);

  S := Copy(S, I, Length(S));
end;

procedure TIdSipRetryAfterHeader.EatLeadingWhitespace(var W: WideString);
var
  I: Integer;
begin
  // Eat leading whitespace
  I := 1;
  while (Ord(W[I]) in [$20, $09]) do
    Inc(I);

  W := Copy(W, I, Length(W));
end;

function TIdSipRetryAfterHeader.GetDuration: Cardinal;
begin
  if Self.HasDuration then
    Result := StrToInt(Self.Params[DurationParam])
  else
    Result := 0;
end;

procedure TIdSipRetryAfterHeader.ParseComment(CommentString: WideString);
var
  I:                Integer;
  ParenthesisCount: Integer;
begin
  Self.fComment    := '';
  ParenthesisCount := 0;

  Self.EatLeadingWhitespace(CommentString);

  CommentString := Copy(CommentString, 1, RPosW(')', CommentString));
  CommentString := WithoutFirstAndLastCharsW(CommentString);

  I := 1;
  while (I <= Length(CommentString)) do begin
    case CommentString[I] of
      '\': Inc(I);
      '(': Inc(ParenthesisCount);
      ')': Dec(ParenthesisCount);
    end;

    Self.fComment := Self.fComment + CommentString[I];

    Inc(I);
  end;

  if (ParenthesisCount <> 0) then
    Self.FailParse(UnmatchedParentheses);
end;

procedure TIdSipRetryAfterHeader.SetDuration(const Value: Cardinal);
begin
  if (Value > 0) then
    Self.Params[DurationParam] := IntToStr(Value)
  else
    Self.Parameters.Delete(Self.IndexOfParam(DurationParam));
end;

//******************************************************************************
//* TIdSipRouteHeader                                                          *
//******************************************************************************
//* TIdSipRouteHeader Public methods *******************************************

constructor TIdSipRouteHeader.Create;
begin
  inherited Create;

  fAddress := TIdSipURI.Create('');
end;

destructor TIdSipRouteHeader.Destroy;
begin
  fAddress.Free;

  inherited Destroy;
end;

function TIdSipRouteHeader.EncodeQuotedStr(const S: String): String;
begin
  Result := S;
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll, rfIgnoreCase]);
end;

function TIdSipRouteHeader.HasSipsUri: Boolean;
begin
  Result := Self.Address.Scheme = SipsScheme;
end;

//* TIdSipRouteHeader Protected methods ****************************************

function TIdSipRouteHeader.GetName: String;
begin
  Result := RouteHeader;
end;

function TIdSipRouteHeader.GetValue: String;
var
  URI: String;
begin
  Result := Self.DisplayName;
  if (IndyPos('"', Result) > 0) or (IndyPos('\', Result) > 0) then
    Result := Self.EncodeQuotedStr(Result);

  Result := QuoteStringIfNecessary(Result);

  URI := '<' + Self.Address.URI + '>';

  if (Result = '') then
    Result := URI
  else
    Result := Result + ' ' + URI;
end;

procedure TIdSipRouteHeader.Parse(const Value: String);
var
  AddrSpec:     String;
  DisplayName:  String;
  HeaderParams: String;
begin
  if not ParseNameAddr(Value, DisplayName, AddrSpec) then
    Self.FailParse(InvalidUri);

  if (IndyPos(':', AddrSpec) = 0) then
    Self.FailParse(MissingScheme);

  Self.Address.URI := AddrSpec;
  Self.DisplayName := DisplayName;

  // cull the processed name-addr (and its parameters!)
  HeaderParams := Value;
  Fetch(HeaderParams, '>');

  inherited Parse(HeaderParams);
end;

//* TIdSipRouteHeader Private methods ******************************************

function TIdSipRouteHeader.GetIsLooseRoutable: Boolean;
begin
  Result := IndyPos(LooseRoutableParam, Self.Address.URI) > 0;
end;

procedure TIdSipRouteHeader.SetAddress(Value: TIdSipURI);
begin
  fAddress.URI := Value.URI;
end;

procedure TIdSipRouteHeader.SetIsLooseRoutable(Value: Boolean);
begin
  if Value then begin
    if not Self.Address.HasParameter(LooseRoutableParam) then
      Self.Address.AddParameter(LooseRoutableParam);
  end
  else begin
    Self.Address.RemoveParameter(LooseRoutableParam);
  end;
end;

//******************************************************************************
//* TIdSipRecordRouteHeader                                                    *
//******************************************************************************
//* TIdSipRecordRouteHeader Protected methods **********************************

function TIdSipRecordRouteHeader.GetName: String;
begin
  Result := RecordRouteHeader;
end;

//******************************************************************************
//* TIdSipTimestampHeader                                                      *
//******************************************************************************
//* TIdSipTimestampHeader Public methods ***************************************

constructor TIdSipTimestampHeader.Create;
begin
  inherited Create;

  fDelay     := TIdSipTimestamp.Create;
  fTimestamp := TIdSipTimestamp.Create;
end;

destructor TIdSipTimestampHeader.Destroy;
begin
  Self.Timestamp.Free;
  Self.Delay.Free;

  inherited Destroy;
end;

function TIdSipTimestampHeader.NormalizeLWS(const S: String): String;
var
  I: Integer;
begin
  Result := S;

  I := 1;
  while (I <= Length(Result)) do begin
    if (Result[I] in LWSChars) then begin
      Result[I] := ' ';
      Inc(I);

      while (Result[I] in LWSChars) do
        Delete(Result, I, 1);
    end;
    Inc(I);
  end;
end;

function TIdSipTimestampHeader.ReadNumber(var Src: String): Cardinal;
var
  I: Integer;
  Number: String;
begin
  Result := 0;
  if (Src = '') then Self.FailParse(InvalidNumber);

  I := 1;
  while (I <= Length(Src)) and (Src[I] in Digits) do Inc(I);


  Number := Copy(Src, 1, I - 1);
  if not TIdSipParser.IsNumber(Number) then Self.FailParse(InvalidNumber);

  try
    Result := StrToInt(Number);
    Delete(Src, 1, I - 1);
  except
    on EConvertError do
      Self.FailParse(InvalidNumber);
    on ERangeError do
      Self.FailParse(InvalidNumber);
    else raise;
  end;
end;

//* TIdSipTimestampHeader Protected methods ************************************

function TIdSipTimestampHeader.GetName: String;
begin
  Result := TimestampHeader;
end;

function TIdSipTimestampHeader.GetValue: String;
begin
  Result := IntToStr(Self.Timestamp.IntegerPart);

  if (Self.Timestamp.FractionalPart <> 0) then
    Result := Result + IntToStr(Self.Timestamp.FractionalPart);
end;

procedure TIdSipTimestampHeader.Parse(const Value: String);
var
  S: String;
begin
  Self.Delay.FractionalPart     := 0;
  Self.Delay.IntegerPart        := 0;
  Self.Timestamp.FractionalPart := 0;
  Self.Timestamp.IntegerPart    := 0;

  S := Self.NormalizeLWS(Value);

  Self.Timestamp.IntegerPart := Self.ReadNumber(S);
  if (S <> '') then begin
    if (S[1] <> '.') then Self.FailParse(InvalidDecimal);
    Delete(S, 1, 1);

    if (S <> '') then
      Self.Timestamp.FractionalPart := Self.ReadNumber(S);
  end;

  if (S <> '') then begin
    if (S[1] <> ' ') then Self.FailParse(InvalidDelay);
    Delete(S, 1, 1);

    if (S[1] = '.') then
      Self.Delay.IntegerPart := 0
    else
      Self.Delay.IntegerPart := Self.ReadNumber(S);
  end;

  if (S <> '') then begin
    if (S[1] <> '.') then Self.FailParse(InvalidDecimal);
    Delete(S, 1, 1);

    Self.Delay.FractionalPart := Self.ReadNumber(S);
  end;
end;

//******************************************************************************
//* TIdSipToHeader                                                             *
//******************************************************************************
//* TIdSipToHeader Protected methods *******************************************

function TIdSipToHeader.GetName: String;
begin
  Result := ToHeaderFull;
end;

//******************************************************************************
//* TIdSipViaHeader                                                            *
//******************************************************************************
//* TIdSipViaHeader Public methods *********************************************

constructor TIdSipViaHeader.Create;
begin
  inherited Create;

  Self.HostAndPort := TIdSipHostAndPort.Create;
end;

destructor TIdSipViaHeader.Destroy;
begin
  Self.HostAndPort.Free;

  inherited Destroy;
end;

procedure TIdSipViaHeader.Assign(Src: TPersistent);
var
  V: TIdSipViaHeader;
begin
  if (Src is TIdSipViaHeader) then begin
    V := Src as TIdSipViaHeader;

    // Yes, we're referencing private variables directly. We do this so we
    // avoid the parse checking that the setters normally do. For instance,
    // a blank or RFC 2543 style branch is invalid in RFC 3261, but we still
    // need to be able to work with the (malformed) Via.
    Self.SentBy          := V.SentBy;
    Self.SipVersion      := V.SipVersion;
    Self.Port            := V.Port;
    Self.Transport       := V.Transport;

    Self.HostAndPort.PortIsSpecified := V.HostAndPort.PortIsSpecified;

    // And we use the usual way of setting everything else.
    Self.Parameters := V.Parameters;
  end
  else
    inherited Assign(Src);
end;

function TIdSipViaHeader.AsUri: String;
begin
  // Transport registry!
  if (Self.Transport = TlsTransport) then
    Result := SipsScheme
  else
    Result := SipScheme;

  Result := Result + Self.HostAndPort.Value;
end;

function TIdSipViaHeader.DefaultPortForTransport(const Transport: String): Cardinal;
begin
  if (Transport = TlsTransport) then
    Result := IdPort_SIPS
  else
    Result := IdPORT_SIP;
end;

function TIdSipViaHeader.HasBranch: Boolean;
begin
  Result := Self.Branch <> '';
end;

function TIdSipViaHeader.HasMaddr: Boolean;
begin
  Result := Self.Maddr <> '';
end;

function TIdSipViaHeader.HasReceived: Boolean;
begin
  Result := Self.Received <> '';
end;

function TIdSipViaHeader.HasRport: Boolean;
begin
  Result := Self.HasParam(RPortParam);
end;

function TIdSipViaHeader.IsDefaultPortForTransport(Port: Cardinal;
                                                   const Transport: String): Boolean;
begin
  Result := ((Transport = TlsTransport) and (Port = IdPORT_SIPS))
         or (Port = IdPORT_SIP);
end;

function TIdSipViaHeader.IsRFC3261Branch: Boolean;
begin
  Result := Copy(Self.Branch, 1, Length(BranchMagicCookie)) = BranchMagicCookie;
end;

procedure TIdSipViaHeader.RemoveBranch;
begin
  // Really, this method just serves a debugging purpose, allowing us to turn
  // good, healthy RFC 3261 Via headers into twisted, perverted RFC 2543 Via
  // headers.
  Self.RemoveParameter(BranchParam);
end;

//* TIdSipViaHeader Protected methods ******************************************

function TIdSipViaHeader.GetName: String;
begin
  Result := ViaHeaderFull;
end;

function TIdSipViaHeader.GetValue: String;
begin
  Result := Self.SipVersion + '/' + Self.Transport
          + ' ' + Self.HostAndPort.Value;

end;

procedure TIdSipViaHeader.Parse(const Value: String);
var
  Token: String;
  S:     String;
begin
  inherited Parse(Value);

  Self.AssertBranchWellFormed;
  Self.AssertReceivedWellFormed;
  Self.AssertMaddrWellFormed;
  Self.AssertTTLWellFormed;

  S := Value;
  S := Fetch(S, ';', false);

  Token := Trim(Fetch(S, '/')) + '/';
  Token := Token + Trim(Fetch(S, '/'));
  Self.SipVersion := Token;

  S := Trim(S);
  Token := Trim(Fetch(S, ' '));
  if not TIdSipParser.IsTransport(Token) then
    Self.FailParse(InvalidSentProtocol);

  Self.Transport := Token;

  Token := Trim(Fetch(S, ';'));

  Self.HostAndPort.Value := Token;
end;

//* TIdSipViaHeader Private methods ********************************************

procedure TIdSipViaHeader.AssertBranchWellFormed;
begin
  if (Self.IndexOfParam(BranchParam) > -1)
     and not TIdSipParser.IsToken(Self.Params[BranchParam]) then
    Self.FailParse(InvalidBranchId);
end;

procedure TIdSipViaHeader.AssertMaddrWellFormed;
begin
  if (Self.Parameters.IndexOfName(MaddrParam) > -1) then begin
    if    not TIdSipParser.IsFQDN(Self.Parameters.Values[MaddrParam])
      and not TIdIPAddressParser.IsIPv4Address(Self.Parameters.Values[MaddrParam])
      and not TIdSipParser.IsIPv6Reference(Self.Parameters.Values[MaddrParam]) then
      Self.FailParse(InvalidMaddr);
  end;
end;

procedure TIdSipViaHeader.AssertReceivedWellFormed;
begin
  if (Self.IndexOfParam(ReceivedParam) > -1)
    and not TIdIPAddressParser.IsIPv4Address(Self.Params[ReceivedParam])
    and not TIdIPAddressParser.IsIPv6Address(Self.Params[ReceivedParam]) then
    Self.FailParse(InvalidReceived);
end;

procedure TIdSipViaHeader.AssertTTLWellFormed;
begin
  if (Self.Parameters.IndexOfName(TTLParam) > -1) then begin
    if not TIdSipParser.IsByte(Self.Parameters.Values[TTLParam]) then
      Self.FailParse(InvalidNumber);
  end;
end;

function TIdSipViaHeader.GetBranch: String;
begin
  if Self.HasParam(BranchParam) then
    Result := Self.Params[BranchParam]
  else
    Result := '';
end;

function TIdSipViaHeader.GetMaddr: String;
begin
  if Self.HasParam(MaddrParam) then
    Result := Self.Params[MaddrParam]
  else
    Result := '';
end;

function TIdSipViaHeader.GetPort: Cardinal;
begin
  Result := Self.HostAndPort.Port;
end;

function TIdSipViaHeader.GetReceived: String;
begin
  if Self.HasParam(ReceivedParam) then
    Result := Self.Params[ReceivedParam]
  else
    Result := '';
end;

function TIdSipViaHeader.GetRport: Cardinal;
begin
  if Self.HasParam(RportParam) then
    Result := StrToIntDef(Self.Params[RPortParam], 0)
  else
    Result := 0;
end;

function TIdSipViaHeader.GetSentBy: String;
begin
  Result := Self.HostAndPort.Host;
end;

function TIdSipViaHeader.GetTTL: Byte;
begin
  if Self.HasParam(TTLParam) then
    Result := StrToInt(Self.Params[TTLParam])
  else
    Result := 0;
end;

procedure TIdSipViaHeader.SetBranch(const Value: String);
begin
  Self.Params[BranchParam] := Value;

  Self.AssertBranchWellFormed;
end;

procedure TIdSipViaHeader.SetMaddr(const Value: String);
begin
  Self.Params[MaddrParam] := Value;

  Self.AssertMaddrWellFormed;
end;

procedure TIdSipViaHeader.SetPort(Value: Cardinal);
begin
  Self.HostAndPort.Port := Value;
end;

procedure TIdSipViaHeader.SetReceived(const Value: String);
begin
  Self.Params[ReceivedParam] := Value;

  Self.AssertReceivedWellFormed;
end;

procedure TIdSipViaHeader.SetRport(Value: Cardinal);
begin
  Self.Params[RportParam] := IntToStr(Value);
end;

procedure TIdSipViaHeader.SetSentBy(const Value: String);
begin
  Self.HostAndPort.Host := Value;
end;

procedure TIdSipViaHeader.SetTransport(const Value: String);
begin
  Self.fTransport := Value;

  Self.HostAndPort.DefaultPort := Self.DefaultPortForTransport(Value)
end;

procedure TIdSipViaHeader.SetTTL(Value: Byte);
begin
  Self.Params[TTLParam] := IntToStr(Value);
end;

//******************************************************************************
//* TIdSipWarningHeader                                                        *
//******************************************************************************

class function TIdSipWarningHeader.IsHostPort(Token: String): Boolean;
var
  HP: TIdSipHostAndPort;
begin
  try
    HP := TIdSipHostAndPort.Create;
    try
      HP.Value := Token;
    finally
      HP.Free;
    end;

    Result := TIdIPAddressParser.IsIPv4Address(HP.Host)
           or TIdIPAddressParser.IsIPv6Address(HP.Host)
  except
    on EBadHeader do
      Result := false;
    on EConvertError do
      Result := false;
  end;
end;

constructor TIdSipWarningHeader.Create;
begin
  inherited Create;

  Self.HostAndPort := TIdSipHostAndPort.Create;
end;

destructor TIdSipWarningHeader.Destroy;
begin
  Self.HostAndPort.Free;

  inherited Destroy;
end;

//* TIdSipWarningHeader Protected methods **************************************

function TIdSipWarningHeader.GetName: String;
begin
  Result := WarningHeader;
end;

function TIdSipWarningHeader.GetValue: String;
begin
  Result := Format('%d %s "%s"', [Self.Code, Self.Agent, Self.EncodeQuotedStr(Self.Text)]);
end;

procedure TIdSipWarningHeader.Parse(const Value: String);
var
  S: String;
  Token: String;
begin
  // Warning        =  "Warning" HCOLON warning-value *(COMMA warning-value)
  // warning-value  =  warn-code SP warn-agent SP warn-text
  // warn-code      =  3DIGIT
  // warn-agent     =  hostport / pseudonym
  //                   ;  the name or pseudonym of the server adding
  //                   ;  the Warning header, for use in debugging
  // warn-text      =  quoted-string
  // pseudonym      =  token

  S := Value;

  // warning-code
  Token := Fetch(S, ' ');
  if not TIdSipParser.IsNumber(Token) or (Length(Token) <> 3) then
    Self.FailParse(InvalidWarnCode);

  try
    Self.Code := StrToInt(Token);
  except
    on EConvertError do
      Self.FailParse(InvalidNumber);
    on ERangeError do
      Self.FailParse(InvalidNumber);
    else raise;
  end;

  // warn-agent
  Token := Fetch(S, ' ');
  Self.HostAndPort.Value := Token;

  // warn-text
  if not TIdSipParser.IsQuotedString(S) then
    Self.FailParse(InvalidWarnText);

  DecodeQuotedStr(Copy(S, 2, Length(S) - 2), S);
  Self.Text := S;
end;

//* TIdSipWarningHeader Private methods **************************************&&

function TIdSipWarningHeader.GetAgent: String;
begin
  Result := Self.HostAndPort.Value;
end;

procedure TIdSipWarningHeader.SetAgent(const Value: String);
begin
  Self.HostAndPort.Value := Value;
end;

//******************************************************************************
//* TIdSipWWWAuthenticateHeader                                                *
//******************************************************************************

//* TIdSipWWWAuthenticateHeader Protected methods ******************************

function TIdSipWWWAuthenticateHeader.GetName: String;
begin
  Result := WWWAuthenticateHeader;
end;

//******************************************************************************
//* TIdSipHeaderMap                                                            *
//******************************************************************************
//* TIdSipHeaderMap Public methods *********************************************

constructor TIdSipHeaderMap.Create(HeaderName: String; HeaderClass: TIdSipHeaderClass);
begin
  inherited Create;

  fHeaderName := HeaderName;
  fHeaderClass  := HeaderClass;
end;

//******************************************************************************
//* TIdSipHeaderList                                                           *
//******************************************************************************
//* TIdSipHeaderList Public methods ********************************************

class function TIdSipHeaderList.CanonicaliseName(HeaderName: String): String;
begin
  Result := '';

  if not Assigned(GCanonicalHeaderNames) then begin
    GCanonicalHeaderNames := TStringList.Create;
    GCanonicalHeaderNames.Add(AcceptHeader               + '=' + AcceptHeader);
    GCanonicalHeaderNames.Add(AcceptEncodingHeader       + '=' + AcceptEncodingHeader);
    GCanonicalHeaderNames.Add(AcceptLanguageHeader       + '=' + AcceptLanguageHeader);
    GCanonicalHeaderNames.Add(AlertInfoHeader            + '=' + AlertInfoHeader);
    GCanonicalHeaderNames.Add(AllowHeader                + '=' + AllowHeader);
    GCanonicalHeaderNames.Add(AuthenticationInfoHeader   + '=' + AuthenticationInfoHeader);
    GCanonicalHeaderNames.Add(AuthorizationHeader        + '=' + AuthorizationHeader);
    GCanonicalHeaderNames.Add(CallIDHeaderFull           + '=' + CallIDHeaderFull);
    GCanonicalHeaderNames.Add(CallIDHeaderShort          + '=' + CallIDHeaderFull);
    GCanonicalHeaderNames.Add(CallInfoHeader             + '=' + CallInfoHeader);
    GCanonicalHeaderNames.Add(ContactHeaderFull          + '=' + ContactHeaderFull);
    GCanonicalHeaderNames.Add(ContactHeaderShort         + '=' + ContactHeaderFull);
    GCanonicalHeaderNames.Add(ContentDispositionHeader   + '=' + ContentDispositionHeader);
    GCanonicalHeaderNames.Add(ContentEncodingHeaderFull  + '=' + ContentEncodingHeaderFull);
    GCanonicalHeaderNames.Add(ContentEncodingHeaderShort + '=' + ContentEncodingHeaderFull);
    GCanonicalHeaderNames.Add(ContentLanguageHeader      + '=' + ContentLanguageHeader);
    GCanonicalHeaderNames.Add(ContentLengthHeaderFull    + '=' + ContentLengthHeaderFull);
    GCanonicalHeaderNames.Add(ContentLengthHeaderShort   + '=' + ContentLengthHeaderFull);
    GCanonicalHeaderNames.Add(ContentTypeHeaderFull      + '=' + ContentTypeHeaderFull);
    GCanonicalHeaderNames.Add(ContentTypeHeaderShort     + '=' + ContentTypeHeaderFull);
    GCanonicalHeaderNames.Add(CSeqHeader                 + '=' + CSeqHeader);
    GCanonicalHeaderNames.Add(DateHeader                 + '=' + DateHeader);
    GCanonicalHeaderNames.Add(ErrorInfoHeader            + '=' + ErrorInfoHeader);
    GCanonicalHeaderNames.Add(ExpiresHeader              + '=' + ExpiresHeader);
    GCanonicalHeaderNames.Add(FromHeaderFull             + '=' + FromHeaderFull);
    GCanonicalHeaderNames.Add(FromHeaderShort            + '=' + FromHeaderFull);
    GCanonicalHeaderNames.Add(InReplyToHeader            + '=' + InReplyToHeader);
    GCanonicalHeaderNames.Add(MaxForwardsHeader          + '=' + MaxForwardsHeader);
    GCanonicalHeaderNames.Add(MIMEVersionHeader          + '=' + MIMEVersionHeader);
    GCanonicalHeaderNames.Add(MinExpiresHeader           + '=' + MinExpiresHeader);
    GCanonicalHeaderNames.Add(OrganizationHeader         + '=' + OrganizationHeader);
    GCanonicalHeaderNames.Add(PriorityHeader             + '=' + PriorityHeader);
    GCanonicalHeaderNames.Add(ProxyAuthenticateHeader    + '=' + ProxyAuthenticateHeader);
    GCanonicalHeaderNames.Add(ProxyAuthorizationHeader   + '=' + ProxyAuthorizationHeader);
    GCanonicalHeaderNames.Add(ProxyRequireHeader         + '=' + ProxyRequireHeader);
    GCanonicalHeaderNames.Add(RecordRouteHeader          + '=' + RecordRouteHeader);
    GCanonicalHeaderNames.Add(ReplyToHeader              + '=' + ReplyToHeader);
    GCanonicalHeaderNames.Add(RequireHeader              + '=' + RequireHeader);
    GCanonicalHeaderNames.Add(RetryAfterHeader           + '=' + RetryAfterHeader);
    GCanonicalHeaderNames.Add(RouteHeader                + '=' + RouteHeader);
    GCanonicalHeaderNames.Add(ServerHeader               + '=' + ServerHeader);
    GCanonicalHeaderNames.Add(SubjectHeaderFull          + '=' + SubjectHeaderFull);
    GCanonicalHeaderNames.Add(SubjectHeaderShort         + '=' + SubjectHeaderFull);
    GCanonicalHeaderNames.Add(SupportedHeaderFull        + '=' + SupportedHeaderFull);
    GCanonicalHeaderNames.Add(SupportedHeaderShort       + '=' + SupportedHeaderFull);
    GCanonicalHeaderNames.Add(TimestampHeader            + '=' + TimestampHeader);
    GCanonicalHeaderNames.Add(ToHeaderFull               + '=' + ToHeaderFull);
    GCanonicalHeaderNames.Add(ToHeaderShort              + '=' + ToHeaderFull);
    GCanonicalHeaderNames.Add(UnsupportedHeader          + '=' + UnsupportedHeader);
    GCanonicalHeaderNames.Add(UserAgentHeader            + '=' + UserAgentHeader);
    GCanonicalHeaderNames.Add(ViaHeaderFull              + '=' + ViaHeaderFull);
    GCanonicalHeaderNames.Add(ViaHeaderShort             + '=' + ViaHeaderFull);
    GCanonicalHeaderNames.Add(WarningHeader              + '=' + WarningHeader);
    GCanonicalHeaderNames.Add(WWWAuthenticateHeader      + '=' + WWWAuthenticateHeader);
  end;

  Result := GCanonicalHeaderNames.Values[HeaderName];

  if (Result = '') then begin
      Result := HeaderName;
  end;
end;

class function TIdSipHeaderList.GetHeaderName(Header: String): String;
begin
  Result := Trim(Fetch(Header, ':'));
end;

procedure TIdSipHeaderList.AddInReverseOrder(Headers: TIdSipHeaderList);
var
  I: Integer;
begin
  for I := Headers.Count - 1 downto 0 do
    Self.Add(Headers.Items[I]);
end;

function TIdSipHeaderList.AsString: String;
begin
  Result := '';
  Self.First;

  while Self.HasNext do begin
    Result := Result + Self.CurrentHeader.AsString + EOL;
    Self.Next;
  end;
end;

function TIdSipHeaderList.FirstMalformedHeader: TIdSipHeader;
begin
  Result := nil;
  
  Self.First;
  while Self.HasNext and not Assigned(Result) do begin
    if Self.CurrentHeader.IsMalformed then
      Result := Self.CurrentHeader;

    Self.Next;
  end;
end;

function TIdSipHeaderList.HasEqualValues(const OtherHeaders: TIdSipHeaderList): Boolean;
begin
  Result := Self.Count = OtherHeaders.Count;

  Self.First;
  OtherHeaders.First;
  if Result then begin
    while Result and Self.HasNext do begin
      Result := Result
            and (Self.CurrentHeader.Value = OtherHeaders.CurrentHeader.Value)
            and (Self.CurrentHeader.ParamsAsString = OtherHeaders.CurrentHeader.ParamsAsString);
      Self.Next;
      OtherHeaders.Next;
    end;
  end;
end;

function TIdSipHeaderList.IsMalformed: Boolean;
begin
  Result := Self.FirstMalformedHeader <> nil;
end;

function TIdSipHeaderList.Equals(OtherHeaders: TIdSipHeaderList): Boolean;
  procedure DumpHeaders(Headers: TIdSipHeaderList; List: TStringList);
  begin
    Headers.First;
    while Headers.HasNext do begin
      List.Add(Headers.CurrentHeader.AsString);
      Headers.Next;
    end;

    List.Sort;
  end;
var
  I:            Integer;
  OurHeaders:   TStringList;
  TheirHeaders: TStringList;
begin
  // In brief: TStringLists allow handy sorting of strings. We dump our
  // headers in one TStringList and their headers in another, sort the
  // lists and compare them. First though we try short-circuit the
  // comparison.

  Result := Self.Count = OtherHeaders.Count;

  if Result then begin
    OurHeaders := TStringList.Create;
    try
      TheirHeaders := TStringList.Create;
      try
        DumpHeaders(Self, OurHeaders);
        DumpHeaders(OtherHeaders, TheirHeaders);

        for I := 0 to OurHeaders.Count - 1 do
          Result := (OurHeaders[I] = TheirHeaders[I]);
      finally
        TheirHeaders.Free;
      end;
    finally
      OurHeaders.Free;
    end;
  end;
end;

function TIdSipHeaderList.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

//* TIdSipHeaderList Protected methods *****************************************

class function TIdSipHeaderList.HeaderTypes: TObjectList;
begin
  if not Assigned(GIdSipHeadersMap) then begin
    GIdSipHeadersMap := TObjectList.Create(true);

    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(AcceptHeader,               TIdSipWeightedCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(AcceptEncodingHeader,       TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(AlertInfoHeader,            TIdSipUriHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(AllowHeader,                TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(AuthenticationInfoHeader,   TIdSipAuthenticationInfoHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(AuthorizationHeader,        TIdSipAuthorizationHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(CallIDHeaderFull,           TIdSipCallIDHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(CallIDHeaderShort,          TIdSipCallIDHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContactHeaderFull,          TIdSipContactHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContactHeaderShort,         TIdSipContactHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContentDispositionHeader,   TIdSipContentDispositionHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContentEncodingHeaderFull,  TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContentEncodingHeaderShort, TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContentLanguageHeader,      TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContentLengthHeaderFull,    TIdSipNumericHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(CSeqHeader,                 TIdSipCSeqHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(DateHeader,                 TIdSipDateHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ErrorInfoHeader,            TIdSipUriHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ExpiresHeader,              TIdSipNumericHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(FromHeaderFull,             TIdSipFromHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(FromHeaderShort,            TIdSipFromHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(InReplyToHeader,            TIdSipCallIdHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(MaxForwardsHeader,          TIdSipMaxForwardsHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(MinExpiresHeader,           TIdSipNumericHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ProxyAuthenticateHeader,    TIdSipProxyAuthenticateHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ProxyAuthorizationHeader,   TIdSipProxyAuthorizationHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ProxyRequireHeader,         TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(RecordRouteHeader,          TIdSipRecordRouteHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(RequireHeader,              TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(RetryAfterHeader,           TIdSipRetryAfterHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(RouteHeader,                TIdSipRouteHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(SupportedHeaderFull,        TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(SupportedHeaderShort,       TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(TimestampHeader,            TIdSipTimestampHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ToHeaderFull,               TIdSipToHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ToHeaderShort,              TIdSipToHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(UnsupportedHeader,          TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ViaHeaderFull,              TIdSipViaHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ViaHeaderShort,             TIdSipViaHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(WarningHeader,              TIdSipWarningHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(WWWAuthenticateHeader,      TIdSipWWWAuthenticateHeader));
  end;

  Result := GIdSipHeadersMap;
end;

class function TIdSipHeaderList.IsHeader(const Header,
                                         ExpectedHeaderName: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Self.CanonicaliseName(Name), ExpectedHeaderName);
end;

function TIdSipHeaderList.ConstructHeader(HeaderName: String): TIdSipHeader;
var
  I: Integer;
begin
  HeaderName := Self.CanonicaliseName(HeaderName);

  Result := nil;
  I := 0;
  while (I < Self.HeaderTypes.Count) and not Assigned(Result) do
    if Self.IsHeader(TIdSipHeaderMap(Self.HeaderTypes[I]).HeaderName, HeaderName) then
      Result := TIdSipHeaderMap(Self.HeaderTypes[I]).HeaderClass.Create
    else
      Inc(I);

  if not Assigned(Result) then
    Result := TIdSipHeader.Create;
end;

//******************************************************************************
//* TIdSipHeadersFilter                                                        *
//******************************************************************************
//* TIdSipHeadersFilter Public methods *****************************************

constructor TIdSipHeadersFilter.Create(Headers: TIdSipHeaders;
                                       const HeaderName: String);
begin
  Self.fHeaderName := HeaderName;
  Self.Headers     := Headers;
end;

function TIdSipHeadersFilter.Add(const HeaderName: String): TIdSipHeader;
begin
  Result := Self.Headers.Add(HeaderName);
end;

procedure TIdSipHeadersFilter.Add(Copy: TIdSipHeader);
begin
  Self.Headers.Add(Copy);
end;

procedure TIdSipHeadersFilter.Add(Headers: TIdSipHeaderList);
begin
  Self.Headers.Add(Headers);
end;

procedure TIdSipHeadersFilter.Clear;
begin
  Self.Headers.RemoveAll(Self.HeaderName);
end;

function TIdSipHeadersFilter.Count: Integer;
begin
  Result := 0;
  Self.Headers.First;
  while Self.Headers.HasNext do begin
    if (Self.Headers.CurrentHeader.Name = Self.HeaderName) then
      Inc(Result);

    Self.Headers.Next;
  end;
end;

function TIdSipHeadersFilter.CurrentHeader: TIdSipHeader;
begin
  if Self.IsEmpty or (Self.CurrentIndex >= Self.Count) then
    Result := nil
  else
    Result := Self.GetItems(Self.CurrentIndex);
end;

procedure TIdSipHeadersFilter.First;
begin
  Self.CurrentIndex := 0;
end;

function TIdSipHeadersFilter.HasNext: Boolean;
begin
  Result := Self.CurrentIndex < Self.Count;
end;

procedure TIdSipHeadersFilter.Next;
begin
  Inc(Self.CurrentIndex);
end;

procedure TIdSipHeadersFilter.Remove(Header: TIdSipHeader);
begin
  Self.Headers.Remove(Header);
end;

procedure TIdSipHeadersFilter.RemoveAll;
begin
  Self.Headers.RemoveAll(Self.HeaderName);
end;

//* TIdSipHeadersFilter Protected methods **************************************

function TIdSipHeadersFilter.GetItems(Index: Integer): TIdSipHeader;
var
  ItemCount: Integer;
begin
  Result    := nil;
  ItemCount := -1;

  Self.Headers.First;
  while Self.Headers.HasNext and not Assigned(Result) do begin
    if (Self.Headers.CurrentHeader.Name = Self.HeaderName) then
      Inc(ItemCount);

    if (ItemCount = Index) then
      Result := Self.Headers.CurrentHeader;

    Self.Headers.Next;
  end;
end;

//******************************************************************************
//* TIdSipHeaders                                                              *
//******************************************************************************
//* TIdSipHeaders Public methods ***********************************************

class function TIdSipHeaders.IsCallID(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, CallIDHeaderFull);
end;

class function TIdSipHeaders.IsCompoundHeader(const Header: String): Boolean;
begin
  Result := Self.IsContact(Header)
         or Self.IsErrorInfo(Header)
         or Self.IsRecordRoute(Header)
         or Self.IsRoute(Header)
         or Self.IsVia(Header)
         or Self.IsWarning(Header);
end;

class function TIdSipHeaders.IsContact(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, ContactHeaderFull);
end;

class function TIdSipHeaders.IsContentLength(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, ContentLengthHeaderFull);
end;

class function TIdSipHeaders.IsCSeq(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, CseqHeader);
end;

class function TIdSipHeaders.IsErrorInfo(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, ErrorInfoHeader);
end;

class function TIdSipHeaders.IsFrom(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, FromHeaderFull);
end;

class function TIdSipHeaders.IsMaxForwards(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, MaxForwardsHeader);
end;

class function TIdSipHeaders.IsRecordRoute(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, RecordRouteHeader);
end;

class function TIdSipHeaders.IsRoute(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, RouteHeader);
end;

class function TIdSipHeaders.IsTo(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, ToHeaderFull);
end;

class function TIdSipHeaders.IsVia(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, ViaHeaderFull);
end;

class function TIdSipHeaders.IsWarning(const Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, WarningHeader);
end;

constructor TIdSipHeaders.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdSipHeaders.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

function TIdSipHeaders.Add(const HeaderName: String): TIdSipHeader;
begin
  Result := Self.ConstructHeader(HeaderName);
  try
    Self.List.Add(Result);
    Result.Name := HeaderName;
  except
    if (Self.List.IndexOf(Result) <> -1) then begin
      Self.List.Remove(Result);
      Result := nil;
    end
    else
      FreeAndNil(Result);

    raise;
  end;
end;

procedure TIdSipHeaders.Add(Copy: TIdSipHeader);
var
  H: TIdSipHeader;
begin
  H := Self.Add(Copy.Name);

  if Assigned(H) then
    H.Assign(Copy);
end;

procedure TIdSipHeaders.Add(Headers: TIdSipHeaderList);
begin
  Headers.First;
  while Headers.HasNext do begin
    Self.Add(Headers.CurrentHeader);
    Headers.Next;
  end;
end;

procedure TIdSipHeaders.Clear;
begin
  Self.List.Clear;
end;

function TIdSipHeaders.CurrentHeader: TIdSipHeader;
begin
  if Self.IsEmpty or (Self.CurrentIndex >= Self.Count) then
    Result := nil
  else
    Result := Self.List[Self.CurrentIndex] as TIdSipHeader;
end;

procedure TIdSipHeaders.Delete(I: Integer);
begin
  Self.List.Delete(I);
end;

function TIdSipHeaders.Count: Integer;
begin
  Result := Self.List.Count;
end;

procedure TIdSipHeaders.First;
begin
  Self.CurrentIndex := 0;
end;

function TIdSipHeaders.GetAllButFirst: TIdSipHeaderList;
begin
  Result := TIdSipHeaders.Create;
  try
    Self.First;

    if Self.HasNext then
      Self.Next;

    while Self.HasNext do begin
      (Result as TIdSipHeaders).Add(Self.CurrentHeader);
      Self.Next;
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipHeaders.HasHeader(const HeaderName: String): Boolean;
begin
  Result := Assigned(Self.FindFirst(HeaderName));
end;

function TIdSipHeaders.HasNext: Boolean;
begin
  Result := Self.CurrentIndex < Self.Count;
end;

procedure TIdSipHeaders.Next;
begin
  Inc(Self.CurrentIndex);
end;

procedure TIdSipHeaders.Remove(Header: TIdSipHeader);
begin
  Self.List.Remove(Header);
end;

procedure TIdSipHeaders.RemoveAll(const HeaderName: String);
var
  I: Integer;
  H: TIdSipHeader;
begin
  I := 0;
  while (I < Self.List.Count) do begin
    H := Self.GetItems(I);
    if (IsEqual(H.Name, HeaderName)) then
      Self.Remove(H)
    else
      Inc(I);
  end;
end;

//* TIdSipHeaders Protected methods ********************************************

function TIdSipHeaders.GetItems(I: Integer): TIdSipHeader;
begin
  Result := Self.List[I] as TIdSipHeader;
end;

//* TIdSipHeaders Private methods **********************************************

function TIdSipHeaders.FindFirst(const HeaderName: String): TIdSipHeader;
var
  I: Integer;
begin
  Result := nil;

  I := 0;
  while (I < Self.List.Count) and not Assigned(Result) do
    if IsEqual((Self.List[I] as TIdSipHeader).Name, HeaderName) then
      Result := Self.List[I] as TIdSipHeader
    else
      Inc(I);
end;

function TIdSipHeaders.GetHeaders(const Name: String): TIdSipHeader;
begin
  Result := Self.FindFirst(Name);

  if not Assigned(Result) then
    Result := Self.Add(Name);
end;

//******************************************************************************
//* TIdSipAuthorizations                                                       *
//******************************************************************************
//* TIdSipAuthorizations Public methods ****************************************

constructor TIdSipAuthorizations.Create(Headers: TIdSipHeaders);
begin
  inherited Create(Headers, AuthorizationHeader);
end;

constructor TIdSipAuthorizations.Create;
begin
  Self.BlankHeaders := TIdSipHeaders.Create;
  inherited Create(Self.BlankHeaders, AuthorizationHeader);
end;

destructor TIdSipAuthorizations.Destroy;
begin
  Self.BlankHeaders.Free;

  inherited Destroy;
end;

function TIdSipAuthorizations.CurrentAuthorization: TIdSipAuthorizationHeader;
begin
  Result := Self.CurrentHeader as TIdSipAuthorizationHeader;
end;

//******************************************************************************
//* TIdSipContacts                                                             *
//******************************************************************************
//* TIdSipContacts Public methods **********************************************

constructor TIdSipContacts.Create(Headers: TIdSipHeaders);
begin
  inherited Create(Headers, ContactHeaderFull);
end;

constructor TIdSipContacts.Create;
begin
  Self.BlankHeaders := TIdSipHeaders.Create;
  inherited Create(Self.BlankHeaders, ContactHeaderFull);
end;

destructor TIdSipContacts.Destroy;
begin
  Self.BlankHeaders.Free;

  inherited Destroy;
end;

function TIdSipContacts.ContactFor(Address: TIdSipAddressHeader): TIdSipContactHeader;
begin
  Result := nil;
  Self.First;

  while Self.HasNext and not Assigned(Result) do begin
    if Self.CurrentContact.Address.Equals(Address.Address) then
      Result := Self.CurrentContact;

    Self.Next;
  end;
end;

function TIdSipContacts.CurrentContact: TIdSipContactHeader;
begin
  Result := Self.CurrentHeader as TIdSipContactHeader;
end;

function TIdSipContacts.HasContact(Address: TIdSipAddressHeader): Boolean;
begin
  Result := false;
  Self.First;

  while Self.HasNext and not Result do begin
    Result := Self.CurrentContact.Address.Equals(Address.Address);

    Self.Next;
  end;
end;

//******************************************************************************
//* TIdSipExpiresHeaders
//******************************************************************************
//* TIdSipExpiresHeaders Public methods ****************************************

constructor TIdSipExpiresHeaders.Create(Headers: TIdSipHeaders);
begin
  inherited Create(Headers, ExpiresHeader);
end;

function TIdSipExpiresHeaders.CurrentExpires: Cardinal;
var
  Header: TIdSipHeader;
begin
  Header := Self.CurrentHeader;


  if not Assigned(Header) then
    Result := 0
  else
    Result := (Header as TIdSipNumericHeader).NumericValue;
end;

//******************************************************************************
//* TIdSipProxyAuthorizations                                                  *
//******************************************************************************
//* TIdSipProxyAuthorizations Public methods ***********************************

constructor TIdSipProxyAuthorizations.Create(Headers: TIdSipHeaders);
begin
  inherited Create(Headers, ProxyAuthorizationHeader);
end;

constructor TIdSipProxyAuthorizations.Create;
begin
  Self.BlankHeaders := TIdSipHeaders.Create;
  inherited Create(Self.BlankHeaders, ProxyAuthorizationHeader);
end;

destructor TIdSipProxyAuthorizations.Destroy;
begin
  Self.BlankHeaders.Free;

  inherited Destroy;
end;

function TIdSipProxyAuthorizations.CurrentProxyAuthorization: TIdSipProxyAuthorizationHeader;
begin
  Result := Self.CurrentHeader as TIdSipProxyAuthorizationHeader;
end;

//******************************************************************************
//* TIdSipRecordRoutePath                                                      *
//******************************************************************************
//* TIdSipRecordRoutePath Public methods ***************************************

constructor TIdSipRecordRoutePath.Create(Headers: TIdSipHeaders);
begin
  inherited Create(Headers, RecordRouteHeader);
end;

constructor TIdSipRecordRoutePath.Create;
begin
  Self.BlankHeaders := TIdSipHeaders.Create;
  inherited Create(Self.BlankHeaders, RecordRouteHeader);
end;

destructor TIdSipRecordRoutePath.Destroy;
begin
  Self.BlankHeaders.Free;

  inherited Destroy;
end;

function TIdSipRecordRoutePath.CurrentRecordRoute: TIdSipRecordRouteHeader;
begin
  Result := Self.CurrentHeader as TIdSipRecordRouteHeader;
end;

//******************************************************************************
//* TIdSipRoutePath                                                            *
//******************************************************************************
//* TIdSipRoutePath Public methods *********************************************

constructor TIdSipRoutePath.Create(Headers: TIdSipHeaders);
begin
  inherited Create(Headers, RouteHeader);
end;

constructor TIdSipRoutePath.Create;
begin
  Self.BlankHeaders := TIdSipHeaders.Create;
  inherited Create(Self.BlankHeaders, RouteHeader);
end;

destructor TIdSipRoutePath.Destroy;
begin
  Self.BlankHeaders.Free;

  inherited Destroy;
end;

procedure TIdSipRoutePath.AddRoute(RouteUri: TIdSipUri);
var
  NewRoute: TIdSipRouteHeader;
begin
  NewRoute := RouteUri.AsRouteHeader;
  try
    Self.Add(NewRoute);
  finally
    NewRoute.Free;
  end;
end;

function TIdSipRoutePath.CurrentRoute: TIdSipRouteHeader;
begin
  Result := Self.CurrentHeader as TIdSipRouteHeader;
end;

function TIdSipRoutePath.GetAllButFirst: TIdSipRoutePath;
begin
  Result := TIdSipRoutePath.Create;
  try
    Self.First;

    if Self.HasNext then
      Self.Next;

    while Self.HasNext do begin
      Result.Add(Self.CurrentHeader);
      Self.Next;
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

//******************************************************************************
//* TIdSipViaPath                                                              *
//******************************************************************************
//* TIdSipViaPath Public methods ***********************************************

constructor TIdSipViaPath.Create(Headers: TIdSipHeaders);
begin
  inherited Create(Headers, ViaHeaderFull);
end;

function TIdSipViaPath.LastHop: TIdSipViaHeader;
begin
  Self.First;
  Result := Self.CurrentHeader as TIdSipViaHeader;
end;

function TIdSipViaPath.Length: Integer;
begin
  Result := Self.Count;
end;

procedure TIdSipViaPath.RemoveLastHop;
begin
  Self.Remove(Self.LastHop);
end;

//******************************************************************************
//* TIdSipMessage                                                              *
//******************************************************************************
//* TIdSipMessage Public methods ***********************************************

class function TIdSipMessage.MessageType(const FirstLine: String): TIdSipMessageClass;
begin
  if TIdSipParser.IsRequest(FirstLine) then
    Result := TIdSipRequest
  else
    Result := TIdSipResponse;
end;

class function TIdSipMessage.ReadMessageFrom(const RawData: String): TIdSipMessage;
var
  S: TStream;
begin
  S := TStringStream.Create(RawData);
  try
    Result := Self.ReadMessageFrom(S);
    Result.ReadBody(S);
  finally
    S.Free;
  end;
end;

class function TIdSipMessage.ReadMessageFrom(RawData: TStream): TIdSipMessage;
var
  Parser: TIdSipParser;
begin
  Parser := TIdSipParser.Create;
  try
    Parser.Source := RawData;

    // chew up leading blank lines (RFC 3261, section 7.5)
    Parser.SkipBlankLines;

    Result := Self.CreateAndReadMessageFrom(RawData,
                                            Self.MessageType(Parser.PeekLine));
  finally
    Parser.Free;
  end;
end;

class function TIdSipMessage.ReadRequestFrom(const RawData: String): TIdSipRequest;
var
  S: TStream;
begin
  S := TStringStream.Create(RawData);
  try
    Result := Self.CreateAndReadMessageFrom(S, TIdSipRequest) as TIdSipRequest;
    Result.ReadBody(S);
  finally
    S.Free;
  end;
end;

class function TIdSipMessage.ReadResponseFrom(const RawData: String): TIdSipResponse;
var
  S: TStream;
begin
  S := TStringStream.Create(RawData);
  try
    Result := Self.CreateAndReadMessageFrom(S, TIdSipResponse) as TIdSipResponse;
    Result.ReadBody(S);
  finally
    S.Free;
  end;
end;

class function TIdSipMessage.WillEstablishDialog(Request: TIdSipRequest;
                                                 Response: TIdSipResponse): Boolean;
begin
  Result := Request.IsInvite and Response.IsOK;
end;

constructor TIdSipMessage.Create;
begin
  inherited Create;

  fIsMalformed := false;

  fHeaders := TIdSipHeaders.Create;

  fContacts    := TIdSipContacts.Create(Self.Headers);
  fPath        := TIdSipViaPath.Create(Self.Headers);
  fRecordRoute := TIdSipRecordRoutePath.Create(Self.Headers);

  Self.fParseFailReason := '';
  Self.SIPVersion       := IdSipMessage.SIPVersion;
end;

destructor TIdSipMessage.Destroy;
begin
  Self.RecordRoute.Free;
  Self.Path.Free;
  Self.Contacts.Free;
  Self.Headers.Free;

  inherited Destroy;
end;

procedure TIdSipMessage.Accept(Visitor: IIdSipMessageVisitor);
begin
end;

function TIdSipMessage.AddHeader(const HeaderName: String): TIdSipHeader;
begin
  Result := Self.Headers.Add(HeaderName);
end;

procedure TIdSipMessage.AddHeader(Copy: TIdSipHeader);
begin
  Self.Headers.Add(Copy);
end;

procedure TIdSipMessage.AddHeaders(Headers: TIdSipHeaderList);
begin
  Self.Headers.Add(Headers);
end;

procedure TIdSipMessage.Assign(Src: TPersistent);
var
  S: TIdSipMessage;
begin
  if (Src is TIdSipMessage) then begin
    S := Src as TIdSipMessage;

    Self.SIPVersion := S.SIPVersion;

    Self.ClearHeaders;
    Self.AddHeaders(S.Headers);
    
    Self.Body := S.Body;
  end
  else
    inherited Assign(Src);
end;

function TIdSipMessage.AsString: String;
begin
  Result := Self.FirstLine;

  Result := Result + Self.Headers.AsString;

  Result := Result + EOL;
  Result := Result + Self.Body;
end;

procedure TIdSipMessage.ClearHeaders;
begin
  Self.Headers.Clear;
end;

function TIdSipMessage.ContactCount: Cardinal;
var
  Contacts: TIdSipContacts;
begin
  Contacts := TIdSipContacts.Create(Self.Headers);
  try
    Result := Contacts.Count;
  finally
    Contacts.Free;
  end;
end;

function TIdSipMessage.Copy: TIdSipMessage;
begin
  Result := TIdSipMessageClass(Self.ClassType).Create;
  Result.Assign(Self);
end;

procedure TIdSipMessage.CopyHeaders(Src: TIdSipMessage;
                                    const HeaderName: String);
var
  Filter: TIdSipHeadersFilter;
begin
  Filter := TIdSipHeadersFilter.Create(Src.Headers,
                                       HeaderName);
  try
    Self.AddHeaders(Filter);
  finally
    Filter.Free;
  end;
end;

function TIdSipMessage.FirstContact: TIdSipContactHeader;
begin
  Result := Self.FirstHeader(ContactHeaderFull) as TIdSipContactHeader;
end;

function TIdSipMessage.FirstExpires: TIdSipNumericHeader;
begin
  Result := Self.FirstHeader(ExpiresHeader) as TIdSipNumericHeader;
end;

function TIdSipMessage.FirstHeader(const HeaderName: String): TIdSipHeader;
begin
  Result := Self.Headers[HeaderName];
end;

function TIdSipMessage.FirstMinExpires: TIdSipNumericHeader;
begin
  Result := Self.FirstHeader(MinExpiresHeader) as TIdSipNumericHeader;
end;

function TIdSipMessage.FirstRequire: TIdSipCommaSeparatedHeader;
begin
  Result := Self.FirstHeader(RequireHeader) as TIdSipCommaSeparatedHeader;
end;

function TIdSipMessage.FirstRetryAfter: TIdSipRetryAfterHeader;
begin
  Result := Self.FirstHeader(RetryAfterHeader) as TIdSipRetryAfterHeader;
end;

function TIdSipMessage.HasBody: Boolean;
begin
  Result := Self.Body <> '';
end;

function TIdSipMessage.HasExpiry: Boolean;
var
  Contacts: TIdSipContacts;
begin
  Result := Self.HasHeader(ExpiresHeader);

  if not Result then begin
    Contacts := TIdSipContacts.Create(Self.Headers);
    try
      Contacts.First;
      while Contacts.HasNext and not Result do begin
        Result := Result or Contacts.CurrentHeader.HasParam(ExpiresParam);
        Contacts.Next;
      end;
    finally
      Contacts.Free;
    end;
  end;
end;

function TIdSipMessage.HasHeader(const HeaderName: String): Boolean;
begin
  Result := Self.Headers.HasHeader(HeaderName);
end;

function TIdSipMessage.InSameDialogAs(Msg: TIdSipMessage): Boolean;
begin
  Result := (Self.CallID = Msg.CallID)
        and (Self.From.Tag = Msg.From.Tag)
        and (Self.ToHeader.Tag = Msg.ToHeader.Tag);
end;

function TIdSipMessage.IsMalformed: Boolean;
begin
  Self.fParseFailReason := '';

  Result := Self.HasMalformedFirstLine
    or Self.MissingRequiredHeaders
    or Self.HasMalformedHeaders
    or not Self.ContentLengthEqualsBodyLength
    or Self.HasBodyButMissingContentType;
end;

function TIdSipMessage.HeaderCount: Integer;
begin
  Result := Self.Headers.Count;
end;

function TIdSipMessage.QuickestExpiry: Cardinal;
begin
  if not Self.HasExpiry then
    Result := 0
  else begin
    Result := Self.Minimum(Self.QuickestContactExpiry,
                           Self.QuickestExpiresHeader)
  end;
end;

function TIdSipMessage.LastHop: TIdSipViaHeader;
begin
  Result := Self.Path.LastHop;
end;

procedure TIdSipMessage.MarkAsInvalid(const Reason: String);
begin
  Self.fIsMalformed     := true;
  Self.fParseFailReason := Reason;
end;

function TIdSipMessage.ParseFailReason: String;
var
  H: TIdSipHeader;
begin
  if not Self.IsMalformed then begin
    Result := '';
    Exit;
  end;
  
  Result := Self.fParseFailReason;

  if (Result = '') then begin
    H := Self.FirstMalformedHeader;

    if Assigned(H) then
      Result := Format(MalformedToken, [H.Name, H.UnparsedValue])
    else
      Result := RSSIPBadRequest;
  end;
end;

procedure TIdSipMessage.Parse(Parser: TIdSipParser);
begin
  Self.Initialize;

  Self.fRawMessage := StreamToStr(Parser.Source);

  if not Parser.Eof then begin
    Self.ParseFirstLine(Parser);
    Self.ParseHeaders(Parser);
  end;
end;

procedure TIdSipMessage.ReadBody(Src: TStream);
const
  BufLen = 100;
var
  Buf:            array[1..BufLen] of Char;
  BytesToRead:    Integer;
  Read:           Integer;
  RemainingBytes: Integer;
begin
  // The transport must set Content-Length before this method gets called!

  if (Self.ContentLength > 0) then begin
    BytesToRead := Self.ContentLength;

    repeat
      Read := Src.Read(Buf, Min(BufLen, BytesToRead));
      Dec(BytesToRead, Read);

      Self.Body := Self.Body + System.Copy(Buf, 1, Read);
    until (Read < BufLen) or (BytesToRead <= 0);

    if (Read < Self.ContentLength) then begin
      RemainingBytes := Self.ContentLength - Read;
      Self.MarkAsInvalid(Format(UnexpectedMessageLength,
                               [RemainingBytes, Self.ContentLength]));
     end;
  end;
end;

procedure TIdSipMessage.RemoveHeader(Header: TIdSipHeader);
begin
  Self.Headers.Remove(Header);
end;

procedure TIdSipMessage.RemoveAllHeadersNamed(const Name: String);
begin
  Self.Headers.RemoveAll(Name);
end;

//* TIdSipMessage Protected methods ********************************************

procedure TIdSipMessage.FailParse(const Reason: String);
begin
  Self.MarkAsInvalid(Reason);

  raise Self.MalformedException.Create(Reason, Self.RawMessage);
end;

function TIdSipMessage.HasMalformedHeaders: Boolean;
begin
  Result := Self.Headers.IsMalformed;

  if Result and (Self.fParseFailReason = '') then
    Self.fParseFailReason := '';
end;

function TIdSipMessage.HasMalformedFirstLine: Boolean;
begin
  // By default behave laxly
  Result := false;
end;

function TIdSipMessage.MatchRequest(InitialRequest: TIdSipRequest;
                                    UseCSeqMethod: Boolean): Boolean;
begin
  // We should never enter this clause since matching messages to transactions
  // only happens in the Transaction layer, and the Transport layer should
  // (must!) reject malformed messages (like messages with no Via headers).
  // Still, better safe than sorry.
  if Self.Path.IsEmpty or InitialRequest.Path.IsEmpty then begin
    Result := false;
    Exit;
  end;

  // cf. RFC 3261 section 17.2.3
  if Self.LastHop.IsRFC3261Branch then
    Result := Self.MatchRFC3261Request(InitialRequest, UseCSeqMethod)
  else
    Result := Self.MatchRFC2543Request(InitialRequest, UseCSeqMethod);
end;

function TIdSipMessage.MissingRequiredHeaders: Boolean;
begin
  Result := true;

  if not Self.HasHeader(CallIDHeaderFull) then begin
    Self.MarkAsInvalid(MissingCallID);
    Exit;
  end;

  if not Self.HasHeader(CSeqHeader) then begin
    Self.MarkAsInvalid(MissingCSeq);
    Exit;
  end;

  if not Self.HasHeader(FromHeaderFull) then begin
    Self.MarkAsInvalid(MissingFrom);
    Exit;
  end;

  if not Self.HasHeader(ToHeaderFull) then begin
    Self.MarkAsInvalid(MissingTo);
    Exit;
  end;

  if not Self.HasHeader(ViaHeaderFull) then begin
    Self.MarkAsInvalid(MissingVia);
    Exit;
  end;

  Result := false;
end;

procedure TIdSipMessage.ParseCompoundHeader(const Header: String;
                                            Parms: String);
begin
  while (Parms <> '') do
    Self.AddHeader(Header).Value := Fetch(Parms, ',');
end;

procedure TIdSipMessage.ParseFirstLine(Parser: TIdSipParser);
begin
  Self.fRawFirstLine := Parser.PeekLine;
  try
    Self.ParseStartLine(Parser);
  except
    on EBadMessage do;
  end;
end;

procedure TIdSipMessage.ParseHeader(Parser: TIdSipParser;
                                    const RawHeader: String);
var
  Name:  String;
  Value: String;
begin
  Name := Parser.GetHeaderName(RawHeader);
  Name := TIdSipHeaders.CanonicaliseName(Name);

  Value := Parser.GetHeaderValue(RawHeader);

  if TIdSipHeaders.IsCompoundHeader(RawHeader) then
    Self.ParseCompoundHeader(Name,
                             Value)
  else begin
    Self.AddHeader(Name).Value := Trim(Value);
  end;
end;

procedure TIdSipMessage.ParseHeaders(Parser: TIdSipParser);
var
  FoldedHeader: String;
  Line:         String;
begin
  FoldedHeader := Parser.ReadLn;
  if (FoldedHeader <> '') then begin
    Line := Parser.ReadLn;
    while (Line <> '') do begin
      if (Line[1] in [' ', #9]) then begin
        FoldedHeader := FoldedHeader + ' ' + Trim(Line);
        Line := Parser.ReadLn;
      end
      else begin
        Self.ParseHeader(Parser, FoldedHeader);
        FoldedHeader := Line;
        Line := Parser.ReadLn;
      end;
    end;
    if (FoldedHeader <> '') then
      Self.ParseHeader(Parser, FoldedHeader);
  end;
end;

//* TIdSipMessage Private methods **********************************************

class function TIdSipMessage.CreateAndReadMessageFrom(RawData: TStream;
                                                      ClassType: TIdSipMessageClass): TIdSipMessage;
var
  Parser: TIdSipParser;
begin
  Parser := TIdSipParser.Create;
  try
    Parser.Source := RawData;

    Parser.SkipBlankLines;

    Result := ClassType.Create;

    Result.Parse(Parser);
  finally
    Parser.Free;
  end;
end;

function TIdSipMessage.ContentLengthEqualsBodyLength: Boolean;
var
  CL: Integer;
begin
  if Self.HasHeader(ContentLengthHeaderFull) then
    CL := Self.ContentLength
  else
    CL := 0;

  Result := CL = Length(Self.Body);

  if not Result then
    Self.MarkAsInvalid(BadContentLength);
end;

function TIdSipMessage.FirstHeaderValue(const HeaderName: String): String;
begin
  if Self.HasHeader(HeaderName) then
    Result := Self.FirstHeader(HeaderName).Value
  else
    Result := '';
end;

function TIdSipMessage.FirstMalformedHeader: TIdSipHeader;
begin
  Result := nil;

  Self.Headers.First;
  while not Assigned(Result) and Self.Headers.HasNext do begin
    if Self.Headers.CurrentHeader.IsMalformed then
      Result := Self.Headers.CurrentHeader;
    Self.Headers.Next;
  end;
end;

function TIdSipMessage.GetCallID: String;
begin
  Result := Self.FirstHeaderValue(CallIDHeaderFull);
end;

function TIdSipMessage.GetContentDisposition: TIdSipContentDispositionHeader;
begin
  Result := Self.FirstHeader(ContentDispositionHeader) as TIdSipContentDispositionHeader;
end;

function TIdSipMessage.GetContentLanguage: String;
begin
  Result := Self.FirstHeaderValue(ContentLanguageHeader);
end;

function TIdSipMessage.GetContentLength: Integer;
begin
  Result := StrToIntDef(Self.FirstHeaderValue(ContentLengthHeaderFull), 0);
end;

function TIdSipMessage.GetContentType: String;
begin
  Result := Self.FirstHeaderValue(ContentTypeHeaderFull);
end;

function TIdSipMessage.GetCSeq: TIdSipCSeqHeader;
begin
  Result := Self.FirstHeader(CSeqHeader) as TIdSipCSeqHeader;
end;

function TIdSipMessage.GetFrom: TIdSipFromHeader;
begin
  Result := Self.FirstHeader(FromHeaderFull) as TIdSipFromHeader;
end;

function TIdSipMessage.GetTo: TIdSipToHeader;
begin
  Result := Self.FirstHeader(ToHeaderFull) as TIdSipToHeader;
end;

function TIdSipMessage.HasBodyButMissingContentType: Boolean;
begin
  Result := (Length(Self.Body) > 0)
    and not Self.HasHeader(ContentTypeHeaderFull);

  if Result then
    Self.MarkAsInvalid(MissingContentType);
end;

procedure TIdSipMessage.Initialize;
begin
  Self.ClearHeaders;
  Self.SipVersion := '';
end;

function TIdSipMessage.Minimum(A, B: Cardinal): Cardinal;
begin
  // We can't use Min() because that takes two Integers.
  if (A < B) then
    Result := A
  else
    Result := B;
end;

function TIdSipMessage.QuickestContactExpiry: Cardinal;
var
  Contacts: TIdSipContacts;
begin
  Result := High(Result);
  Contacts := TIdSipContacts.Create(Self.Headers);
  try
    if not Contacts.IsEmpty then begin
      Contacts.First;

      while Contacts.HasNext do begin
        if Contacts.CurrentContact.WillExpire then
          Result := Self.Minimum(Result, Contacts.CurrentContact.Expires);
        Contacts.Next;
      end;
    end;
  finally
    Contacts.Free;
  end;
end;

function TIdSipMessage.QuickestExpiresHeader: Cardinal;
var
  Expires: TIdSipExpiresHeaders;
begin
  Result := High(Result);
  Expires := TIdSipExpiresHeaders.Create(Self.Headers);
  try
    if not Expires.IsEmpty then begin
      Expires.First;

      while Expires.HasNext do begin
        Result := Self.Minimum(Result, Expires.CurrentExpires);
        Expires.Next;
      end;
    end;
  finally
    Expires.Free;
  end;
end;

procedure TIdSipMessage.SetCallID(const Value: String);
begin
  Self.FirstHeader(CallIDHeaderFull).Value := Value;
end;

procedure TIdSipMessage.SetContacts(Value: TIdSipContacts);
begin
  Self.Contacts.Clear;
  Self.Contacts.Add(Value);
end;

procedure TIdSipMessage.SetContentDisposition(Value: TIdSipContentDispositionHeader);
begin
  Self.ContentDisposition.Assign(Value);
end;

procedure TIdSipMessage.SetContentLanguage(const Value: String);
begin
  Self.FirstHeader(ContentLanguageHeader).Value := Value;
end;

procedure TIdSipMessage.SetContentLength(Value: Integer);
begin
  Self.FirstHeader(ContentLengthHeaderFull).Value := IntToStr(Value);
end;

procedure TIdSipMessage.SetContentType(const Value: String);
begin
  Self.FirstHeader(ContentTypeHeaderFull).Value := Value;
end;

procedure TIdSipMessage.SetCSeq(Value: TIdSipCSeqHeader);
begin
  Self.CSeq.Assign(Value);
end;

procedure TIdSipMessage.SetFrom(Value: TIdSipFromHeader);
begin
  Self.FirstHeader(FromHeaderFull).Assign(Value);
end;

procedure TIdSipMessage.SetPath(Value: TIdSipViaPath);
begin
  Self.Path.Clear;
  Self.Path.Add(Value);
end;

procedure TIdSipMessage.SetRecordRoute(Value: TIdSipRecordRoutePath);
begin
  Self.RecordRoute.Clear;
  Self.RecordRoute.Add(Value);
end;

procedure TIdSipMessage.SetTo(Value: TIdSipToHeader);
begin
  Self.FirstHeader(ToHeaderFull).Assign(Value);
end;

//*******************************************************************************
//* TIdSipRequest                                                               *
//*******************************************************************************
//* TIdSipRequest Public methods ************************************************

constructor TIdSipRequest.Create;
begin
  inherited Create;

  Self.fRequestUri := TIdSipURI.Create('');
  Self.fRoute      := TIdSipRoutePath.Create(Self.Headers);

  Self.ContentLength := 0;
  Self.MaxForwards   := Self.DefaultMaxForwards;
end;

destructor TIdSipRequest.Destroy;
begin
  Self.Route.Free;
  Self.RequestUri.Free;

  inherited Destroy;
end;

procedure TIdSipRequest.Accept(Visitor: IIdSipMessageVisitor);
begin
  Visitor.VisitRequest(Self);
end;

function TIdSipRequest.AckFor(Response: TIdSipResponse): TIdSipRequest;
var
  Dlg: TIdSipDialog;
begin
  if Response.WillEstablishDialog(Self) then begin
    Dlg := TIdSipDialog.CreateOutboundDialog(Self,
                                             Response,
                                             Self.RequestUri.IsSecure);
    try
      Result := Dlg.CreateAck;
    finally
      Dlg.Free;
    end;
  end
  else begin
    Result := TIdSipRequest.Create;
    try
      Result.SIPVersion  := Self.SIPVersion;

      Result.CallID          := Self.CallID;
      Result.CSeq.Method     := MethodAck;
      Result.CSeq.SequenceNo := Response.CSeq.SequenceNo;
      Result.From            := Self.From;
      Result.MaxForwards     := Result.DefaultMaxForwards;
      Result.Method          := Result.CSeq.Method;
      Result.RequestUri      := Self.RequestUri;
      Result.ToHeader        := Response.ToHeader;

      Result.AddHeaders(Self.Route);

      Result.Path.Add(Self.LastHop);
      Result.ContentLength := 0;
      Result.Body          := '';

      if Self.HasHeader(AuthorizationHeader) then
        Result.AddHeader(AuthorizationHeader).Value := Self.FirstHeader(AuthorizationHeader).FullValue;
      if Self.HasHeader(ProxyAuthorizationHeader) then
        Result.AddHeader(ProxyAuthorizationHeader).Value := Self.FirstHeader(ProxyAuthorizationHeader).FullValue;
    except
      FreeAndNil(Result);

      raise;
    end;
  end;
end;

function TIdSipRequest.AddressOfRecord: String;
begin
  Result := Self.ToHeader.AsAddressOfRecord;
end;

procedure TIdSipRequest.Assign(Src: TPersistent);
var
  R: TIdSipRequest;
begin
  inherited Assign(Src);

  R := Src as TIdSipRequest;

  Self.Method     := R.Method;
  Self.RequestUri := R.RequestUri;
end;

function TIdSipRequest.AuthorizationFor(const Realm: String): TIdSipAuthorizationHeader;
begin
  Result := Self.FindAuthorizationHeader(Realm,
                                         AuthorizationHeader) as TIdSipAuthorizationHeader;
end;

function TIdSipRequest.CreateCancel: TIdSipRequest;
begin
  Assert(Self.IsInvite, 'Only INVITE requests may be CANCELled');
  Result := TIdSipRequest.Create;
  try
    Result.Method      := MethodCancel;
    Result.CSeq.Method := Result.Method;

    Result.RequestUri.Uri  := Self.RequestUri.Uri;
    Result.CallID          := Self.CallID;
    Result.ToHeader        := Self.ToHeader;
    Result.CSeq.SequenceNo := Self.CSeq.SequenceNo;
    Result.From            := Self.From;

    Result.Path.Add(Self.LastHop);

    Result.Route := Self.Route;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipRequest.DefaultMaxForwards: Cardinal;
begin
  Result := 70;
end;

function TIdSipRequest.DestinationUri: String;
begin
  // Use the Result of this function to know what URI you can use to determine
  // possible target machines. cf RFC 3261 section 8.1.2.

  if Self.HasRoute and Self.FirstRoute.IsLooseRoutable then
    Result := Self.FirstRoute.Address.AsString
  else
    Result := Self.RequestUri.AsString;
end;

function TIdSipRequest.FirstAuthorization: TIdSipAuthorizationHeader;
begin
  Result := Self.FirstHeader(AuthorizationHeader) as TIdSipAuthorizationHeader
end;

function TIdSipRequest.FirstProxyAuthorization: TIdSipProxyAuthorizationHeader;
begin
  Result := Self.FirstHeader(ProxyAuthorizationHeader) as TIdSipProxyAuthorizationHeader
end;

function TIdSipRequest.FirstProxyRequire: TIdSipCommaSeparatedHeader;
begin
  Result := Self.FirstHeader(ProxyRequireHeader) as TIdSipCommaSeparatedHeader;
end;

function TIdSipRequest.FirstRoute: TIdSipRouteHeader;
begin
  Result := Self.FirstHeader(RouteHeader) as TIdSipRouteHeader
end;

function TIdSipRequest.HasAuthorization: Boolean;
begin
  Result := Self.HasHeader(AuthorizationHeader);
end;

function TIdSipRequest.HasAuthorizationFor(const Realm: String): Boolean;
var
  AuthHeaders: TIdSipAuthorizations;
begin
  AuthHeaders := TIdSipAuthorizations.Create(Self.Headers);
  try
    AuthHeaders.First;

    Result := false;
    while AuthHeaders.HasNext and not Result do
      if IsEqual(AuthHeaders.CurrentAuthorization.Realm, Realm) then
        Result := true
      else
        AuthHeaders.Next;
  finally
    AuthHeaders.Free;
  end;
end;

function TIdSipRequest.HasProxyAuthorization: Boolean;
begin
  Result := Self.HasHeader(ProxyAuthorizationHeader);
end;

function TIdSipRequest.HasProxyAuthorizationFor(const Realm: String): Boolean;
var
  ProxyAuthHeaders: TIdSipProxyAuthorizations;
begin
  ProxyAuthHeaders := TIdSipProxyAuthorizations.Create(Self.Headers);
  try
    ProxyAuthHeaders.First;

    Result := false;
    while ProxyAuthHeaders.HasNext and not Result do
      if IsEqual(ProxyAuthHeaders.CurrentProxyAuthorization.Realm, Realm) then
        Result := true
      else
        ProxyAuthHeaders.Next;
  finally
    ProxyAuthHeaders.Free;
  end;
end;

function TIdSipRequest.HasRoute: Boolean;
begin
  Result := Self.HasHeader(RouteHeader);
end;

function TIdSipRequest.HasSipsUri: Boolean;
var
  S: String;
begin
  S := Self.RequestUri.URI;
  Result := Lowercase(Fetch(S, ':')) = SipsScheme;
end;

function TIdSipRequest.IsAck: Boolean;
begin
  Result := Self.Method = MethodAck;
end;

function TIdSipRequest.IsBye: Boolean;
begin
  Result := Self.Method = MethodBye;
end;

function TIdSipRequest.IsCancel: Boolean;
begin
  Result := Self.Method = MethodCancel;
end;

function TIdSipRequest.Equals(Msg: TIdSipMessage): Boolean;
var
  Request: TIdSipRequest;
begin
  if (Msg is Self.ClassType) then begin
    Request := Msg as TIdSipRequest;

    Result := (Self.SIPVersion     = Request.SIPVersion)
          and (Self.Method         = Request.Method)
          and (Self.RequestUri.URI = Request.RequestUri.URI)
          and (Self.Headers.Equals(Request.Headers));
  end
  else
    Result := false;
end;

function TIdSipRequest.IsInvite: Boolean;
begin
  Result := Self.Method = MethodInvite;
end;

function TIdSipRequest.IsMalformed: Boolean;
begin
  Result := inherited IsMalformed
            or not Self.CSeqMatchesMethod;
end;

function TIdSipRequest.IsOptions: Boolean;
begin
  Result := Self.Method = MethodOptions;
end;

function TIdSipRequest.IsRegister: Boolean;
begin
  Result := Self.Method = MethodRegister;
end;

function TIdSipRequest.IsRequest: Boolean;
begin
  Result := true;
end;

function TIdSipRequest.MalformedException: EBadMessageClass;
begin
  Result := EBadRequest;
end;

function TIdSipRequest.Match(Msg: TIdSipMessage): Boolean;
begin
  // If Self represents an initial request for a transaction, does Msg belong
  // to the same transaction?

  Result := Msg.MatchRequest(Self, true);
end;

function TIdSipRequest.MatchCancel(Cancel: TIdSipRequest): Boolean;
begin
  Result := Cancel.MatchRequest(Self, false);
end;

function TIdSipRequest.ProxyAuthorizationFor(const Realm: String): TIdSipProxyAuthorizationHeader;
begin
  Result := Self.FindAuthorizationHeader(Realm,
                                         ProxyAuthorizationHeader) as TIdSipProxyAuthorizationHeader;
end;

function TIdSipRequest.RequiresResponse: Boolean;
begin
  Result := not Self.IsAck;
end;

//* TIdSipRequest Protected methods ********************************************

function TIdSipRequest.FirstLine: String;
begin
  Result := Format(RequestLine,
                   [Self.Method, Self.RequestUri.URI, Self.SIPVersion]);
end;

function TIdSipRequest.HasMalformedFirstLine: Boolean;
var
  RawMethod: String;
  RawURI:    String;
begin
  Result := true;

  if (Self.RawFirstLine <> '') then begin
    RawURI    := Self.RawFirstLine;
    RawMethod := Fetch(RawURI, ' ');
    RawURI    := Fetch(RawURI, ' ');
  end
  else begin
    RawMethod := Self.Method;
    RawUri    := Self.RequestUri.AsString; // Yes, it's a NOP
  end;

  if not TIdSipParser.IsMethod(RawMethod) then begin
    Self.MarkAsInvalid(Format(MalformedToken, [MethodToken, RawMethod]));
    Exit;
  end;

  if (RawUri = '') then begin
    Self.MarkAsInvalid(RequestUriNoSpaces);
    Exit;
  end
  else begin
    if (RawURI[1] = '<') and (RawURI[Length(RawURI)] = '>') then begin
      Self.MarkAsInvalid(RequestUriNoAngleBrackets);
      Exit;
    end;
  end;

  if not TIdSipParser.IsSipVersion(Self.SIPVersion) then begin
    Self.MarkAsInvalid(Format(InvalidSipVersion, [Self.SIPVersion]));
    Exit;
  end;

  Result := false;
end;

function TIdSipRequest.MatchRFC2543Request(InitialRequest: TIdSipRequest;
                                           UseCSeqMethod: Boolean): Boolean;
begin
  // NOTE BENE:
  // This DOES NOT consistitute a full match! If Self.IsAck then we also
  // need to check the To tag against the response the server sent. We
  // cannot do that here, which means that only a(n INVITE) transaction can
  // do the full match.
  if Self.IsInvite then
    Result := Self.RequestUri.Equals(InitialRequest.RequestUri)
         and (Self.ToHeader.Tag = InitialRequest.ToHeader.Tag)
         and (Self.From.Tag = InitialRequest.From.Tag)
         and (Self.CallID = InitialRequest.CallID)
         and  Self.LastHop.Equals(InitialRequest.LastHop)
  else if Self.IsAck then
    Result := RequestUri.Equals(InitialRequest.RequestUri)
         and (Self.From.Tag = InitialRequest.From.Tag)
         and (Self.CallID = InitialRequest.CallID)
         and (Self.CSeq.SequenceNo = InitialRequest.CSeq.SequenceNo)
         and  Self.LastHop.Equals(InitialRequest.LastHop)
  else
    Result := Self.RequestUri.EqualParameters(InitialRequest.RequestUri)
         and (Self.ToHeader.Tag = InitialRequest.ToHeader.Tag)
         and (Self.From.Tag = InitialRequest.From.Tag)
         and (Self.CallID = InitialRequest.CallID)
         and  Self.LastHop.Equals(InitialRequest.LastHop);

  if not Self.IsAck then begin
    if UseCSeqMethod then
      Result := Result
            and Self.CSeq.Equals(InitialRequest.CSeq)
    else
      Result := Result
            and (Self.CSeq.SequenceNo = InitialRequest.CSeq.SequenceNo);
  end;
end;

function TIdSipRequest.MatchRFC3261Request(InitialRequest: TIdSipRequest;
                                           UseCSeqMethod: Boolean): Boolean;
begin
  Result := (Self.LastHop.Branch = InitialRequest.LastHop.Branch)
             and (Self.LastHop.SentBy = InitialRequest.LastHop.SentBy);

  if UseCseqMethod then begin
    if Self.IsACK then
      Result := Result and InitialRequest.IsInvite
    else
      Result := Result and (Self.Method = InitialRequest.Method);
  end;
end;

function TIdSipRequest.MissingRequiredHeaders: Boolean;
begin
  Result := inherited MissingRequiredHeaders;


  if not Self.HasHeader(MaxForwardsHeader) then begin
    Result := true;
    Self.MarkAsInvalid(MissingMaxForwards);
    Exit;
  end;
end;

procedure TIdSipRequest.ParseStartLine(Parser: TIdSipParser);
var
  Line:   String;
  Tokens: TStrings;
  URI:    String;
begin
  // chew up leading blank lines (Section 7.5)
  Line := Parser.ReadFirstNonBlankLine;

  Tokens := TStringList.Create;
  try
    BreakApart(Line, ' ', Tokens);

    if (Tokens.Count > 3) then
      Self.FailParse(RequestUriNoSpaces)
    else if (Tokens.Count < 3) then
      Self.FailParse(Format(MalformedToken, ['Request-Line', Line]));

    Self.Method := Tokens[0];
    // we want to check the Method
    if not Parser.IsMethod(Self.Method) then
      Self.FailParse(Format(MalformedToken, [MethodToken, Self.Method]));

    URI := Tokens[1];

    // cf RFC 3261 section 7.1 
    if (URI <> '') and (URI[1] = '<') and (URI[Length(URI)] = '>') then
      Self.FailParse(RequestUriNoAngleBrackets);

    Self.RequestUri.URI := URI;

    Self.SIPVersion := Tokens[2];

    if not Parser.IsSipVersion(Self.SIPVersion) then
      Self.FailParse(Format(InvalidSipVersion, [Self.SIPVersion]));
  finally
    Tokens.Free;
  end;
end;

//* TIdSipRequest Private methods **********************************************

function TIdSipRequest.CSeqMatchesMethod: Boolean;
begin
  Result := Self.CSeq.Method = Self.Method;

  Self.MarkAsInvalid(CSeqMethodMismatch);
end;

function TIdSipRequest.FindAuthorizationHeader(const Realm: String;
                                               const HeaderType: String): TIdSipHeader;
var
  RealmString: String;
begin
  Result := nil;
  RealmString := RealmParam + '="' + Lowercase(Realm) + '"';

  Self.Headers.First;

  while Self.Headers.HasNext and not Assigned(Result) do
    if    IsEqual(Self.Headers.CurrentHeader.Name, HeaderType)
      and (IndyPos(RealmString, Lowercase(Self.Headers.CurrentHeader.Value)) > 0) then
      Result := Self.Headers.CurrentHeader as TIdSipAuthorizationHeader
    else
      Self.Headers.Next;
end;

function TIdSipRequest.GetMaxForwards: Byte;
begin
  if not Self.HasHeader(MaxForwardsHeader)
     or (Self.FirstHeader(MaxForwardsHeader).Value = '') then
    Self.MaxForwards := Self.DefaultMaxForwards;

  Result := StrToInt(Self.FirstHeader(MaxForwardsHeader).Value);
end;

procedure TIdSipRequest.SetMaxForwards(Value: Byte);
begin
  Self.FirstHeader(MaxForwardsHeader).Value := IntToStr(Value);
end;

procedure TIdSipRequest.SetRequestUri(Value: TIdSipURI);
begin
  Self.fRequestUri.URI := Value.URI
end;

procedure TIdSipRequest.SetRoute(Value: TIdSipRoutePath);
begin
  Self.Route.Clear;
  Self.Route.Add(Value);
end;

//*******************************************************************************
//* TIdSipResponse                                                              *
//*******************************************************************************
//* TIdSipResponse Public methods ***********************************************

class function TIdSipResponse.InResponseTo(Request: TIdSipRequest;
                                           StatusCode: Cardinal): TIdSipResponse;
var
  TimestampHeaders: TIdSipHeadersFilter;
begin
  Result := TIdSipResponse.Create;
  try
    Result.RequestRequestUri := Request.RequestUri;
    Result.SIPVersion        := IdSipMessage.SIPVersion;
    Result.StatusCode        := StatusCode;

    // cf RFC 3261 section 8.2.6.1
    if Result.IsTrying then begin
      TimestampHeaders := TIdSipHeadersFilter.Create(Request.Headers,
                                                     TimestampHeader);
      try
        Result.AddHeaders(TimestampHeaders);
      finally
        TimestampHeaders.Free;
      end;
    end;

    // cf RFC 3261 section 8.2.6.2

    if Request.HasHeader(ViaHeaderFull) then
      Result.Path         := Request.Path;
    if Request.HasHeader(CallIDHeaderFull) then
      Result.CallID       := Request.CallID;
    if Request.HasHeader(CSeqHeader) then
      Result.CSeq         := Request.CSeq;
    if Request.HasHeader(FromHeaderFull) then
      Result.From         := Request.From;
    if Request.HasHeader(ToHeaderFull) then
      Result.ToHeader     := Request.ToHeader;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

class function TIdSipResponse.InResponseTo(Request: TIdSipRequest;
                                           StatusCode: Cardinal;
                                           Contact: TIdSipContactHeader): TIdSipResponse;
var
  NewContact:       TIdSipContactHeader;
  FirstRR:          TIdSipRecordRouteHeader;
  ReqRecordRoutes:  TIdSipHeadersFilter;
begin
  Result := Self.InResponseTo(Request, StatusCode);

  if Result.WillEstablishDialog(Request) then begin
    // cf RFC 3261 section 12.1.1
    ReqRecordRoutes := TIdSipHeadersFilter.Create(Request.Headers,
                                                  RecordRouteHeader);
    try
      Result.AddHeaders(ReqRecordRoutes);

      NewContact := TIdSipContactHeader.Create;
      try
        NewContact.Assign(Contact);

        if not ReqRecordRoutes.IsEmpty then begin
          ReqRecordRoutes.First;

          FirstRR := ReqRecordRoutes.CurrentHeader as TIdSipRecordRouteHeader;
          if (FirstRR.Address.IsSecure) then
            NewContact.Address.Scheme := SipsScheme;
        end;

        if Request.HasSipsUri then
          NewContact.Address.Scheme := SipsScheme;

        Result.AddHeader(NewContact);
      finally
        NewContact.Free;
      end;
    finally
      ReqRecordRoutes.Free;
    end;
  end;
end;

constructor TIdSipResponse.Create;
begin
  inherited Create;

  Self.fRequestRequestUri := TIdSipUri.Create('');
end;

destructor TIdSipResponse.Destroy;
begin
  Self.RequestRequestUri.Free;

  inherited Destroy;
end;

procedure TIdSipResponse.Accept(Visitor: IIdSipMessageVisitor);
begin
  Visitor.VisitResponse(Self);
end;

procedure TIdSipResponse.Assign(Src: TPersistent);
var
  R: TIdSipResponse;
begin
  inherited Assign(Src);

  R := Src as TIdSipResponse;

  Self.RequestRequestUri := R.RequestRequestUri;
  Self.StatusCode        := R.StatusCode;
  Self.StatusText        := R.StatusText;
end;

function TIdSipResponse.AuthenticateHeader: TIdSipAuthenticateHeader;
begin
  if Self.HasProxyAuthenticate then
    Result := Self.FirstProxyAuthenticate
  else if Self.HasWWWAuthenticate then
    Result := Self.FirstWWWAuthenticate
  else
    Result := nil;
end;

function TIdSipResponse.Description: String;
begin
  Result := IntToStr(Self.StatusCode) + ' ' + Self.StatusText;
end;

function TIdSipResponse.FirstAuthenticationInfo: TIdSipAuthenticationInfoHeader;
begin
  Result := Self.FirstHeader(AuthenticationInfoHeader) as TIdSipAuthenticationInfoHeader;
end;

function TIdSipResponse.FirstProxyAuthenticate: TIdSipProxyAuthenticateHeader;
begin
  Result := Self.FirstHeader(ProxyAuthenticateHeader) as TIdSipProxyAuthenticateHeader;
end;

function TIdSipResponse.FirstUnsupported: TIdSipCommaSeparatedHeader;
begin
  Result := Self.FirstHeader(UnsupportedHeader) as TIdSipCommaSeparatedHeader;
end;

function TIdSipResponse.FirstWarning: TIdSipWarningHeader;
begin
  Result := Self.FirstHeader(WarningHeader) as TIdSipWarningHeader;
end;

function TIdSipResponse.FirstWWWAuthenticate: TIdSipWWWAuthenticateHeader;
begin
  Result := Self.FirstHeader(WWWAuthenticateHeader) as TIdSipWWWAuthenticateHeader;
end;

function TIdSipResponse.Equals(Msg: TIdSipMessage): Boolean;
var
  Response: TIdSipResponse;
begin
  if (Msg is Self.ClassType) then begin
    Response := Msg as TIdSipResponse;

    Result := (Self.SIPVersion = Response.SipVersion)
          and (Self.StatusCode = Response.StatusCode)
          and (Self.StatusText = Response.StatusText)
          and (Self.Headers.Equals(Response.Headers));
  end
  else
    Result := false;
end;

function TIdSipResponse.HasAuthenticationInfo: Boolean;
begin
  Result := Self.HasHeader(AuthenticationInfoHeader);
end;

function TIdSipResponse.HasProxyAuthenticate: Boolean;
begin
  Result := Self.HasHeader(ProxyAuthenticateHeader);
end;

function TIdSipResponse.HasWarning: Boolean;
begin
  Result := Self.HasHeader(WarningHeader);
end;

function TIdSipResponse.HasWWWAuthenticate: Boolean;
begin
  Result := Self.HasHeader(WWWAuthenticateHeader);
end;

function TIdSipResponse.IsAuthenticationChallenge: Boolean;
begin
  Result := (Self.StatusCode = SIPUnauthorized)
         or (Self.StatusCode = SIPProxyAuthenticationRequired);
end;

function TIdSipResponse.IsFinal: Boolean;
begin
  Result := Self.StatusCode div 100 > 1;
end;

function TIdSipResponse.IsOK: Boolean;
begin
  Result := Self.StatusCode div 100 = 2;
end;

function TIdSipResponse.IsProvisional: Boolean;
begin
  Result := Self.StatusCode div 100 = 1;
end;

function TIdSipResponse.IsRedirect: Boolean;
begin
  Result := Self.StatusCode div 100 = 3;
end;


function TIdSipResponse.IsRequest: Boolean;
begin
  Result := false;
end;

function TIdSipResponse.IsTrying: Boolean;
begin
  Result := Self.StatusCode = SIPTrying;
end;

function TIdSipResponse.MalformedException: EBadMessageClass;
begin
  Result := EBadResponse;
end;

function TIdSipResponse.WillEstablishDialog(Request: TIdSipRequest): Boolean;
begin
  Result := TIdSipResponse.WillEstablishDialog(Request, Self);
end;

//* TIdSipResponse Protected methods *******************************************

function TIdSipResponse.FirstLine: String;
begin
  Result := Format(StatusLine,
                   [Self.SIPVersion, Self.StatusCode, Self.StatusText]);
end;

function TIdSipResponse.HasMalformedFirstLine: Boolean;
var
  StatusCode: String;
begin
  Result := true;
  
  // We presume that RawFirstLine doesn't have weird spacing!
  if (Self.RawFirstLine <> '') then begin
    StatusCode := Self.RawFirstLine;
    Fetch(StatusCode, ' ');
    StatusCode := Fetch(StatusCode, ' ');
  end
  else
    StatusCode := IntToStr(Self.StatusCode);

  if not TIdSipParser.IsNumber(StatusCode)
    or (StrToInt(StatusCode) < 0)
    or (StrToInt(StatusCode) > 999) then begin
    Self.MarkAsInvalid(Format(InvalidStatusCode, [StatusCode]));
    Exit;
  end;

  Result := false;
end;

function TIdSipResponse.MatchRFC2543Request(InitialRequest: TIdSipRequest;
                                            UseCSeqMethod: Boolean): Boolean;
begin
  // From RFC 3261, section 17.2.3:
  //   "For all other request methods, a request is matched to a transaction
  //   if the Request-URI, To tag, From tag, Call-ID, CSeq (including the
  //   method), and top Via header field match those of the request that
  //   created the transaction.  Matching is done based on the matching
  //   rules defined for each of those header fields.  When a non-INVITE
  //   request matches an existing transaction, it is a retransmission of
  //   the request that created that transaction.
  //
  //   Because the matching rules include the Request-URI, the server cannot
  //   match a response to a transaction.  When the TU passes a response to
  //   the server transaction, it must pass it to the specific server
  //   transaction for which the response is targeted."
  //
  // The latter paragraph provides us with the reason why a Response has the
  // RequestRequestUri property: it allows us to match responses against server
  // transactions without passing Transaction objects all over the place!

  Result := Self.RequestRequestUri.Equals(InitialRequest.RequestUri)
        and (Self.ToHeader.Tag = InitialRequest.ToHeader.Tag)
        and (Self.From.Tag     = InitialRequest.From.Tag)
        and (Self.CallID       = InitialRequest.CallID)
        and Self.CSeq.Equals(InitialRequest.CSeq);

  Result := Result
        and not Self.Path.IsEmpty
        and not InitialRequest.Path.IsEmpty;

  Result := Result
        and Self.LastHop.Equals(InitialRequest.LastHop);
end;

function TIdSipResponse.MatchRFC3261Request(InitialRequest: TIdSipRequest;
                                            UseCSeqMethod: Boolean): Boolean;
begin
  // cf. RFC 3261 section 17.2.3
  Result := not Self.Path.IsEmpty
        and not InitialRequest.Path.IsEmpty;

  Result := Result
        and (Self.LastHop.Branch = InitialRequest.LastHop.Branch);

  if (Self.CSeq.Method = MethodAck) then
    Result := Result
          and (InitialRequest.IsInvite)
  else
    Result := Result
          and (Self.CSeq.Method = InitialRequest.Method);
end;

procedure TIdSipResponse.ParseStartLine(Parser: TIdSipParser);
var
  Line:       String;
  StatusCode: String;
begin
  // chew up leading blank lines (Section 7.5)
  Line := Parser.ReadFirstNonBlankLine;

  Self.SIPVersion := Fetch(Line);
  if not Parser.IsSipVersion(Self.SIPVersion) then
    Self.MarkAsInvalid(Format(InvalidSipVersion, [Self.SIPVersion]));

  StatusCode := Fetch(Line);
  if not Parser.IsNumber(StatusCode) then
    Self.MarkAsInvalid(Format(InvalidStatusCode, [StatusCode]));

  Self.StatusCode := StrToIntDef(StatusCode, BadStatusCode);

  Self.StatusText := Line;
end;

//* TIdSipResponse Private methods **********************************************

procedure TIdSipResponse.SetRequestRequestUri(Value: TIdSipUri);
begin
  Self.fRequestRequestUri.Uri := Value.Uri;
end;

procedure TIdSipResponse.SetStatusCode(Value: Integer);
begin
  Self.fStatusCode := Value;

  case Self.StatusCode of
    SIPTrying:                           Self.StatusText := RSSIPTrying;
    SIPRinging:                          Self.StatusText := RSSIPRinging;
    SIPCallIsBeingForwarded:             Self.StatusText := RSSIPCallIsBeingForwarded;
    SIPQueued:                           Self.StatusText := RSSIPQueued;
    SIPSessionProgress:                  Self.StatusText := RSSIPSessionProgress;
    SIPOK:                               Self.StatusText := RSSIPOK;
    SIPMultipleChoices:                  Self.StatusText := RSSIPMultipleChoices;
    SIPMovedPermanently:                 Self.StatusText := RSSIPMovedPermanently;
    SIPMovedTemporarily:                 Self.StatusText := RSSIPMovedTemporarily;
    SIPUseProxy:                         Self.StatusText := RSSIPUseProxy;
    SIPAlternativeService:               Self.StatusText := RSSIPAlternativeService;
    SIPBadRequest:                       Self.StatusText := RSSIPBadRequest;
    SIPUnauthorized:                     Self.StatusText := RSSIPUnauthorized;
    SIPPaymentRequired:                  Self.StatusText := RSSIPPaymentRequired;
    SIPForbidden:                        Self.StatusText := RSSIPForbidden;
    SIPNotFound:                         Self.StatusText := RSSIPNotFound;
    SIPMethodNotAllowed:                 Self.StatusText := RSSIPMethodNotAllowed;
    SIPNotAcceptableClient:              Self.StatusText := RSSIPNotAcceptableClient;
    SIPProxyAuthenticationRequired:      Self.StatusText := RSSIPProxyAuthenticationRequired;
    SIPRequestTimeout:                   Self.StatusText := RSSIPRequestTimeout;
    SIPGone:                             Self.StatusText := RSSIPGone;
    SIPRequestEntityTooLarge:            Self.StatusText := RSSIPRequestEntityTooLarge;
    SIPRequestURITooLarge:               Self.StatusText := RSSIPRequestURITooLarge;
    SIPUnsupportedMediaType:             Self.StatusText := RSSIPUnsupportedMediaType;
    SIPUnsupportedURIScheme:             Self.StatusText := RSSIPUnsupportedURIScheme;
    SIPBadExtension:                     Self.StatusText := RSSIPBadExtension;
    SIPExtensionRequired:                Self.StatusText := RSSIPExtensionRequired;
    SIPIntervalTooBrief:                 Self.StatusText := RSSIPIntervalTooBrief;
    SIPTemporarilyUnavailable:           Self.StatusText := RSSIPTemporarilyUnavailable;
    SIPCallLegOrTransactionDoesNotExist: Self.StatusText := RSSIPCallLegOrTransactionDoesNotExist;
    SIPLoopDetected:                     Self.StatusText := RSSIPLoopDetected;
    SIPTooManyHops:                      Self.StatusText := RSSIPTooManyHops;
    SIPAddressIncomplete:                Self.StatusText := RSSIPAddressIncomplete;
    SIPAmbiguous:                        Self.StatusText := RSSIPAmbiguous;
    SIPBusyHere:                         Self.StatusText := RSSIPBusyHere;
    SIPRequestTerminated:                Self.StatusText := RSSIPRequestTerminated;
    SIPNotAcceptableHere:                Self.StatusText := RSSIPNotAcceptableHere;
    SIPRequestPending:                   Self.StatusText := RSSIPRequestPending;
    SIPUndecipherable:                   Self.StatusText := RSSIPUndecipherable;
    SIPInternalServerError:              Self.StatusText := RSSIPInternalServerError;
    SIPNotImplemented:                   Self.StatusText := RSSIPNotImplemented;
    SIPBadGateway:                       Self.StatusText := RSSIPBadGateway;
    SIPServiceUnavailable:               Self.StatusText := RSSIPServiceUnavailable;
    SIPServerTimeOut:                    Self.StatusText := RSSIPServerTimeOut;
    SIPSIPVersionNotSupported:           Self.StatusText := RSSIPSIPVersionNotSupported;
    SIPMessageTooLarge:                  Self.StatusText := RSSIPMessageTooLarge;
    SIPBusyEverywhere:                   Self.StatusText := RSSIPBusyEverywhere;
    SIPDecline:                          Self.StatusText := RSSIPDecline;
    SIPDoesNotExistAnywhere:             Self.StatusText := RSSIPDoesNotExistAnywhere;
    SIPNotAcceptableGlobal:              Self.StatusText := RSSIPNotAcceptableGlobal;
  else
    Self.StatusText := RSSIPUnknownResponseCode;
  end;
end;

//******************************************************************************
//* TIdSipRequestList                                                          *
//******************************************************************************
//* TIdSipRequestList Public methods *******************************************

constructor TIdSipRequestList.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdSipRequestList.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdSipRequestList.AddCopy(Request: TIdSipRequest);
var
  Copy: TIdSipRequest;
begin
  Copy := TIdSipRequest.Create;
  try
    Copy.Assign(Request);
    Self.List.Add(Copy);
  except
    if (Self.List.IndexOf(Copy) <> -1) then
      Self.List.Remove(Copy)
    else
      Copy.Free;
  end;
end;

function TIdSipRequestList.Count: Integer;
begin
  Result := Self.List.Count;
end;

procedure TIdSipRequestList.Delete(Index: Integer);
begin
  Self.List.Delete(Index);
end;

function TIdSipRequestList.First: TIdSipRequest;
begin
  Result := Self.Items[0] as TIdSipRequest;
end;

function TIdSipRequestList.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

function TIdSipRequestList.Last: TIdSipRequest;
begin
  Result := Self.FromTheFront(0);
end;

function TIdSipRequestList.SecondLast: TIdSipRequest;
begin
  Result := Self.FromTheFront(1);
end;

function TIdSipRequestList.ThirdLast: TIdSipRequest;
begin
  Result := Self.FromTheFront(2);
end;

//* TIdSipRequestList Public methods *******************************************

function TIdSipRequestList.FromTheFront(Offset: Integer): TIdSipRequest;
begin
  Assert(Offset >= 0, OffsetMustBeNonNegative);

  Result := Self.Items[Self.List.Count - Offset - 1];
end;

function TIdSipRequestList.GetItems(Index: Integer): TIdSipRequest;
begin
  if (Index <0) or (Index >= Self.Count) then
    Result := nil
  else
    Result := Self.List[Index] as TidSipRequest;
end;

//******************************************************************************
//* TIdSipResponseList                                                         *
//******************************************************************************
//* TIdSipResponseList Public methods ******************************************

constructor TIdSipResponseList.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdSipResponseList.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdSipResponseList.AddCopy(Response: TIdSipResponse);
var
  Copy: TIdSipResponse;
begin
  Copy := TIdSipResponse.Create;
  try
    Copy.Assign(Response);
    Self.List.Add(Copy);
  except
    if (Self.List.IndexOf(Copy) <> -1) then
      Self.List.Remove(Copy)
    else
      Copy.Free;
  end;
end;

function TIdSipResponseList.Count: Integer;
begin
  Result := Self.List.Count;
end;

procedure TIdSipResponseList.Delete(Index: Integer);
begin
  Self.List.Delete(Index);
end;

function TIdSipResponseList.First: TIdSipResponse;
begin
  if Self.IsEmpty then
    Result := nil
  else
    Result := Self.List[0] as TIdSipResponse;
end;

function TIdSipResponseList.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

function TIdSipResponseList.Last: TIdSipResponse;
begin
  if Self.IsEmpty then
    Result := nil
  else
    Result := Self.List[Self.List.Count - 1] as TIdSipResponse;
end;

function TIdSipResponseList.SecondLast: TIdSipResponse;
begin
  if (Self.Count < 2) then
    Result := nil
  else
    Result := Self.List[Self.List.Count - 2] as TIdSipResponse;
end;

//******************************************************************************
//* TIdSipParser                                                               *
//******************************************************************************
//* TIdSipParser Public methods ************************************************

class function TIdSipParser.IsIPv6Reference(const Token: String): Boolean;
begin
  Result := (Copy(Token, 1, 1) = '[')
        and (Copy(Token, Length(Token), 1) = ']')
        and TIdIPAddressParser.IsIPv6Address(Copy(Token, 2, Length(Token) - 2));
end;

class function TIdSipParser.IsMethod(Method: String): Boolean;
begin
  Result := Self.IsToken(Method);
end;

class function TIdSipParser.IsQuotedString(const Token: String): Boolean;
var
  S: String;
begin
  Result := Token <> '';

  if Result then begin
    Result := ((Copy(Token, 1, 1) = '"')
              and (Copy(Token, Length(Token), 1) = '"')
              and DecodeQuotedStr(Copy(Token, 2, Length(Token) - 2), S));
  end;
end;

class function TIdSipParser.IsQValue(const Token: String): Boolean;
begin
  try
    StrToQValue(Token);
    Result := true;
  except
    Result := false;
  end;
end;

class function TIdSipParser.IsRequest(FirstLine: String): Boolean;
begin
  // We do not intend for this method to give a definitive answer.
  // "SIP/2.0" occurs as the first token of a response, and this
  // does not represent a valid token. A method MUST satisfy token
  // syntax. Ergo, we consider the below as sufficient. 
  Result := Self.IsToken(Fetch(FirstLine, ' '));
end;

class function TIdSipParser.IsScheme(const Scheme: String): Boolean;
var
  I: Integer;
begin
  Result := Scheme <> '';

  if Result then begin
    Result := Result and Self.IsLetter(Scheme[1]);

    I := 2;
    while (I <= Length(Scheme)) and Result do begin
      Result := Result
            and (Self.IsAlphaNumeric(Scheme[I])
                 or (Scheme[I] in ['+', '-', '.']));
      Inc(I);
    end;
  end;
end;

class function TIdSipParser.IsSipVersion(Version: String): Boolean;
var
  Token: String;
begin
  Token := Fetch(Version, '/');
  Result := IsEqual(Token, SipName);

  if (Result) then begin
    Token := Fetch(Version, '.');

    Result := Result and Self.IsNumber(Token);
    Result := Result and Self.IsNumber(Version);
  end;
end;

class function TIdSipParser.IsToken(const Token: String): Boolean;
var
  I: Integer;
begin
  Result := Token <> '';

  if Result then
    for I := 1 to Length(Token) do begin
      Result := Result and (Token[I] in LegalTokenChars);
      if not Result then Break;
    end;
end;

class function TIdSipParser.IsTransport(const Token: String): Boolean;
begin
  Result := Self.IsToken(Token)
end;

class function TIdSipParser.IsWord(const Token: String): Boolean;
var
  I: Integer;
begin
  Result := Token <> '';

  if Result then
    for I := 1 to Length(Token) do begin
      Result := Result and (Token[I] in LegalWordChars);

      if not Result then Break;
    end;
end;

function TIdSipParser.GetHeaderName(Header: String): String;
begin
  Result := Trim(Fetch(Header, ':'));
end;

function TIdSipParser.GetHeaderValue(Header: String): String;
begin
  if (IndyPos(':', Header) = 0) then
    Result := ''
  else begin
    Result := Header;
    Fetch(Result, ':');
    Result := Trim(Result);
  end;
end;

//******************************************************************************
//* EBadMessage                                                                *
//******************************************************************************
//* EBadMessage Public methods *************************************************

constructor EBadMessage.Create(const Msg: String;
                               const RawMessage: String);
begin
  inherited Create(Msg);
  Self.RawMessage := RawMessage;
end;

initialization
finalization
  GCanonicalHeaderNames.Free;
  GIdSipHeadersMap.Free;
end.
