unit IdSipHeaders;

interface

uses
  Classes, IdDateTimeStamp, IdSimpleParser, IdURI, Contnrs;

type
  TIdSipQValue = 0..1000;
  TIdSipTransportType = (sttSCTP, sttTCP, sttTLS, sttUDP);

  TIdSipChars = set of Char;

type
  TIdSipHeader = class;
  TIdSipHeaders = class;

  TIdSipUri = class(TObject)
  private
    fHeaders:   TIdSipHeaders;
    fHost:      String;
    fPassword:  String;
    fPort:      Cardinal;
    fScheme:    String;
    fUser:      String;
    Parameters: TStrings;

    class function IsEscapedOrInSet(const Token: String; AcceptableChars: TIdSipChars): Boolean;

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
    function  ParamsAsString: String;
    procedure Parse(Uri: String);
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
  public
    class function IsParamNameOrValue(const Token: String): Boolean;
    class function IsPassword(const Token: String): Boolean;
    class function IsUser(const Token: String): Boolean;

    constructor Create(URI: String = ''); virtual;
    destructor  Destroy; override;

    procedure AddParameter(const Name: String; const Value: String = '');
    function  HasValidSyntax: Boolean;
    function  HasHeaders: Boolean;
    function  IsLooseRoutable: Boolean;
    function  IsSecure: Boolean;
    function  ParamCount: Cardinal;
    function  ParamName(const Index: Cardinal): String;
    function  ParamValue(const Index: Cardinal): String; overload;
    function  ParamValue(const Name: String): String; overload;
    function  UserIsIp: Boolean;
    function  UserIsPhoneNumber: Boolean;

    property Headers:       TIdSipHeaders       read fHeaders;
    property Host:          String              read fHost write fHost;
    property Maddr:         String              read GetMaddr write SetMaddr;
    property Method:        String              read GetMethod write SetMethod;
    property Password:      String              read fPassword write fPassword;
    property Port:          Cardinal            read fPort write fPort;
    property Scheme:        String              read fScheme write fScheme;
    property Transport:     String              read GetTransport write SetTransport;
    property TTL:           Cardinal            read GetTTL write SetTTL;
    property Uri:           String              read GetUri write SetUri;
    property User:          String              read fUser write fUser;
    property UserParameter: String              read GetUserParameter write SetUserParameter;
  end;

  TIdSipHeader = class(TPersistent)
  private
    fName:   String;
    fParams: TStrings;
    fValue:  String;

    function  GetParam(const Name: String): String;
    function  GetParameters: TStrings;
    procedure SetParam(const Name, Value: String);
    procedure SetParameters(const Value: TStrings);

    property Parameters: TStrings read GetParameters write SetParameters;
  protected
    procedure FailParse;
    function  GetName: String; virtual;
    function  GetValue: String; virtual;
    procedure ParseParameters(Value: String; Parameters: TStrings);
    procedure SetName(const Value: String); virtual;
    procedure SetValue(const Value: String); virtual;
  public
    class function EncodeQuotedStr(const S: String): String;
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    function  IndexOfParam(Name: String): Integer;
    function  IsEqualTo(const Header: TIdSipHeader): Boolean; virtual;
    function  ParamCount: Integer;
    function  ParamsAsString: String; virtual;

    property Name:                       String read GetName write SetName;
    property Value:                      String read GetValue write SetValue;
    property Params[const Name: String]: String read GetParam write SetParam;
  end;

  TIdSipHeaderClass = class of TIdSipHeader;

  TIdSipAddressHeader = class(TIdSipHeader)
  private
    fAddress:     TIdURI;
    fDisplayName: String;

    procedure SetAddress(const Value: TIdURI);
  protected
    function  GetValue: String; override;
    procedure ParseWithoutAngleBrackets;
    procedure SetValue(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function HasSipsUri: Boolean;

    property Address:     TIdURI read fAddress write SetAddress;
    property DisplayName: String read fDisplayName write fDisplayName;
  end;

  TIdSipCallIdHeader = class(TIdSipHeader)
  protected
    procedure SetValue(const Value: String); override;
  end;

  TIdSipCommaSeparatedHeader = class(TIdSipHeader)
  private
    fValues: TStrings;
  protected
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    constructor Create; override;
    destructor Destroy; override;

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

    function  GetValues(const Index: Integer): TIdSipWeightedValue;
    procedure SetValues(const Index: Integer; const Value: TIdSipWeightedValue);
  protected
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddValue(const Value: String; const Weight: TIdSipQValue = High(TIdSipQValue));
    procedure ClearValues;
    function  ValueCount: Integer;

    property Values[const Index: Integer]: TIdSipWeightedValue read GetValues write SetValues;
  end;

  TIdSipContactHeader = class(TIdSipAddressHeader)
  private
    fIsWildCard: Boolean;

    function  GetExpires: Cardinal;
    function  GetQ: TIdSipQValue;
    procedure SetExpires(const Value: Cardinal);
    procedure SetQ(const Value: TIdSipQValue);
  protected
    function  GetName: String; override;
    procedure SetValue(const Value: String); override;
  public
    property Expires:    Cardinal     read GetExpires write SetExpires;
    property IsWildCard: Boolean      read fIsWildCard write fIsWildCard;
    property Q:          TIdSipQValue read GetQ write SetQ;
  end;

  TIdSipCSeqHeader = class(TIdSipHeader)
  private
    fMethod:     String;
    fSequenceNo: Cardinal;
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    property Method:     String   read fMethod     write fMethod;
    property SequenceNo: Cardinal read fSequenceNo write fSequenceNo;
  end;

  TIdSipDateHeader = class(TIdSipHeader)
  private
    fAbsoluteTime:   TIdDateTimeStamp;

    function  GetAbsoluteTime: TIdDateTimeStamp;
    procedure SetAbsoluteTime(Value: String);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    destructor Destroy; override;

    property Time: TIdDateTimeStamp read GetAbsoluteTime;
  end;

  TIdSipFromToHeader = class(TIdSipAddressHeader)
  private
    function  GetTag: String;
    procedure SetTag(const Value: String);
  protected
    procedure SetValue(const Value: String); override;
  public
    function HasTag: Boolean;
    function IsEqualTo(const Header: TIdSipHeader): Boolean; override;

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
    procedure SetValue(const Value: String); override;
  public
    property NumericValue: Cardinal read fNumericValue write fNumericValue;
  end;

  TIdSipMaxForwardsHeader = class(TIdSipNumericHeader)
  protected
    function  GetName: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TIdSipRouteHeader = class(TIdSipHeader)
  private
    fAddress:     TIdURI;
    fDisplayName: String;

    procedure SetAddress(const Value: TIdURI);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function EncodeQuotedStr(const S: String): String;
    function HasSipsUri: Boolean;
    function IsLooseRoutable: Boolean;

    property Address:     TIdURI read fAddress write SetAddress;
    property DisplayName: String read fDisplayName write fDisplayName;
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
    procedure SetValue(const Value: String); override;
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

  TIdSipUriHeader = class(TIdSipHeader)
  private
    fAddress: TIdURI;

    procedure SetAddress(const Value: TIdURI);
  protected
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Address: TIdURI read fAddress write SetAddress;
  end;

  TIdSipViaHeader = class(TIdSipHeader)
  private
    fSentBy:     String;
    fSipVersion: String;
    fPort:       Cardinal;
    fTransport:  TIdSipTransportType;

    procedure AssertBranchWellFormed;
    procedure AssertMaddrWellFormed;
    procedure AssertReceivedWellFormed;
    procedure AssertTTLWellFormed;
    function  GetBranch: String;
    function  GetMaddr: String;
    function  GetReceived: String;
    function  GetTTL: Byte;
    procedure SetBranch(const Value: String);
    procedure SetMaddr(const Value: String);
    procedure SetReceived(const Value: String);
    procedure SetTTL(const Value: Byte);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    procedure Assign(Src: TPersistent); override;
    function  DefaultPortForTransport(const T: TIdSipTransportType): Cardinal;
    function  HasBranch: Boolean;
    function  HasReceived: Boolean;
    function  IsDefaultPortForTransport(const Port: Cardinal; const T: TIdSipTransportType): Boolean;
    function  IsRFC3261Branch: Boolean;

    property Branch:     String              read GetBranch write SetBranch;
    property SentBy:     String              read fSentBy write fSentBy;
    property Maddr:      String              read GetMaddr write SetMaddr;
    property Port:       Cardinal            read fPort write fPort;
    property Received:   String              read GetReceived write SetReceived;
    property SipVersion: String              read fSipVersion write fSipVersion;
    property Transport:  TIdSipTransportType read fTransport write fTransport;
    property TTL:        Byte                read GetTTL write SetTTL;
  end;

  TIdSipWarningHeader = class(TIdSipHeader)
  private
    fAgent: String;
    fCode:  Cardinal;
    fText:  String;
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    property Agent: String   read fAgent write fAgent;
    property Code:  Cardinal read fCode write fCode;
    property Text:  String   read fText write fText;
  end;

  TIdSipHeaderMap = class(TObject)
    fHeaderName:  String;
    fHeaderClass: TIdSipHeaderClass;
  public
    constructor Create(HeaderName: String; HeaderClass: TIdSipHeaderClass);

    property HeaderName:  String            read fHeaderName;
    property HeaderClass: TIdSipHeaderClass read fHeaderClass;
  end;

  TIdSipHeadersFilter = class;

  TIdSipHeaderList = class(TObject)
  protected
    function GetItems(const I: Integer): TIdSipHeader; virtual; abstract;
  public
    function  Count: Integer; virtual; abstract;
    function  IsEqualTo(const OtherHeaders: TIdSipHeaderList): Boolean;

    property  Items[const I: Integer]:      TIdSipHeader read GetItems;
  end;

  TIdSipHeaders = class(TIdSipHeaderList)
  private
    List: TObjectList;

    class function HeaderTypes: TObjectList;
    class function IsHeader(const Header, ExpectedHeaderName: String): Boolean;

    function  ConstructHeader(HeaderName: String): TIdSipHeader;
    function  GetHeaders(const Name: String): TIdSipHeader;
    function  GetValues(const Name: String): String;
    function  IndexOf(const HeaderName: String): Integer;
    procedure SetValues(const Header, Value: String);
  protected
    function  GetItems(const I: Integer): TIdSipHeader; override;
  public
    class function CanonicaliseName(HeaderName: String): String;
    class function GetHeaderName(Header: String): String;
    class function IsCallID(Header: String): Boolean;
    class function IsCompoundHeader(Header: String): Boolean;
    class function IsContact(Header: String): Boolean;
    class function IsContentLength(Header: String): Boolean;
    class function IsCSeq(Header: String): Boolean;
    class function IsErrorInfo(Header: String): Boolean;
    class function IsFrom(Header: String): Boolean;
    class function IsMaxForwards(Header: String): Boolean;
    class function IsRecordRoute(Header: String): Boolean;
    class function IsRoute(Header: String): Boolean;
    class function IsTo(Header: String): Boolean;
    class function IsVia(Header: String): Boolean;
    class function IsWarning(Header: String): Boolean;

    constructor Create; virtual;
    destructor  Destroy; override;

    function  Add(const HeaderName: String): TIdSipHeader; overload;
    procedure Add(const Header: TIdSipHeader); overload;
    procedure Add(const Headers: TIdSipHeaderList); overload;
    procedure AddInReverseOrder(const Headers: TIdSipHeadersFilter);
    function  AsString: String;
    procedure Clear;
    procedure Delete(const I: Integer);
    function  Count: Integer; override;
    function  HasHeader(const HeaderName: String): Boolean;
    function  IsEmpty: Boolean;
    procedure Remove(const Header: TIdSipHeader);
    procedure RemoveAll(const HeaderName: String);

    property  Headers[const Name: String]:  TIdSipHeader read GetHeaders; default;
    property  Values[const Header: String]: String       read GetValues write SetValues;
  end;

  TIdSipHeadersFilter = class(TIdSipHeaderList)
  private
    fHeaderName: String;
    Headers:     TIdSipHeaders;
  protected
    function GetItems(const Index: Integer): TIdSipHeader; override;
  public
    constructor Create(const Headers: TIdSipHeaders; const HeaderName: String);

    procedure Add(Header: TIdSipHeader); overload;
    procedure Add(Headers: TIdSipHeaderList); overload;
    procedure Clear;
    function  Count: Integer; override;
    function  IsEmpty: Boolean;
    procedure Remove(Header: TIdSipHeader);
    procedure RemoveAll;

    property HeaderName:                  String       read fHeaderName;
    property Items[const Index: Integer]: TIdSipHeader read GetItems;
  end;

  TIdSipViaPath = class(TIdSipHeadersFilter)
  public
    constructor Create(const Headers: TIdSipHeaders);

    function  LastHop: TIdSipViaHeader;
    function  Length: Integer;
    procedure RemoveLastHop;
  end;

const
  ConvertErrorMsg      = 'Failed to convert ''%s'' to type %s';
  ParamUnreservedChars = ['[', ']', '/', ':', '&', '+', '$'];
  ParamChars           = ParamUnreservedChars + UnreservedChars;
  PasswordChars        = UnreservedChars + ['&', '=', '+', '$', ','];
  UserUnreservedChars  = ['&', '=', '+', '$', ',', ';', '?', '/'];
  UserChars            = Alphabet + Digits + UnreservedChars + UserUnreservedChars;

function NeedsQuotes(Name: String): Boolean;
function ParseNameAddr(NameAddr: String; var DisplayName, AddrSpec: String): Boolean;
function QuoteStringIfNecessary(const Name: String): String;
function QValueToStr(const Q: TIdSipQValue): String;
function StrToQValue(const S: String): TIdSipQValue;
function StrToQValueDef(const S: String; const DefaultValue: TIdSipQValue): TIdSipQValue;
function StrToTransport(const S: String): TIdSipTransportType;
function TransportToStr(const T: TIdSipTransportType): String;

implementation

uses
  IdGlobal, IdSipConsts, IdSipMessage, SysUtils;

// class variables
var
  GCanonicalHeaderNames: TStrings;
  GIdSipHeadersMap:      TObjectList;

//******************************************************************************
//* Unit procedures & functions                                                *
//******************************************************************************
//* Unit private procedures & functions ****************************************

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

function QuoteStringIfNecessary(const Name: String): String;
begin
  Result := Name;
  if NeedsQuotes(Name) then
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

function StrToQValue(const S: String): TIdSipQValue;
var
  Fraction, Int: String;
  Malformed:     Boolean;
  I:             Cardinal;
  E:             Integer;
  F:             Currency;
begin
  Result := 0;
  F      := 0;
  Fraction := S;
  Malformed := (Fraction = '') or (Pos(' ', S) > 0);

  if not Malformed then begin
    Malformed := (IndyPos('.', Fraction) > 0) and (Fraction[Length(Fraction)] = '.');

    Int := Fetch(Fraction, '.');

    Val(Int, I, E);
    Malformed := Malformed or (E <> 0) or (I > 1);

    Malformed := Malformed or (Length(Fraction) > 3);
    if (Fraction <> '') then begin
      F := Trunc(StrToCurrDef('0.' + Fraction, -1)*1000);

      Malformed := Malformed or (F = -1);
    end;

    Result := 1000*I + Trunc(F);
    Malformed := Malformed or (Result > 1000);
  end;

  if Malformed then
    raise EConvertError.Create(Format(ConvertErrorMsg, [S, 'TIdSipQValue']));
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

function StrToTransport(const S: String): TIdSipTransportType;
begin
       if (Lowercase(S) = 'sctp') then Result := sttSCTP
  else if (Lowercase(S) = 'tcp')  then Result := sttTCP
  else if (Lowercase(S) = 'tls')  then Result := sttTLS
  else if (Lowercase(S) = 'udp')  then Result := sttUDP
  else raise EConvertError.Create(Format(ConvertErrorMsg, [S, 'TIdSipTransportType']));
end;

function TransportToStr(const T: TIdSipTransportType): String;
begin
  case T of
    sttSCTP: Result := 'SCTP';
    sttTCP:  Result := 'TCP';
    sttTLS:  Result := 'TLS';
    sttUDP:  Result := 'UDP';
  else
    raise EConvertError.Create(Format(ConvertErrorMsg, ['unknown TIdSipTransportType', 'String']));
  end;
end;

//******************************************************************************
//* TIdSipUri                                                                  *
//******************************************************************************
//* TIdSipUri Public methods ***************************************************

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

constructor TIdSipUri.Create(URI: String = '');
begin
  inherited Create;

  Self.fHeaders   := TIdSipHeaders.Create;
  Self.Parameters := TStringList.Create;
  Self.Parse(Uri);
end;

destructor TIdSipUri.Destroy;
begin
  Self.Parameters.Free;
  Self.Headers.Free;

  inherited Destroy;
end;

procedure TIdSipUri.AddParameter(const Name: String; const Value: String = '');
begin
  Self.Parameters.Add(Name + '=' + TIdUri.URLDecode(Value));
end;

function TIdSipUri.HasValidSyntax: Boolean;
begin
  Result := Self.HasValidScheme
        and Self.HasValidHostInfo
        and Self.HasValidUserInfo
        and Self.HasValidParameters;

  if Self.IsSecure and (Self.Transport <> '') then
    Result := Result and (Self.Transport <> TransportParamUDP);
end;

function TIdSipUri.HasHeaders: Boolean;
begin
  Result := not Self.Headers.IsEmpty;
end;

function TIdSipUri.IsLooseRoutable: Boolean;
begin
  Result := Self.Parameters.IndexOfName(LooseRoutableParam) <> -1;
end;

function TIdSipUri.IsSecure: Boolean;
begin
  Result := IsEqual(Self.Scheme, SipsScheme);
end;

function TIdSipUri.ParamCount: Cardinal;
begin
  Result := Self.Parameters.Count;
end;

function TIdSipUri.ParamName(const Index: Cardinal): String;
begin
  Result := Self.Parameters.Names[Index];
end;

function TIdSipUri.ParamValue(const Index: Cardinal): String;
begin
  Result := Self.Parameters.Values[Self.ParamName(Index)];
end;

function TIdSipUri.ParamValue(const Name: String): String;
begin
  Result := Self.Parameters.Values[Name];
end;

function TIdSipUri.UserIsIp: Boolean;
begin
  Result := IsEqual(Self.UserParameter, UserParamIp);
end;

function TIdSipUri.UserIsPhoneNumber: Boolean;
begin
  Result := IsEqual(Self.UserParameter, UserParamPhone);
end;

//* TIdSipUri Private methods **************************************************

class function TIdSipUri.IsEscapedOrInSet(const Token: String; AcceptableChars: TIdSipChars): Boolean;
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
end;

function TIdSipUri.GetTTL: Cardinal;
begin
  Result := StrToInt(Self.ParamValue(TTLParam));
end;

function TIdSipUri.GetUri: String;
begin
  Result := Self.Scheme + ':' + Self.User + '@' + Self.Host
          + Self.ParamsAsString
          + Self.HeadersAsString;
end;

function TIdSipUri.GetUserParameter: String;
begin
  Result := Self.ParamValue(UserParam);
end;

function TIdSipUri.HasValidHostInfo: Boolean;
begin
  Result := TIdSimpleParser.IsFQDN(Self.Host)
         or TIdSimpleParser.IsIPv4Address(Self.Host)
         or TIdSimpleParser.IsIPv6Address(Self.Host);
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
  Result := ((Self.User = '') or Self.IsUser(Self.User))
        and ((Self.Password = '') or Self.IsPassword(Self.Password))
end;

function TIdSipUri.HeadersAsString: String;
var
  I:           Integer;
  HeaderName:  String;
  HeaderValue: String;
begin
  Result := '';

  if not Self.Headers.IsEmpty then begin
    for I := 0 to Self.Headers.Count - 1 do begin
      HeaderValue := Self.Headers.Items[I].AsString;
      HeaderName  := Trim(Fetch(HeaderValue, ':'));
      HeaderValue := Trim(HeaderValue);

      Result := Result + HeaderName + '=' + TIdUri.ParamsEncode(HeaderValue) + '&';
    end;

    Result := '?' + Copy(Result, 1, Length(Result) - 1);
  end;
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
      Param := Param + '=' + TIdUri.ParamsEncode(Self.ParamValue(I));

    Result := Result + ';' + Param;
  end;
end;

procedure TIdSipUri.Parse(Uri: String);
begin
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

procedure TIdSipUri.ParseHeaders(HeaderList: String);
var
  HeaderName:  String;
  HeaderValue: String;
begin
  while (HeaderList <> '') do begin
    HeaderValue := Fetch(HeaderList, '&');
    HeaderName := Fetch(HeaderValue, '=');

    Self.Headers.Add(HeaderName).Value := TIdURI.URLDecode(HeaderValue);
  end;
end;

procedure TIdSipUri.ParseHost(HostAndPort: String);
begin
  Self.Host := Fetch(HostAndPort, ':');

  if (HostAndPort = '') then begin
    if Self.IsSecure then
      Self.Port := IdPORT_SIPS
    else
      Self.Port := IdPORT_SIP;
  end
  else
    Self.Port := StrToIntDef(HostAndPort, IdPORT_SIP);
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
  Self.User     := Fetch(UserInfo, ':');
  Self.Password := UserInfo;
end;

procedure TIdSipUri.Reset;
begin
  Self.Headers.Clear;
  Self.Host      := '';
  Self.Parameters.Clear;
  Self.Password  := '';
  Self.Port      := 0;
  Self.Scheme    := '';
  Self.User      := '';
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
  Self.Reset;
  Self.Parse(Value);
end;

procedure TIdSipUri.SetUserParameter(const Value: String);
begin
  Self.Parameters.Values[UserParam] := Value;
end;

//******************************************************************************
//* TIdSipHeader                                                               *
//******************************************************************************
//* TIdSipHeader Public methods ************************************************

class function TIdSipHeader.EncodeQuotedStr(const S: String): String;
begin
  Result := S;
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll, rfIgnoreCase]);
end;

constructor TIdSipHeader.Create;
begin
  // This has no functional meaning. It just makes inspecting variables a bit
  // more sane for the developer. For instance, TIdSipViaHeader.fName will be
  // 'Via', even though a TIdSipViaHeader's fName cannot be read because GetName
  // is overridden.
  Self.Name := Self.GetName;
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
  Result := Self.Name + ': ' + Self.Value + Self.ParamsAsString;
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

function TIdSipHeader.IsEqualTo(const Header: TIdSipHeader): Boolean;
begin
  Result := (Self.Name = Header.Name)
        and (Self.Value = Header.Value);
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
  I: Integer;
begin
  Result := '';
  for I := 0 to Self.ParamCount - 1 do
    Result := Result + ';' + Self.Parameters[I];
end;

//* TIdSipHeader Protected methods *********************************************

procedure TIdSipHeader.FailParse;
begin
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

procedure TIdSipHeader.ParseParameters(Value: String; Parameters: TStrings);
var
  ParamName:  String;
  ParamValue: String;
begin
  while (Value <> '') do begin
    ParamValue := Fetch(Value, ';');
    ParamName  := Fetch(ParamValue, '=');

    ParamName  := Trim(ParamName);
    ParamValue := Trim(ParamValue);

    if (ParamValue = '') then
      Parameters.Add(ParamName)
    else
      Parameters.Values[ParamName] := ParamValue;
  end;
end;

procedure TIdSipHeader.SetName(const Value: String);
begin
  fName := Value;
end;

procedure TIdSipHeader.SetValue(const Value: String);
var
  S: String;
begin
  Self.Parameters.Clear;

  S := Value;

  if (IndyPos(';', S) = 0) then begin
    fValue := S;
  end
  else begin
    fValue := Trim(Fetch(S, ';'));

    Self.ParseParameters(S, Self.Parameters);
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

  if (I = -1) then begin
    if (Value = '') then
      Self.Parameters.Add(Trim(Name))
    else
      Self.Parameters.Add(Trim(Name) + '=' + Trim(Value));
  end
  else begin
    if (Value = '') then
      Self.Parameters[I] := Trim(Name)
    else
      Self.Parameters[I] := Trim(Name) + '=' + Trim(Value);
  end;
end;

procedure TIdSipHeader.SetParameters(const Value: TStrings);
begin
  Self.Parameters.Assign(Value);
end;

//******************************************************************************
//* TIdSipAddressHeader                                                        *
//******************************************************************************
//* TIdSipAddressHeader Public methods *****************************************

constructor TIdSipAddressHeader.Create;
begin
  inherited Create;

  fAddress := TIdURI.Create('');
end;

destructor TIdSipAddressHeader.Destroy;
begin
  fAddress.Free;

  inherited Destroy;
end;

function TIdSipAddressHeader.HasSipsUri: Boolean;
begin
  Result := Self.Address.Protocol = SipsScheme;
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

//  if (IndyPos(' ', Result) > 0) then
//    Result := '"' + Result + '"';

  URI := Self.Address.GetFullURI;
  if (IndyPos(';', URI) > 0) or (IndyPos(',', URI) > 0) or (IndyPos('?', URI) > 0) or (Result <> '') then
    URI := '<' + URI + '>';

  if (Result = '') then
    Result := URI
  else
    Result := Result + ' ' + URI;
end;

procedure TIdSipAddressHeader.ParseWithoutAngleBrackets;
begin
end;

procedure TIdSipAddressHeader.SetValue(const Value: String);
var
  AddrSpec:          String;
  DisplayName:       String;
  PreserveSemicolon: Boolean;
  S:                 String;
begin
  PreserveSemicolon := false;
  Self.DisplayName := '';
  Self.Address.URI := '';

  S := Trim(Value);
  if (IndyPos('<', Value) > 0) then begin
    if not ParseNameAddr(Value, DisplayName, AddrSpec) then
      Self.FailParse;

    Self.Address.URI := AddrSpec;
    Fetch(S, '>');
  end
  else begin
    // any semicolons in a URI not in angle brackets indicate that the
    // HEADER has the parameters, not the URI
    PreserveSemicolon := IndyPos(';', S) > 0;
    Self.Address.URI := Trim(Fetch(S, ';'));
  end;

  if PreserveSemicolon then
    S := ';' + S;

  Self.DisplayName := DisplayName;

  inherited SetValue(S);
end;

//* TIdSipAddressHeader Private methods ****************************************

procedure TIdSipAddressHeader.SetAddress(const Value: TIdURI);
begin
  fAddress.URI := Value.GetFullURI;
end;

//******************************************************************************
//* TIdSipCallIdHeader                                                         *
//******************************************************************************
//* TIdSipCallIdHeader Protected methods ***************************************

procedure TIdSipCallIdHeader.SetValue(const Value: String);
var
  Val: String;
  Token: String;
begin
  if (IndyPos('@', Value) > 0) then begin
    Val := Value;
    Token := Fetch(Val, '@');
    if not TIdSipParser.IsWord(Val) or not TIdSipParser.IsWord(Token) then
      Self.FailParse;
  end
  else if not TIdSipParser.IsWord(Value) then
    Self.FailParse;

  inherited SetValue(Value);
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

procedure TIdSipCommaSeparatedHeader.SetValue(const Value: String);
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

  fValues := TObjectList.Create(true);
end;

destructor TIdSipWeightedCommaSeparatedHeader.Destroy;
begin
  fValues.Free;

  inherited Destroy;
end;

procedure TIdSipWeightedCommaSeparatedHeader.AddValue(const Value: String; const Weight: TIdSipQValue = High(TIdSipQValue));
var
  NewValue: TIdSipWeightedValue;
  OldCount: Integer;
begin
  OldCount := Self.ValueCount;
  NewValue := TIdSipWeightedValue.Create;
  try
    NewValue.Value := Value;
    NewValue.Weight := Weight;

    fValues.Add(NewValue);
  except
    if (Self.ValueCount = OldCount) then
      NewValue.Free;
  end;
end;

procedure TIdSipWeightedCommaSeparatedHeader.ClearValues;
begin
  fValues.Clear;
end;

function TIdSipWeightedCommaSeparatedHeader.ValueCount: Integer;
begin
  Result := fValues.Count;
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

procedure TIdSipWeightedCommaSeparatedHeader.SetValue(const Value: String);
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
      Params := MediaRange;
      MediaRange := Fetch(Params, ';');
    end
    else
      Params := '';

    NewParams := TStringList.Create;
    try
      Self.ParseParameters(Params, NewParams);

      QValue := NewParams.Values[Qparam];

      if (QValue <> '')
        and not TIdSipParser.IsQValue(QValue) then
        Self.FailParse;
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

function TIdSipWeightedCommaSeparatedHeader.GetValues(const Index: Integer): TIdSipWeightedValue;
begin
  Result := fValues[Index] as TIdSipWeightedValue;
end;

procedure TIdSipWeightedCommaSeparatedHeader.SetValues(const Index: Integer; const Value: TIdSipWeightedValue);
begin
  (fValues[Index] as TIdSipWeightedValue).Value  := Value.Value;
  (fValues[Index] as TIdSipWeightedValue).Weight := Value.Weight;
end;

//******************************************************************************
//* TIdSipContactHeader                                                        *
//******************************************************************************
//* TIdSipContactHeader Protected methods **************************************

function TIdSipContactHeader.GetName: String;
begin
  Result := ContactHeaderFull;
end;

procedure TIdSipContactHeader.SetValue(const Value: String);
var
  S: String;
begin
  S := Value;
  Self.IsWildCard := Fetch(S, ';') = '*';

  if not Self.IsWildCard then
    inherited SetValue(Value)
  else
    Self.ParseParameters(S, Self.Parameters);

  if (Self.IndexOfParam(QParam) > -1) and not TIdSipParser.IsQValue(Self.Params[QParam]) then
    Self.FailParse;
  if (Self.IndexOfParam(ExpiresParam) > -1) and not TIdSipParser.IsNumber(Self.Params[ExpiresParam]) then
    Self.FailParse;
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

procedure TIdSipContactHeader.SetExpires(const Value: Cardinal);
begin
  Self.Params[ExpiresParam] := IntToStr(Value);
end;

procedure TIdSipContactHeader.SetQ(const Value: TIdSipQValue);
begin
  Self.Params[QParam] := QValueToStr(Value);
end;

//******************************************************************************
//* TIdSipCSeqHeader                                                           *
//******************************************************************************
//* TIdSipCSeqHeader Protected methods *****************************************

function TIdSipCSeqHeader.GetName: String;
begin
  Result := CSeqHeader;
end;

function TIdSipCSeqHeader.GetValue: String;
begin
  Result := IntToStr(Self.SequenceNo) + ' ' + Self.Method;
end;

procedure TIdSipCSeqHeader.SetValue(const Value: String);
var
  S:     String;
  Token: String;
  E:     Integer;
  N:     Cardinal;
begin
  S := Trim(Value);
  // Yes, sure, there will be no spaces returned from Fetch(S, ' '). But what
  // about whitespace? Best to be sure!
  Token := Trim(Fetch(S, ' '));

  Val(Token, N, E);
  if (E <> 0) then
    Self.FailParse;

  Self.SequenceNo := N;

  Token := Trim(S);
  if not TIdSipParser.IsMethod(Token) then
    Self.FailParse;

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

//* TIdSipDateHeader Protected methods *****************************************

function TIdSipDateHeader.GetName: String;
begin
  Result := DateHeader;
end;

function TIdSipDateHeader.GetValue: String;
begin
  Result := inherited GetValue;
end;

procedure TIdSipDateHeader.SetValue(const Value: String);
begin
  inherited SetValue(Value);

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
  Self.Time.SetFromRFC822(Value);

  // todo: this is a bit crap. What if someone uses "xxx, 1 Jan 1899 00:00:00 GMT"?
  if (Self.Time.AsTDateTime = 0) then
    Self.FailParse;
end;

//******************************************************************************
//* TIdSipFromToHeader                                                         *
//******************************************************************************
//* TIdSipFromToHeader Public methods ******************************************

function TIdSipFromToHeader.HasTag: Boolean;
begin
  Result := Self.Tag <> '';
end;

function TIdSipFromToHeader.IsEqualTo(const Header: TIdSipHeader): Boolean;
var
  From: TIdSipFromToHeader;
begin
  Result := Header is TIdSipFromToHeader;

  if Result then begin
    From := Header as TIdSipFromToHeader;

    Result := (Self.Name = Header.Name)
          and (Self.Address.GetFullURI = From.Address.GetFullURI)
          and (Self.Tag = From.Tag);
  end;
end;

//* TIdSipFromToHeader Protected methods ***************************************

procedure TIdSipFromToHeader.SetValue(const Value: String);
begin
  inherited SetValue(Value);

  if (Self.IndexOfParam(TagParam) > -1) and not TIdSipParser.IsToken(Self.Params[TagParam]) then
    Self.FailParse;
end;

//* TIdSipFromToHeader Private methods *****************************************

function TIdSipFromToHeader.GetTag: String;
begin
  Result := Self.Params[TagParam];
end;

procedure TIdSipFromToHeader.SetTag(const Value: String);
begin
  Self.Params[TagParam] := Value;

  if (Self.IndexOfParam(TagParam) > -1) and not TIdSipParser.IsToken(Self.Params[TagParam]) then
    Self.FailParse;
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
//* TIdSipMaxForwardsHeader                                                    *
//******************************************************************************
//* TIdSipMaxForwardsHeader Protected methods **********************************

function TIdSipMaxForwardsHeader.GetName: String;
begin
  Result := MaxForwardsHeader;
end;

procedure TIdSipMaxForwardsHeader.SetValue(const Value: String);
var
  N: Cardinal;
  E: Integer;
begin
  Val(Value, N, E);

  if (E <> 0) or (N > 255) then
    Self.FailParse;

  inherited SetValue(Value);
end;

//******************************************************************************
//* TIdSipNumericHeader                                                        *
//******************************************************************************
//* TIdSipNumericHeader Protected methods **************************************

function TIdSipNumericHeader.GetValue: String;
begin
  Result := IntToStr(fNumericValue);
end;

procedure TIdSipNumericHeader.SetValue(const Value: String);
begin
  if not TIdSipParser.IsNumber(Value) then
    Self.FailParse;

  fNumericValue := StrToInt(Value);

  inherited SetValue(Value);
end;

//******************************************************************************
//* TIdSipRouteHeader                                                          *
//******************************************************************************
//* TIdSipRouteHeader Public methods *******************************************

constructor TIdSipRouteHeader.Create;
begin
  inherited Create;

  fAddress := TIdURI.Create('');
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
  Result := Self.Address.Protocol = SipsScheme;
end;

function TIdSipRouteHeader.IsLooseRoutable: Boolean;
begin
  Result := Pos(LooseRoutableParam, Self.Address.GetFullURI) > 0;
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

procedure TIdSipRouteHeader.SetValue(const Value: String);
var
  AddrSpec:     String;
  DisplayName:  String;
  HeaderParams: String;
begin
  if not ParseNameAddr(Value, DisplayName, AddrSpec) then
    Self.FailParse;

  if (IndyPos(':', AddrSpec) = 0) then
    Self.FailParse;

  Self.Address.URI := AddrSpec;
  Self.DisplayName := DisplayName;

  // cull the processed name-addr (and its parameters!)
  HeaderParams := Value;
  Fetch(HeaderParams, '>');

  inherited SetValue(HeaderParams);
end;

//* TIdSipRouteHeader Private methods ******************************************

procedure TIdSipRouteHeader.SetAddress(const Value: TIdURI);
begin
  fAddress.URI := Value.URI;
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
  if (Src = '') then Self.FailParse;

  I := 1;
  while (Src[I] in Digits) do Inc(I);


  Number := Copy(Src, 1, I - 1);
  if not TIdSipParser.IsNumber(Number) then Self.FailParse;

  Result := StrToInt(Number);
  Delete(Src, 1, I - 1);
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

procedure TIdSipTimestampHeader.SetValue(const Value: String);
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
    if (S[1] <> '.') then Self.FailParse;
    Delete(S, 1, 1);

    if (S <> '') then
      Self.Timestamp.FractionalPart := Self.ReadNumber(S);
  end;

  if (S <> '') then begin
    if (S[1] <> ' ') then Self.FailParse;
    Delete(S, 1, 1);

    if (S[1] = '.') then
      Self.Delay.IntegerPart := 0
    else
      Self.Delay.IntegerPart := Self.ReadNumber(S);
  end;

  if (S <> '') then begin
    if (S[1] <> '.') then Self.FailParse;
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
//* TIdSipUriHeader                                                            *
//******************************************************************************
//* TIdSipUriHeader Public methods *********************************************

constructor TIdSipUriHeader.Create;
begin
  inherited Create;

  fAddress := TIdURI.Create('');
end;

destructor TIdSipUriHeader.Destroy;
begin
  fAddress.Free;

  inherited Destroy;
end;

function TIdSipUriHeader.GetValue: String;
begin
  Result := Self.Address.GetFullURI;

  if   (IndyPos(';', Result) > 0)
    or (IndyPos(',', Result) > 0)
    or (IndyPos('?', Result) > 0)
    or (Result <> '') then
    Result := '<' + Result + '>';
end;

procedure TIdSipUriHeader.SetValue(const Value: String);
var
  AddrSpec:    String;
  DisplayName: String;
  S:           String;
begin
  Self.Address.URI := '';

  S := Trim(Value);
  if (IndyPos('<', Value) = 0) then
    Self.FailParse;

  if not ParseNameAddr(Value, DisplayName, AddrSpec) then
    Self.FailParse;
  if (DisplayName <> '') then
    Self.FailParse;

  Self.Address.URI := AddrSpec;
  Fetch(S, '>');

  inherited SetValue(S);
end;

//* TIdSipUriHeader Private methods ********************************************

procedure TIdSipUriHeader.SetAddress(const Value: TIdURI);
begin
  fAddress.URI := Value.URI;
end;

//******************************************************************************
//* TIdSipViaHeader                                                            *
//******************************************************************************
//* TIdSipViaHeader Public methods *********************************************

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
    fSentBy     := V.SentBy;
    fSipVersion := V.SipVersion;
    fPort       := V.Port;
    fTransport  := V.Transport;

    // And we use the usual way of setting everything else.
    Self.Parameters := V.Parameters;
  end
  else
    inherited Assign(Src);
end;

function TIdSipViaHeader.DefaultPortForTransport(const T: TIdSipTransportType): Cardinal;
begin
  if (T = sttTLS) then
    Result := IdPort_SIPS
  else
    Result := IdPORT_SIP;
end;

function TIdSipViaHeader.HasBranch: Boolean;
begin
  Result := Self.Branch <> '';
end;

function TIdSipViaHeader.HasReceived: Boolean;
begin
  Result := Self.Received <> '';
end;

function TIdSipViaHeader.IsDefaultPortForTransport(const Port: Cardinal; const T: TIdSipTransportType): Boolean;
begin
  Result := ((T = sttTLS) and (Port = IdPORT_SIPS))
         or (Port = IdPORT_SIP);
end;

function TIdSipViaHeader.IsRFC3261Branch: Boolean;
begin
  Result := Copy(Self.Branch, 1, Length(BranchMagicCookie)) = BranchMagicCookie;
end;

//* TIdSipViaHeader Protected methods ******************************************

function TIdSipViaHeader.GetName: String;
begin
  Result := ViaHeaderFull;
end;

function TIdSipViaHeader.GetValue: String;
begin
  Result := Self.SipVersion + '/' + TransportToStr(Self.Transport)
          + ' ' + Self.SentBy;

  if not Self.IsDefaultPortForTransport(Self.Port, Self.Transport) then
    Result := Result + ':' + IntToStr(Self.Port);
end;

procedure TIdSipViaHeader.SetValue(const Value: String);
var
  Token: String;
  S:     String;
begin
  inherited SetValue(Value);

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
    Self.FailParse;

  Self.Transport := StrToTransport(Token);

  Token := Trim(Fetch(S, ';'));
  Self.SentBy := Fetch(Token, ':');

  if (Token = '') then
    Self.Port := Self.DefaultPortForTransport(Self.Transport)
  else
    Self.Port := StrToInt(Token);
end;

//* TIdSipViaHeader Private methods ********************************************

procedure TIdSipViaHeader.AssertBranchWellFormed;
begin
  if (Self.IndexOfParam(BranchParam) > -1)
     and not TIdSipParser.IsToken(Self.Params[BranchParam]) then
    Self.FailParse;
end;

procedure TIdSipViaHeader.AssertMaddrWellFormed;
begin
  if (Self.Parameters.IndexOfName(MaddrParam) > -1) then begin
    if    not TIdSipParser.IsFQDN(Self.Parameters.Values[MaddrParam])
      and not TIdSipParser.IsIPv4Address(Self.Parameters.Values[MaddrParam])
      and not TIdSipParser.IsIPv6Reference(Self.Parameters.Values[MaddrParam]) then
      Self.FailParse;
  end;
end;

procedure TIdSipViaHeader.AssertReceivedWellFormed;
begin
  if (Self.IndexOfParam(ReceivedParam) > -1)
    and not TIdSipParser.IsIPv4Address(Self.Params[ReceivedParam])
    and not TIdSipParser.IsIPv6Address(Self.Params[ReceivedParam]) then
    Self.FailParse;
end;

procedure TIdSipViaHeader.AssertTTLWellFormed;
begin
  if (Self.Parameters.IndexOfName(TTLParam) > -1) then begin
    if not TIdSipParser.IsByte(Self.Parameters.Values[TTLParam]) then
      Self.FailParse;
  end;
end;

function TIdSipViaHeader.GetBranch: String;
begin
  if (Self.Parameters.IndexOfName(BranchParam) = -1) then
    Result := ''
  else
    Result := Self.Params[BranchParam];
end;

function TIdSipViaHeader.GetMaddr: String;
begin
  if (Self.Parameters.IndexOfName(MaddrParam) = -1) then
    Result := ''
  else
    Result := Self.Params[MaddrParam];
end;

function TIdSipViaHeader.GetReceived: String;
begin
  if (Self.Parameters.IndexOfName(ReceivedParam) = -1) then
    Result := ''
  else
    Result := Self.Params[ReceivedParam];
end;

function TIdSipViaHeader.GetTTL: Byte;
begin
  if (Self.Parameters.IndexOfName(TTLParam) = -1) then
    Result := 0
  else
    Result := StrToInt(Self.Params[TTLParam]);
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

procedure TIdSipViaHeader.SetReceived(const Value: String);
begin
  Self.Params[ReceivedParam] := Value;

  Self.AssertReceivedWellFormed;
end;

procedure TIdSipViaHeader.SetTTL(const Value: Byte);
begin
  Self.Params[TTLParam] := IntToStr(Value);
end;

//******************************************************************************
//* TIdSipWarningHeader                                                        *
//******************************************************************************
//* TIdSipWarningHeader Protected methods **************************************

function TIdSipWarningHeader.GetName: String;
begin
  Result := WarningHeader;
end;

function TIdSipWarningHeader.GetValue: String;
begin
  Result := Format('%d %s "%s"', [Self.Code, Self.Agent, Self.EncodeQuotedStr(Self.Text)]);
end;

procedure TIdSipWarningHeader.SetValue(const Value: String);
var
  S: String;
  Token: String;
begin
  S := Value;

  Token := Fetch(S, ' ');
  if not TIdSipParser.IsNumber(Token) or (Length(Token) <> 3) then
    Self.FailParse;
  Self.Code := StrToInt(Token);

  Token := Fetch(S, ' ');
  if not TIdSipParser.IsToken(Token) then
    Self.FailParse;
  Self.Agent := Token;

  if not TIdSipParser.IsQuotedString(S) then
    Self.FailParse;

  DecodeQuotedStr(Copy(S, 2, Length(S) - 2), S);
  Self.Text := S;
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

function TIdSipHeaderList.IsEqualTo(const OtherHeaders: TIdSipHeaderList): Boolean;
var
  I: Integer;
begin
  Result := Self.Count = OtherHeaders.Count;

  I := 0;
  if Result then begin
    while Result and (I < Self.Count) do begin
      Result := Result and (Self.Items[I].IsEqualTo(OtherHeaders.Items[I]));
      Inc(I);
    end;
  end;
end;

//******************************************************************************
//* TIdSipHeaders                                                              *
//******************************************************************************
//* TIdSipHeaders Public methods ***********************************************

class function TIdSipHeaders.CanonicaliseName(HeaderName: String): String;
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

  if (GCanonicalHeaderNames.IndexOfName(HeaderName) > -1) then
    Result := GCanonicalHeaderNames.Values[HeaderName];

  if (Result = '') then begin
      Result := HeaderName;
  end;
end;

class function TIdSipHeaders.GetHeaderName(Header: String): String;
begin
  Result := Trim(Fetch(Header, ':'));
end;

class function TIdSipHeaders.IsCallID(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, CallIDHeaderFull);
end;

class function TIdSipHeaders.IsCompoundHeader(Header: String): Boolean;
begin
  Result := Self.IsContact(Header)
         or Self.IsErrorInfo(Header)
         or Self.IsRecordRoute(Header)
         or Self.IsRoute(Header)
         or Self.IsVia(Header)
         or Self.IsWarning(Header);
end;

class function TIdSipHeaders.IsContact(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, ContactHeaderFull);
end;

class function TIdSipHeaders.IsContentLength(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, ContentLengthHeaderFull);
end;

class function TIdSipHeaders.IsCSeq(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, CseqHeader);
end;

class function TIdSipHeaders.IsErrorInfo(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, ErrorInfoHeader);
end;

class function TIdSipHeaders.IsFrom(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, FromHeaderFull);
end;

class function TIdSipHeaders.IsMaxForwards(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, MaxForwardsHeader);
end;

class function TIdSipHeaders.IsRecordRoute(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, RecordRouteHeader);
end;

class function TIdSipHeaders.IsRoute(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, RouteHeader);
end;

class function TIdSipHeaders.IsTo(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, ToHeaderFull);
end;

class function TIdSipHeaders.IsVia(Header: String): Boolean;
begin
  Result := Self.IsHeader(Header, ViaHeaderFull);
end;

class function TIdSipHeaders.IsWarning(Header: String): Boolean;
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
    Result.Free;

    raise;
  end;
end;

procedure TIdSipHeaders.Add(const Header: TIdSipHeader);
var
  H: TIdSipHeader;
begin
  H := Self.ConstructHeader(Header.Name);
  H.Assign(Header);

  Self.List.Add(H);
end;

procedure TIdSipHeaders.Add(const Headers: TIdSipHeaderList);
var
  I: Integer;
begin
  for I := 0 to Headers.Count - 1 do
    Self.Add(Headers.Items[I]);
end;

procedure TIdSipHeaders.AddInReverseOrder(const Headers: TIdSipHeadersFilter);
var
  I: Integer;
begin
  for I := Headers.Count - 1 downto 0 do
    Self.Add(Headers.Items[I]);
end;

function TIdSipHeaders.AsString: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Self.Count - 1 do
    Result := Result + (Self.List[I] as TIdSipHeader).AsString + EOL;
end;

procedure TIdSipHeaders.Clear;
begin
  Self.List.Clear;
end;

procedure TIdSipHeaders.Delete(const I: Integer);
begin
  Self.List.Delete(I);
end;

function TIdSipHeaders.Count: Integer;
begin
  Result := Self.List.Count;
end;

function TIdSipHeaders.HasHeader(const HeaderName: String): Boolean;
begin
  Result := Self.IndexOf(HeaderName) <> -1;
end;

function TIdSipHeaders.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

procedure TIdSipHeaders.Remove(const Header: TIdSipHeader);
begin
  Self.List.Remove(Header);
end;

procedure TIdSipHeaders.RemoveAll(const HeaderName: String);
var
  I: Integer;
  H: TIdSipHeader;
begin
  I := 0;

  while (I < Self.Count) do begin
    H := Self.Items[I];
    if (IsEqual(H.Name, HeaderName)) then
      Self.Remove(H)
    else
      Inc(I);
  end;
end;

//* TIdSipHeaders Protected methods ********************************************

function TIdSipHeaders.GetItems(const I: Integer): TIdSipHeader;
begin
  Result := Self.List[I] as TIdSipHeader;
end;

//* TIdSipHeaders Private methods **********************************************

class function TIdSipHeaders.HeaderTypes: TObjectList;
begin
  if not Assigned(GIdSipHeadersMap) then begin
    GIdSipHeadersMap := TObjectList.Create(true);

    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(AcceptHeader,                TIdSipWeightedCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(AcceptEncodingHeader,        TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(AlertInfoHeader,             TIdSipUriHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(AllowHeader,                 TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(CallIDHeaderFull,            TIdSipCallIDHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(CallIDHeaderShort,           TIdSipCallIDHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContactHeaderFull,           TIdSipContactHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContactHeaderShort,          TIdSipContactHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContentEncodingHeaderFull,   TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContentEncodingHeaderShort,  TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContentLanguageHeader,       TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContentLengthHeaderFull,     TIdSipNumericHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(CSeqHeader,                  TIdSipCSeqHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(DateHeader,                  TIdSipDateHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ErrorInfoHeader,             TIdSipUriHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ExpiresHeader,               TIdSipNumericHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(FromHeaderFull,              TIdSipFromHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(FromHeaderShort,             TIdSipFromHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(InReplyToHeader,             TIdSipCallIdHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(MaxForwardsHeader,           TIdSipMaxForwardsHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ProxyRequireHeader,          TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(RecordRouteHeader,           TIdSipRecordRouteHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(RequireHeader,               TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(RouteHeader,                 TIdSipRouteHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(SupportedHeaderFull,         TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(SupportedHeaderShort,        TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(TimestampHeader,             TIdSipTimestampHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ToHeaderFull,                TIdSipToHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ToHeaderShort,               TIdSipToHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(UnsupportedHeader,           TIdSipCommaSeparatedHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ViaHeaderFull,               TIdSipViaHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ViaHeaderShort,              TIdSipViaHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(WarningHeader,               TIdSipWarningHeader));
  end;

  Result := GIdSipHeadersMap;
end;

class function TIdSipHeaders.IsHeader(const Header, ExpectedHeaderName: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Self.CanonicaliseName(Name), ExpectedHeaderName);
end;

function TIdSipHeaders.ConstructHeader(HeaderName: String): TIdSipHeader;
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

function TIdSipHeaders.GetHeaders(const Name: String): TIdSipHeader;
var
  I: Integer;
begin
  I := Self.IndexOf(Name);

  if (I = -1) then
    Result := Self.Add(Name)
  else
    Result := Self.List[I] as TIdSipHeader;
end;

function TIdSipHeaders.GetValues(const Name: String): String;
var
  I: Integer;
begin
  I := Self.IndexOf(Name);

  if (I = -1) then begin
//    Result := Self.Add(Name).Value;
    Result := ''
  end
  else
    Result := (Self.List[I] as TIdSipHeader).Value;
end;

function TIdSipHeaders.IndexOf(const HeaderName: String): Integer;
var
  Found: Boolean;
begin
  Result := 0;
  Found := false;
  while (Result < Self.List.Count) and not Found do
    if IsEqual((Self.List[Result] as TIdSipHeader).Name, HeaderName) then
      Found := true
    else
      Inc(Result);

  if (Result = Self.List.Count) then
    Result := -1;
end;

procedure TIdSipHeaders.SetValues(const Header, Value: String);
var
  I: Integer;
begin
  I := Self.IndexOf(Header);

  if (I = -1) then
    Self.Add(Header).Value := Value
  else
    (Self.List[I] as TIdSipHeader).Value := Value;
end;

//******************************************************************************
//* TIdSipHeadersFilter                                                        *
//******************************************************************************
//* TIdSipHeadersFilter Public methods *****************************************

constructor TIdSipHeadersFilter.Create(const Headers: TIdSipHeaders; const HeaderName: String);
begin
  Self.fHeaderName := HeaderName;
  Self.Headers     := Headers;
end;

procedure TIdSipHeadersFilter.Add(Header: TIdSipHeader);
begin
  Self.Headers.Add(Header);
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
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Self.Headers.Count - 1 do
    if (Self.Headers.Items[I].Name = Self.HeaderName) then
      Inc(Result);
end;

function TIdSipHeadersFilter.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
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

function TIdSipHeadersFilter.GetItems(const Index: Integer): TIdSipHeader;
var
  I:         Integer;
  ItemCount: Integer;
begin
  Result := nil;
  I         := 0;
  ItemCount := -1;

  while (I < Self.Headers.Count) and not Assigned(Result) do begin
    if (Self.Headers.Items[I].Name = Self.HeaderName) then
      Inc(ItemCount);

    if (ItemCount = Index) then
      Result := Self.Headers.Items[I];

    Inc(I);
  end;
end;

//******************************************************************************
//* TIdSipViaPath                                                              *
//******************************************************************************
//* TIdSipViaPath Public methods ***********************************************

constructor TIdSipViaPath.Create(const Headers: TIdSipHeaders);
begin
  inherited Create(Headers, ViaHeaderFull);
end;

function TIdSipViaPath.LastHop: TIdSipViaHeader;
var
  I: Integer;
begin
  Result := nil;
  I := 0;

  while (I < Self.Headers.Count) and not Assigned(Result) do
    if TIdSipHeaders.IsVia(Self.Headers.Items[I].Name) then
      Result := Self.Headers.Items[I] as TIdSipViaHeader
    else
      Inc(I);

//  if not Assigned(Result) then
//    Result := Self.Headers.Add(ViaHeaderFull) as TIdSipViaHeader;
end;

function TIdSipViaPath.Length: Integer;
begin
  Result := Self.Count;
end;

procedure TIdSipViaPath.RemoveLastHop;
begin
  Self.Remove(Self.LastHop);
end;

initialization
finalization
  GCanonicalHeaderNames.Free;
  GIdSipHeadersMap.Free;
end.
