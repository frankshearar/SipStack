unit IdSipParser;

interface

uses
  Classes, Contnrs, IdGlobal, IdURI, SysUtils;

const
  SIPVersion = 'SIP/2.0';

type
  TIdSipTransportType = (sttSCTP, sttTCP, sttTLS, sttUDP);

  TIdSipHeader = class(TPersistent)
  private
    fName:   String;
    fParams: TStrings;
    fValue: String;

    function  GetParam(const Name: String): String;
    procedure SetParam(const Name, Value: String);
    procedure SetParameters(const Value: TStrings);
  protected
    function  GetValue: String; virtual;
    procedure SetValue(const Value: String); virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;
    function  IsEqualTo(const Header: TIdSipHeader): Boolean; virtual;
    function  ParamsAsString: String; virtual;

    property Name:                       String read fName write fName;
    property Value:                      String read GetValue write SetValue;
    property Parameters:                 TStrings read fParams write SetParameters;
    property Params[const Name: String]: String read GetParam write SetParam;
  end;

  TIdSipHeaders = class(TObject)
  private
    List: TObjectList;

    function GetHeaders(const Name: String): TIdSipHeader;
    function IndexOf(const HeaderName: String): Integer;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    function Add(const HeaderName: String): TIdSipHeader;
    function Count: Integer;
    function HasHeader(const HeaderName: String): Boolean;
    property Headers[const Name: String]: TIdSipHeader read GetHeaders; default;
  end;

  TIdSipAddressHeader = class(TIdSipHeader)
  private
    fAddress: TIdURI;
    fName:    String;

    procedure SetAddress(const Value: TIdURI);
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Address: TIdURI read fAddress write SetAddress;
    property Name:    String read fName write fName;
  end;

  TIdSipViaHeader = class(TIdSipHeader)
  private
    fHost:       String;
    fSipVersion: String;
    fPort:       Cardinal;
    fTransport:  TIdSipTransportType;
  protected
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    function DefaultPortForTransport(const T: TIdSipTransportType): Cardinal;
    function IsDefaultPortForTransport(const Port: Cardinal; const T: TIdSipTransportType): Boolean;
    function IsEqualTo(const Header: TIdSipHeader): Boolean; override;

    property Host:       String              read fHost write fHost;
    property Port:       Cardinal            read fPort write fPort;
    property SipVersion: String              read fSipVersion write fSipVersion;
    property Transport:  TIdSipTransportType read fTransport write fTransport;
  end;

  TIdSipPath = class(TObject)
  private
    fViaHeaders: TObjectList;

    property Headers: TObjectList read fViaHeaders;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Hop: TIdSipViaHeader);
    function  FirstHop: TIdSipViaHeader;
    function  LastHop: TIdSipViaHeader;
    function  Length: Integer;
  end;

  TIdSipMessage = class(TObject)
  private
    fBody:          String;
    fCallID:        String;
    fContentLength: Cardinal;
    fPath:          TIdSipPath;
    fMaxForwards:   Byte;
    fOtherHeaders:  TStrings;
    fSIPVersion:    String;

    procedure SetPath(const Value: TIdSipPath);
  public
    constructor Create;
    destructor  Destroy; override;

    function  AsString: String; virtual; abstract;
    function  MalformedException: ExceptClass; virtual; abstract;
    procedure ReadBody(const S: TStream);

    property Body:          String     read fBody write fBody;
    property CallID:        String     read fCallID write fCallID;
    property ContentLength: Cardinal   read fContentLength write fContentLength;
    property MaxForwards:   Byte       read fMaxForwards write fMaxForwards;
    property OtherHeaders:  TStrings   read fOtherHeaders;
    property Path:          TIdSipPath read fPath write SetPath;
    property SIPVersion:    String     read fSIPVersion write fSIPVersion;
  end;

  TIdSipRequest = class(TIdSipMessage)
  private
    fMethod:     String;
    fRequest:    String;
  public
    function AsString: String; override;
    function MalformedException: ExceptClass; override;

    property Method:  String read fMethod write fMethod;
    property Request: String read fRequest write fRequest;
  end;

  TIdSipResponse = class(TIdSipMessage)
  private
    fStatusCode:    Integer;
    fStatusText:    String;

    procedure SetStatusCode(const Value: Integer);
  public
    function AsString: String; override;
    function MalformedException: ExceptClass; override;

    property StatusCode: Integer read fStatusCode write SetStatusCode;
    property StatusText: String  read fStatusText write fStatusText;
  end;

  TIdSipParser = class(TObject)
  private
    fCurrentLine: Cardinal;
    fSource:      TStream;

    procedure AddHeader(const Msg: TIdSipMessage; Header: String);
    procedure IncCurrentLine;
    procedure InitialiseMessage(Msg: TIdSipMessage);
//    procedure IsSpecifiedHeader(Header: String
    procedure ParseHeader(const Msg: TIdSipMessage; const Header: String);
    procedure ParseHeaders(const Msg: TIdSipMessage);
    procedure ParseRequestLine(const Request: TIdSipRequest);
    procedure ParseStatusLine(const Response: TIdSipResponse);
    procedure ParseViaHeader(const Msg: TIdSipMessage; ViaParms: String);
    procedure ResetCurrentLine;
  public
    constructor Create;

    function  CurrentLine: Cardinal;
    function  Eof: Boolean;
    function  GetHeaderName(Header: String): String;
    function  GetHeaderNumberValue(const Msg: TIdSipMessage; const Header: String): Cardinal;
    function  GetHeaderValue(Header: String): String;
    function  IsCallID(Header: String): Boolean;
    function  IsContentLength(Header: String): Boolean;
    function  IsMaxForwards(Header: String): Boolean;
    function  IsMethod(Method: String): Boolean;
    function  IsNumber(Number: String): Boolean;
    function  IsTo(Header: String): Boolean;
    function  IsVia(Header: String): Boolean;
    function  IsSipVersion(Version: String): Boolean;
    function  MakeBadRequestResponse(const Reason: String): TIdSipResponse;
    function  ParseAndMakeMessage: TIdSipMessage;
    procedure ParseRequest(const Request: TIdSipRequest);
    procedure ParseResponse(const Response: TIdSipResponse);
    function  Peek: Char;
    function  PeekLine: String;
    function  ReadOctet: Char;
    function  ReadOctets(Count: Cardinal): String;
    function  ReadLn: String;
    function  ReadFirstNonBlankLn: String;

    property  Source: TStream read fSource write fSource;
  end;

  EParser = class(Exception);
  EBadRequest = class(EParser);
  EBadResponse = class(EParser);

const
  BadStatusCode             = -1;
  DamagedLineEnd            = 'Damaged line end at line %d, expected %s but was %s';
  EmptyInputStream          = 'Empty input stream';
  InvalidSipVersion         = 'Invalid Sip-Version: ''%s''';
  InvalidStatusCode         = 'Invalid Status-Code: ''%s''';
  MalformedToken            = 'Malformed %s: ''%s''';
  MissingSipVersion         = 'Missing SIP-Version';
  RequestLine               = '%s %s %s' + EOL;
  RequestUriNoAngleBrackets = 'Request-URI may not be enclosed in <>';
  RequestUriNoSpaces        = 'Request-URI may not contain spaces';
  StatusLine                = '%s %d %s' + EOL;
  UnexpectedMessageLength   = 'Expected message-body length of %d but was %d';

const
  CallIDHeaderFull         = 'Call-ID';
  CallIDHeaderShort        = 'i';
  ContentLengthHeaderFull  = 'Content-Length';
  ContentLengthHeaderShort = 'l';
  DefaultMaxForwards       = 70;
  MaxForwardsHeader        = 'Max-Forwards';
  MethodAck                = 'ACK';
  MethodBye                = 'BYE';
  MethodCancel             = 'CANCEL';
  MethodInvite             = 'INVITE';
  MethodOptions            = 'OPTIONS';
  MethodRegister           = 'REGISTER';
  SipName                  = 'SIP';
  ToHeaderFull             = 'To';
  ToHeaderShort            = 't';
  ViaHeaderFull            = 'Via';
  ViaHeaderShort           = 'v';

// move this to the appropriate unit - IdAssignedNumbers?
const
  IdPORT_SIP     = 5060;
  IdPORT_SIP_TLS = 5061;

// to go in IdResourceStrings
const
  RSSIPTrying                           = 'Trying';
  RSSIPRinging                          = 'Ringing';
  RSSIPCallIsBeingForwarded             = 'Call Is Being Forwarded';
  RSSIPQueued                           = 'Queued';
  RSSIPSessionProgess                   = 'Session Progress';
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
  RSSIPTemporarilyNotAvailable          = 'Temporarily not available';
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

const
  SIPTrying                           = 100;
  SIPRinging                          = 180;
  SIPCallIsBeingForwarded             = 181;
  SIPQueued                           = 182;
  SIPSessionProgess                   = 183;
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
  SIPTemporarilyNotAvailable          = 480;
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

function IsEqual(const S1, S2: String): Boolean;
function StrToTransport(const S: String): TIdSipTransportType;
function TransportToStr(const T: TIdSipTransportType): String;

implementation

uses
  StrUtils;

//******************************************************************************
//* Unit public procedures & functions                                         *
//******************************************************************************

function IsEqual(const S1, S2: String): Boolean;
begin
  Result := Lowercase(S1) = Lowercase(S2);
end;

function StrToTransport(const S: String): TIdSipTransportType;
begin
       if (Lowercase(S) = 'sctp') then Result := sttSCTP
  else if (Lowercase(S) = 'tcp')  then Result := sttTCP
  else if (Lowercase(S) = 'tls')  then Result := sttTLS
  else if (Lowercase(S) = 'udp')  then Result := sttUDP
  else raise EConvertError.Create('Failed to convert ''' + S + ''' to type TIdSipTransportType');
end;

function TransportToStr(const T: TIdSipTransportType): String;
begin
  case T of
    sttSCTP: Result := 'SCTP';
    sttTCP:  Result := 'TCP';
    sttTLS:  Result := 'TLS';
    sttUDP:  Result := 'UDP';
  else
    raise EConvertError.Create('Failed to convert unknown transport to type string');
  end;
end;

//******************************************************************************
//* TIdSipHeader                                                               *
//******************************************************************************
//* TIdSipHeader Public methods ************************************************

constructor TIdSipHeader.Create;
begin
  inherited Create;

  fParams := TStringList.Create;
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
    Self.Parameters := H.Parameters;
    Self.Value      := H.Value;
  end
  else inherited Assign(Src);
end;

function TIdSipHeader.IsEqualTo(const Header: TIdSipHeader): Boolean;
begin
  Result := (Self.Name = Header.Name)
        and (Self.Value = Header.Value);
end;

function TIdSipHeader.ParamsAsString: String;
var
  I: Integer;
begin
  for I := 0 to Self.Parameters.Count - 1 do
    Result := ';' + Self.Parameters[I];
end;

//* TIdSipHeader Protected methods *********************************************

function TIdSipHeader.GetValue: String;
begin
  Result := fValue;
end;

procedure TIdSipHeader.SetValue(const Value: String);
var
  ParamName:  String;
  ParamValue: String;
  S:          String;
begin
  fValue := Value;

  S := Value;
  Fetch(S, ';');
  if (Pos(';', S) = 0) then begin
    ParamValue := S;
    ParamName := Fetch(ParamValue, '=');

    Self.Params[ParamName] := ParamValue;
  end
  else begin
    while (S <> '') do begin
      ParamValue := Fetch(S, ';');
      ParamName  := Fetch(ParamValue, '=');

      Self.Params[ParamName] := ParamValue;
    end;
  end;
end;

//* TIdSipHeader Private methods ***********************************************

function TIdSipHeader.GetParam(const Name: String): String;
begin
  Result := Self.Parameters.Values[Name];
end;

procedure TIdSipHeader.SetParam(const Name, Value: String);
begin
  Self.Parameters.Values[Name] := Value;
end;

procedure TIdSipHeader.SetParameters(const Value: TStrings);
begin
  Self.Parameters.Assign(Value);
end;

//******************************************************************************
//* TIdSipHeaders                                                              *
//******************************************************************************
//* TIdSipHeaders Public methods ***********************************************

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
  // based on HeaderName, construct the appropriate header type
  Result := TIdSipHeader.Create;
  Result.Name := HeaderName;

  Self.List.Add(Result);
end;

function TIdSipHeaders.Count: Integer;
begin
  Result := Self.List.Count;
end;

function TIdSipHeaders.HasHeader(const HeaderName: String): Boolean;
begin
  Result := Self.IndexOf(HeaderName) <> -1;
end;

//* TIdSipHeaders Private methods **********************************************

function TIdSipHeaders.GetHeaders(const Name: String): TIdSipHeader;
var
  I: Integer;
begin
  I := Self.IndexOf(Name);

  if (I = -1) then
    Result := nil
  else
    Result := Self.List[I] as TIdSipHeader;
end;

function TIdSipHeaders.IndexOf(const HeaderName: String): Integer;
var
  Found: Boolean;
begin
  Result := 0;
  Found := false;
  while (Result < Self.List.Count) and not Found do
    if ((Self.List[Result] as TIdSipHeader).Name <> HeaderName) then
      Inc(Result)
    else
      Found := true;

  if (Result = Self.List.Count) then
    Result := -1;
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

//* TIdSipAddressHeader Private methods ****************************************

procedure TIdSipAddressHeader.SetAddress(const Value: TIdURI);
begin
  fAddress.URI := Value.URI;
end;

//******************************************************************************
//* TIdSipViaHeader                                                            *
//******************************************************************************
//* TIdSipViaHeader Public methods *********************************************

function TIdSipViaHeader.DefaultPortForTransport(const T: TIdSipTransportType): Cardinal;
begin
  if (T = sttTLS) then
    Result := IdPort_SIP_TLS
  else
    Result := IdPORT_SIP;
end;

function TIdSipViaHeader.IsDefaultPortForTransport(const Port: Cardinal; const T: TIdSipTransportType): Boolean;
begin
  Result := ((T = sttTLS) and (Port = IdPORT_SIP_TLS))
         or (Port = IdPORT_SIP);
end;

function TIdSipViaHeader.IsEqualTo(const Header: TIdSipHeader): Boolean;
begin
  Result := inherited IsEqualTo(Header);
{
  if not (Header is Self.ClassType) then
    Result := false
  else
    Result := (Self.Host = TIdSipViaHeader(Header).Host)
          and (Self.Port = TIdSipViaHeader(Header).Port)
          and (Self.SipVersion = TIdSipViaHeader(Header).SipVersion)
          and (Self.Transport = TIdSipViaHeader(Header).Transport);
}
end;

//* TIdSipViaHeader Private methods ********************************************

function TIdSipViaHeader.GetValue: String;
begin
  Result := Self.SipVersion + '/' + TransportToStr(Self.Transport)
          + ' ' + Self.Host;

  if not Self.IsDefaultPortForTransport(Self.Port, Self.Transport) then
    Result := Result + ':' + IntToStr(Self.Port);

  Result := Result + Self.ParamsAsString;
end;

procedure TIdSipViaHeader.SetValue(const Value: String);
var
  Token: String;
  S:     String;
begin
  inherited SetValue(Value);

  S := Value;

  Token := Trim(Fetch(S, '/')) + '/';
  Token := Token + Trim(Fetch(S, '/'));
  Self.SipVersion := Token;

  S := Trim(S);
  Token := Trim(Fetch(S, ' '));
  Self.Transport := StrToTransport(Token);
  Token := Trim(Fetch(S, ';'));
  Self.Host := Fetch(Token, ':');
  
  if (Token = '') then 
    Self.Port := Self.DefaultPortForTransport(Self.Transport)
  else
    Self.Port := StrToInt(Token);
end;

//******************************************************************************
//* TIdSipPath                                                                 *
//******************************************************************************
//* TIdSipPath Public methods **************************************************

constructor TIdSipPath.Create;
begin
  inherited Create;

  fViaHeaders := TObjectList.Create(true);
end;

destructor TIdSipPath.Destroy;
begin
  fViaHeaders.Free;

  inherited Destroy;
end;

procedure TIdSipPath.Add(Hop: TIdSipViaHeader);
var
  NewHop: TIdSipViaHeader;
begin
  NewHop := TIdSipViaHeader.Create;
  NewHop.Assign(Hop);

  Self.Headers.Add(NewHop);
end;

function TIdSipPath.FirstHop: TIdSipViaHeader;
begin
  if (Self.Length > 0) then
    Result := Self.Headers[Self.Headers.Count - 1] as TIdSipViaHeader
  else
    Result := nil;
end;

function TIdSipPath.LastHop: TIdSipViaHeader;
begin
  if (Self.Length > 0) then
    Result := Self.Headers[0] as TIdSipViaHeader
  else
    Result := nil;
end;

function TIdSipPath.Length: Integer;
begin
  Result := Self.Headers.Count;
end;

//******************************************************************************
//* TIdSipMessage                                                              *
//******************************************************************************
//* TIdSipMessage Public methods ***********************************************

constructor TIdSipMessage.Create;
begin
  inherited Create;

  fOtherHeaders := TStringList.Create;
  fPath := TIdSipPath.Create;

  Self.MaxForwards := DefaultMaxForwards;
  Self.SIPVersion := IdSipParser.SIPVersion;
end;

destructor TIdSipMessage.Destroy;
begin
  fPath.Free;
  fOtherHeaders.Free;

  inherited Destroy;
end;

procedure TIdSipMessage.ReadBody(const S: TStream);
begin
  SetLength(fBody, Self.ContentLength);
  S.Read(fBody[1], Self.ContentLength);
end;

//* TIdSipMessage Private methods **********************************************

procedure TIdSipMessage.SetPath(const Value: TIdSipPath);
begin

end;

//*******************************************************************************
//* TIdSipRequest                                                               *
//*******************************************************************************
//* TIdSipRequest Public methods ************************************************

function TIdSipRequest.AsString: String;
var
  I: Integer;
begin
  Result := Format(RequestLine, [Self.Method, Self.Request, Self.SIPVersion]);

  for I := 0 to Self.OtherHeaders.Count - 1 do
    Result := Result + Self.OtherHeaders[I] + EOL;

  Result := Result + ContentLengthHeaderFull + ':' + CHAR32 + IntToStr(Self.ContentLength) + EOL;
  Result := Result + EOL;
  Result := Result + Self.Body;
end;

function TIdSipRequest.MalformedException: ExceptClass;
begin
  Result := EBadRequest;
end;

//*******************************************************************************
//* TIdSipResponse                                                              *
//*******************************************************************************
//* TIdSipResponse Public methods ***********************************************

function TIdSipResponse.AsString: String;
var
  I: Integer;
begin
  Result := Format(StatusLine, [Self.SIPVersion, Self.StatusCode, Self.StatusText]);

  for I := 0 to Self.OtherHeaders.Count - 1 do
    Result := Result + Self.OtherHeaders[I] + EOL;

  Result := Result + ContentLengthHeaderFull + ':' + CHAR32 + IntToStr(Self.ContentLength) + EOL;
  Result := Result + EOL;
  Result := Result + Self.Body;
end;

function TIdSipResponse.MalformedException: ExceptClass;
begin
  Result := EBadResponse;
end;

//* TIdSipResponse Private methods **********************************************

procedure TIdSipResponse.SetStatusCode(const Value: Integer);
begin
  Self.fStatusCode := Value;

  case Self.StatusCode of
    SIPTrying:                           Self.StatusText := RSSIPTrying;
    SIPRinging:                          Self.StatusText := RSSIPRinging;
    SIPCallIsBeingForwarded:             Self.StatusText := RSSIPCallIsBeingForwarded;
    SIPQueued:                           Self.StatusText := RSSIPQueued;
    SIPSessionProgess:                   Self.StatusText := RSSIPSessionProgess;
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
    SIPTemporarilyNotAvailable:          Self.StatusText := RSSIPTemporarilyNotAvailable;
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
//* TIdSipParser                                                               *
//******************************************************************************
//* TIdSipParser Public methods ************************************************

constructor TIdSipParser.Create;
begin
  inherited Create;

  Self.ResetCurrentLine;
end;

function TIdSipParser.CurrentLine: Cardinal;
begin
  Result := fCurrentLine;
end;

function TIdSipParser.Eof: Boolean;
var
  C: Char;
  N: Integer;
begin
  N := Self.Source.Read(C, 1);
  Result := (N = 0);

  if not Result then
    Self.Source.Seek(-1, soFromCurrent);
end;

function TIdSipParser.GetHeaderName(Header: String): String;
begin
  Result := Trim(Fetch(Header, ':'));
end;

function TIdSipParser.GetHeaderNumberValue(const Msg: TIdSipMessage; const Header: String): Cardinal;
var
  Name:  String;
  Value: String;
  E:     Integer;
begin
  Name := Self.GetHeaderName(Header);
  Value := Self.GetHeaderValue(Header);
  Val(Value, Result, E);
  if (E <> 0) then
    raise Msg.MalformedException.Create(Format(MalformedToken, [Name, Header]));
end;

function TIdSipParser.GetHeaderValue(Header: String): String;
begin
  if (Pos(':', Header) = 0) then
    Result := ''
  else begin
    Result := Header;
    Fetch(Result, ':');
    Result := Trim(Result);
  end;
end;

function TIdSipParser.IsCallID(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, CallIDHeaderFull)
         or IsEqual(Name, CallIDHeaderShort);
end;

function TIdSipParser.IsContentLength(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, ContentLengthHeaderFull)
         or IsEqual(Name, ContentLengthHeaderShort);
end;

function TIdSipParser.IsMaxForwards(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, MaxForwardsHeader);
end;

function TIdSipParser.IsMethod(Method: String): Boolean;
var
  I: Integer;
  function IsAlphaNum(const C: Char): Boolean;
  begin
    Result := C in ['a'..'z', 'A'..'Z', '0'..'9'];
  end;
begin
  Result := (Length(Method) > 0) and IsAlphaNum(Method[1]);

  if (Result) then begin
    I := 2;
    while (I < Length(Method)) and Result do begin
      Result := Result and (IsAlphaNum(Method[I])
                            or (Method[I] in ['-', '.', '!', '%', '*', '_', '+', '`', '''', '~']));



      Inc(I);
    end;
  end;
end;

function TIdSipParser.IsNumber(Number: String): Boolean;
var
  I: Integer;
begin
  Result := Length(Number) > 0;

  if (Result) then
    for I := 1 to Length(Number) do
      Result := Result and (Number[I] in ['0'..'9']);
end;

function TIdSipParser.IsTo(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, ToHeaderFull)
         or IsEqual(Name, ToHeaderShort);
end;

function TIdSipParser.IsVia(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, ViaHeaderFull)
         or IsEqual(Name, ViaHeaderShort);
end;

function TIdSipParser.IsSipVersion(Version: String): Boolean;
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

function TIdSipParser.MakeBadRequestResponse(const Reason: String): TIdSipResponse;
begin
  Result := TIdSipResponse.Create;
  Result.StatusCode := SIPBadRequest;
  Result.StatusText := Reason;
  Result.SipVersion := SIPVersion;
end;

function TIdSipParser.ParseAndMakeMessage: TIdSipMessage;
var
  FirstLine: String;
  FirstToken: String;
begin
  if not Self.Eof then begin
    FirstLine := Self.PeekLine;
    FirstToken := Fetch(FirstLine);
    FirstToken := Fetch(FirstToken, '/');

    // It's safe to do this because we know the string starts with "SIP/",
    // and the "/" is not allowed in an action name.
    if (FirstToken = SipName) then begin
      Result := TIdSipResponse.Create;
      Self.ParseResponse(Result as TIdSipResponse);
    end
    else begin
      Result := TIdSipRequest.Create;
      Self.ParseRequest(Result as TIdSipRequest);
    end;
  end
  else
    raise EParser.Create(EmptyInputStream);
end;

procedure TIdSipParser.ParseRequest(const Request: TIdSipRequest);
begin
  Self.InitialiseMessage(Request);
  if not Self.Eof then begin
    Self.ResetCurrentLine;
    Self.ParseRequestLine(Request);
    Self.ParseHeaders(Request);
  end;
end;

procedure TIdSipParser.ParseResponse(const Response: TIdSipResponse);
begin
  Self.InitialiseMessage(Response);
  if not Self.Eof then begin
    Self.ResetCurrentLine;
    Self.ParseStatusLine(Response);
    Self.ParseHeaders(Response);
  end;
end;

function TIdSipParser.Peek: Char;
begin
  Result := Self.ReadOctet;
  Self.Source.Seek(-1, soFromCurrent);
end;

function TIdSipParser.PeekLine: String;
var
  C: Char;
begin
  Result := '';
  while not Self.Eof and not (Self.Peek = #13) do begin
    C := Self.ReadOctet;
    Result := Result + C;
  end;

  Self.Source.Seek(-Length(Result), soFromCurrent);
end;

function TIdSipParser.ReadOctet: Char;
begin
  Self.Source.Read(Result, 1);
end;

function TIdSipParser.ReadOctets(Count: Cardinal): String;
begin
  Result := '';
  while not Self.Eof and (Count > 0) do begin
    Result := Result + Self.ReadOctet;
    Dec(Count);
  end;
end;

function TIdSipParser.ReadLn: String;
var
  C: Char;
begin
  Result := Self.PeekLine;
  Self.Source.Seek(Length(Result), soFromCurrent);

  if (not Self.Eof) then begin
    C := Self.ReadOctet;
    Assert(C = #13, Format(DamagedLineEnd, [Self.CurrentLine, '#$0D', '#$' + IntToHex(Ord(C), 2)]));
    C := Self.ReadOctet;
    Assert(C = #10, Format(DamagedLineEnd, [Self.CurrentLine, '#$0A', '#$' + IntToHex(Ord(C), 2)]));
  end;

  Self.IncCurrentLine;
end;

function TIdSipParser.ReadFirstNonBlankLn: String;
begin
  Result := Self.ReadLn;
  while (Result = '') and not Self.Eof do
    Result := Self.ReadLn;
end;

//* TIdSipParser Private methods ***********************************************

procedure TIdSipParser.AddHeader(const Msg: TIdSipMessage; Header: String);
begin
  Msg.OtherHeaders.Add(Header);
end;

procedure TIdSipParser.IncCurrentLine;
begin
  Inc(fCurrentLine);
end;

procedure TIdSipParser.InitialiseMessage(Msg: TIdSipMessage);
begin
  Msg.ContentLength := 0;
  Msg.MaxForwards   := 0;
  Msg.OtherHeaders.Clear;
  Msg.SIPVersion    := '';
end;

procedure TIdSipParser.ParseHeader(const Msg: TIdSipMessage; const Header: String);
var
  N: Integer;
begin
  if Self.IsCallID(Header) then
    Msg.CallID := Self.GetHeaderValue(Header)
  else if Self.IsContentLength(Header) then
    Msg.ContentLength := Self.GetHeaderNumberValue(Msg, Header)
  else if Self.IsMaxForwards(Header) then begin
    N := Self.GetHeaderNumberValue(Msg, Header);
    if (N < 0) or (N > 255) then
      raise Msg.MalformedException.Create(Format(MalformedToken, [Self.GetHeaderName(Header), Header]));
    Msg.MaxForwards := N;
  end
//  else if Self.IsTo(Header) then
  else if Self.IsVia(Header) then
    Self.ParseViaHeader(Msg, Self.GetHeaderValue(Header))
  else
    Self.AddHeader(Msg, Header);
end;

procedure TIdSipParser.ParseHeaders(const Msg: TIdSipMessage);
var
  FoldedHeader: String;
  Line:         String;
begin
  FoldedHeader := Self.ReadLn;
  if (FoldedHeader <> '') then begin
    Line := Self.ReadLn;
    while (Line <> '') do begin
      if (Line[1] in [' ', #9]) then begin
        FoldedHeader := FoldedHeader + ' ' + Trim(Line);
        Line := Self.ReadLn;
      end
      else begin
        Self.ParseHeader(Msg, FoldedHeader);
        FoldedHeader := Line;
        Line := Self.ReadLn;
      end;
    end;
    if (FoldedHeader <> '') then
      Self.ParseHeader(Msg, FoldedHeader);
  end;

  //TODO: check for required headers - To, From, Call-ID, Call-Seq, Max-Forwards, Via
end;

procedure TIdSipParser.ParseRequestLine(const Request: TIdSipRequest);
var
  Line:   String;
  Tokens: TStrings;
begin
  // chew up leading blank lines (Section 7.5)
  Line := Self.ReadFirstNonBlankLn;

  Tokens := TStringList.Create;
  try
    BreakApart(Line, ' ', Tokens);

    if (Tokens.Count > 3) then
      raise Request.MalformedException.Create(RequestUriNoSpaces)
    else if (Tokens.Count < 3) then
      raise Request.MalformedException.Create(Format(MalformedToken, ['Request-Line', Line]));

    Request.Method := Tokens[0];
    // we want to check the Method
    if not Self.IsMethod(Request.Method) then
      raise Request.MalformedException.Create(Format(MalformedToken, ['Method', Request.Method]));

    Request.Request := Tokens[1];

    if (Request.Request[1] = '<') and (Request.Request[Length(Request.Request)] = '>') then
      raise Request.MalformedException.Create(RequestUriNoAngleBrackets);

    Request.SIPVersion := Tokens[2];

    if not Self.IsSipVersion(Request.SIPVersion) then
      raise Request.MalformedException.Create(Format(InvalidSipVersion, [Request.SIPVersion]));
  finally
    Tokens.Free;
  end;
end;

procedure TIdSipParser.ParseStatusLine(const Response: TIdSipResponse);
var
  Line:   String;
  StatusCode: String;
begin
  // chew up leading blank lines (Section 7.5)
  Line := Self.ReadFirstNonBlankLn;

  Response.SIPVersion := Fetch(Line);
  if not Self.IsSipVersion(Response.SIPVersion) then
    raise Response.MalformedException.Create(Format(InvalidSipVersion, [Response.SIPVersion]));

  StatusCode := Fetch(Line);
  if not Self.IsNumber(StatusCode) then
    raise Response.MalformedException.Create(Format(InvalidStatusCode, [StatusCode]));

  Response.StatusCode := StrToIntDef(StatusCode, BadStatusCode);

  Response.StatusText := Line;
end;

procedure TIdSipParser.ParseViaHeader(const Msg: TIdSipMessage; ViaParms: String);
var
  NewHeader: TIdSipHeader;
  OneVia:    String;
begin
  while (ViaParms <> '') do begin
    OneVia := Fetch(ViaParms, ',');

    NewHeader := TIdSipViaHeader.Create;
    try
      NewHeader.Name  := 'Via';
      NewHeader.Value := OneVia;
    except
      NewHeader.Free;
      NewHeader := nil;
    end;

    if Assigned(NewHeader) then
      Msg.Path.Add(NewHeader as TIdSipViaHeader)
  end;
end;

procedure TIdSipParser.ResetCurrentLine;
begin
  fCurrentLine := 1;
end;

end.
