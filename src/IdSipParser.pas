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

    property Parameters: TStrings read fParams write SetParameters;
  protected
    function  GetValue: String; virtual;
    procedure SetValue(const Value: String); virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    function  IndexOfParam(Name: String): Integer;
    function  IsEqualTo(const Header: TIdSipHeader): Boolean; virtual;
    function  ParamCount: Integer;
    function  ParamsAsString: String; virtual;

    property Name:                       String read fName write fName;
    property Value:                      String read GetValue write SetValue;
    property Params[const Name: String]: String read GetParam write SetParam;
  end;

  TIdSipHeaderClass = class of TIdSipHeader;

  TIdSipAddressHeader = class(TIdSipHeader)
  private
    fAddress:     TIdURI;
    fDisplayName: String;

    function  NeedsQuotes(Name: String): Boolean;
    function  QuoteStringIfNecessary(const Name: String): String;
    procedure SetAddress(const Value: TIdURI);
  protected
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function DecodeQuotedStr(const S: String): String;
    function EncodeQuotedStr(const S: String): String;

    property Address:     TIdURI read fAddress write SetAddress;
    property DisplayName: String read fDisplayName write fDisplayName;
  end;

  TIdSipCSeqHeader = class(TIdSipHeader)
  private
    fMethod:     String;
    fSequenceNo: Cardinal;
  protected
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    property Method: String       read fMethod     write fMethod;
    property SequenceNo: Cardinal read fSequenceNo write fSequenceNo;
  end;

  TIdSipDateHeader = class(TIdSipHeader)
  private
    fIsRelativeTime: Boolean;
    fRelativeTime:   Cardinal;
    fAbsoluteTime:   TDateTime;

    procedure SetAbsoluteTime(Value: String);
    procedure SetRelativeTime(const Value: String);
  protected
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    property IsRelativeTime: Boolean   read fIsRelativeTime write fIsRelativeTime;
    property RelativeTime:   Cardinal  read fRelativeTime write fRelativeTime;
    property AbsoluteTime:   TDateTime read fAbsoluteTime write fAbsoluteTime;
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

  TIdSipHeaderMap = class(TObject)
    fHeaderName:  String;
    fHeaderClass: TIdSipHeaderClass;
  public
    constructor Create(HeaderName: String; HeaderClass: TIdSipHeaderClass);
    property HeaderName:  String            read fHeaderName;
    property HeaderClass: TIdSipHeaderClass read fHeaderClass;
  end;

  TIdSipHeaders = class(TObject)
  private
    List: TObjectList;

    class function HeaderTypes: TObjectList;

    function  ConstructHeader(const HeaderName: String): TIdSipHeader;
    function  GetHeaders(const Name: String): TIdSipHeader;
    function  GetItems(const I: Integer): TIdSipHeader;
    function  GetValues(const Name: String): String;
    function  IndexOf(const HeaderName: String): Integer;
    procedure SetValues(const Header, Value: String);
  public
    class function GetHeaderName(Header: String): String;
    class function IsCallID(Header: String): Boolean;
    class function IsContact(Header: String): Boolean;
    class function IsContentLength(Header: String): Boolean;
    class function IsCSeq(Header: String): Boolean;
    class function IsFrom(Header: String): Boolean;
    class function IsMaxForwards(Header: String): Boolean;
    class function IsTo(Header: String): Boolean;
    class function IsVia(Header: String): Boolean;

    constructor Create; virtual;
    destructor  Destroy; override;

    function  Add(const HeaderName: String): TIdSipHeader;
    function  AsString: String;
    procedure Clear;
    function  Count: Integer;

    function  HasHeader(const HeaderName: String): Boolean;

    property  Headers[const Name: String]:  TIdSipHeader read GetHeaders; default;
    property  Items[const I: Integer]:      TIdSipHeader read GetItems;
    property  Values[const Header: String]: String read GetValues write SetValues;
  end;

  TIdSipPath = class(TObject)
  private
    fHeaders: TIdSipHeaders;

    property Headers: TIdSipHeaders read fHeaders write fHeaders;
  public
    constructor Create(const Headers: TIdSipHeaders);

    procedure Add(Hop: TIdSipViaHeader);
    function  FirstHop: TIdSipViaHeader;
    function  LastHop: TIdSipViaHeader;
    function  Length: Integer;
  end;

  TIdSipMessage = class(TObject)
  private
    fBody:       String;
    fPath:       TIdSipPath;
    fHeaders:    TIdSipHeaders;
    fSIPVersion: String;

    function  GetCallID: String;
    function  GetContentLength: Cardinal;
    function  GetCSeq: TIdSipCSeqHeader;
    function  GetFrom: TIdSipAddressHeader;
    function  GetMaxForwards: Byte;
    function  GetTo: TIdSipAddressHeader;
    procedure SetCallID(const Value: String);
    procedure SetContentLength(const Value: Cardinal);
    procedure SetCSeq(const Value: TIdSipCSeqHeader);
    procedure SetFrom(const Value: TIdSipAddressHeader);
    procedure SetMaxForwards(const Value: Byte);
    procedure SetPath(const Value: TIdSipPath);
    procedure SetTo(const Value: TIdSipAddressHeader);
  protected
    function FirstLine: String; virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;

    function  AsString: String;
    function  MalformedException: ExceptClass; virtual; abstract;
    procedure ReadBody(const S: TStream);

    property Body:          String              read fBody write fBody;
    property CallID:        String              read GetCallID write SetCallID;
    property ContentLength: Cardinal            read GetContentLength write SetContentLength;
    property CSeq:          TIdSipCSeqHeader    read GetCSeq write SetCSeq;
    property From:          TIdSipAddressHeader read GetFrom write SetFrom;
    property Headers:       TIdSipHeaders       read fHeaders;
    property MaxForwards:   Byte                read GetMaxForwards write SetMaxForwards;
    property Path:          TIdSipPath          read fPath write SetPath;
    property SIPVersion:    String              read fSIPVersion write fSIPVersion;
    property ToHeader:      TIdSipAddressHeader read GetTo write SetTo;
  end;

  TIdSipRequest = class(TIdSipMessage)
  private
    fMethod:     String;
    fRequest:    String;
  protected
    function FirstLine: String; override;
  public
    function MalformedException: ExceptClass; override;

    property Method:  String read fMethod write fMethod;
    property Request: String read fRequest write fRequest;
  end;

  TIdSipResponse = class(TIdSipMessage)
  private
    fStatusCode:    Integer;
    fStatusText:    String;

    procedure SetStatusCode(const Value: Integer);
  protected
    function FirstLine: String; override;
  public
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
    procedure ParseContactHeader(const Msg: TIdSipMessage; ContactParms: String);
    procedure ParseHeader(const Msg: TIdSipMessage; const Header: String);
    procedure ParseHeaders(const Msg: TIdSipMessage);
    procedure ParseRequestLine(const Request: TIdSipRequest);
    procedure ParseStatusLine(const Response: TIdSipResponse);
    procedure ParseViaHeader(const Msg: TIdSipMessage; ViaParms: String);
    procedure ResetCurrentLine;
  public
    class function IsMethod(Method: String): Boolean;
    class function IsNumber(Number: String): Boolean;
    class function IsToken(const Token: String): Boolean;
    constructor Create;

    function  CanonicaliseName(HeaderName: String): String;
    function  CurrentLine: Cardinal;
    function  Eof: Boolean;
    function  GetHeaderName(Header: String): String;
    function  GetHeaderNumberValue(const Msg: TIdSipMessage; const Header: String): Cardinal;
    function  GetHeaderValue(Header: String): String;
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
  EBadHeader = class(EParser);
  EBadRequest = class(EParser);
  EBadResponse = class(EParser);

const
  BadStatusCode             = -1;
  BadSyntax                 = 'Bad syntax';
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
  UnmatchedQuotes           = 'Unmatched quotes';

const
  CallIDHeaderFull         = 'Call-ID';
  CallIDHeaderShort        = 'i';
  ContactHeaderFull        = 'Contact';
  ContactHeaderShort       = 'm';
  ContentLengthHeaderFull  = 'Content-Length';
  ContentLengthHeaderShort = 'l';
  ContentTypeHeaderFull    = 'Content-Type';
  ContentTypeHeaderShort   = 'c';
  CSeqHeader               = 'CSeq';
  DefaultMaxForwards       = 70;
  ExpiresHeader            = 'Expires';
  FromHeaderFull           = 'From';
  FromHeaderShort          = 'f';
  MaxForwardsHeader        = 'Max-Forwards';
  MethodAck                = 'ACK';
  MethodBye                = 'BYE';
  MethodCancel             = 'CANCEL';
  MethodInvite             = 'INVITE';
  MethodOptions            = 'OPTIONS';
  MethodRegister           = 'REGISTER';
  SipName                  = 'SIP';
  SubjectHeader            = 'Subject';
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

// class variables
var
  GIdSipHeadersMap: TObjectList;

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
  Result := Self.Parameters.Count;
end;

function TIdSipHeader.ParamsAsString: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Self.Parameters.Count - 1 do
    Result := Result + ';' + Self.Parameters[I];
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
  Self.Parameters.Clear;

  S := Value;

  if (Pos(';', S) = 0) then begin
    fValue := S;
  end
  else begin
    fValue := Trim(Fetch(S, ';'));
    if (Pos(';', S) = 0) then begin
      ParamValue := S;
      ParamName := Trim(Fetch(ParamValue, '='));

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
end;

//* TIdSipHeader Private methods ***********************************************

function TIdSipHeader.GetParam(const Name: String): String;
var
  I: Integer;
begin
  I := Self.IndexOfParam(Name);

  Result := Self.Parameters[I];
  Fetch(Result, '=');
  Result := Trim(Result);
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

function TIdSipAddressHeader.DecodeQuotedStr(const S: String): String;
var
  I: Integer;
  FoundSlash: Boolean;
begin
  // in summary:
  // '\' is illegal, '%s\' is illegal.

  Result := S;

  if (Result <> '') then begin
    if (Result = '\') then
      raise EBadHeader.Create(Self.Name);

    if (Length(Result) >= 2) and (Result[Length(Result)] = '\') and (Result[Length(Result) - 1] <> '\') then
      raise EBadHeader.Create(Self.Name);

    // We use "<" and not "<=" because if a \ is the last character we have
    // a malformed string. Too, this allows use to Result[I + 1]
    I := 1;
    while I < Length(Result) do begin
      FoundSlash := Result[I] = '\';
      if (FoundSlash) then begin
        Delete(Result, I, 1);

        // protect '\\'
        if (FoundSlash) then begin
          Inc(I);
        end;
      end
      else
        Inc(I);
    end;
  end;
end;

function TIdSipAddressHeader.EncodeQuotedStr(const S: String): String;
begin
  Result := S;
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll, rfIgnoreCase]);
end;

//* TIdSipAddressHeader Protected methods **************************************

function TIdSipAddressHeader.GetValue: String;
var
  URI: String;
begin
  Result := Self.DisplayName;
  if (Pos('"', Result) > 0) or (Pos('\', Result) > 0) then
    Result := Self.EncodeQuotedStr(Result);

  Result := Self.QuoteStringIfNecessary(Result);

//  if (Pos(' ', Result) > 0) then
//    Result := '"' + Result + '"';

  URI := Self.Address.GetFullURI;
  if (Pos(';', URI) > 0) or (Pos(',', URI) > 0) or (Pos('?', URI) > 0) or (Result <> '') then
    URI := '<' + URI + '>';

  if (Result = '') then
    Result := URI
  else
    Result := Result + ' ' + URI;
end;

procedure TIdSipAddressHeader.SetValue(const Value: String);
var
  Name: String;
  S:    String;
  PreserveSemicolon: Boolean;
begin
  PreserveSemicolon := false;
  Self.DisplayName := '';
  Self.Address.URI := '';

  S := Trim(Value);

  if (Pos('<', S) > 0) then begin
    if (S[1] = '"') then begin
      Name := Trim(Fetch(S, '<'));
      Delete(Name, 1, 1);

      if (Pos('"', Name) = 0) then
        raise EBadHeader.Create(Self.Name);

      Name := Copy(Name, 1, RPos('"', Name, -1) - 1);

      // There was an encoded ", which MUST NOT match the opening "
      if (Name <> '') and (Name[Length(Name)] = '\') then
        raise EBadHeader.Create(Self.Name);

        Self.DisplayName := Self.DecodeQuotedStr(Name);
    end else begin
      Self.DisplayName := Trim(Fetch(S, '<'));
      if Self.NeedsQuotes(Self.DisplayName) then
        raise EBadHeader.Create(Self.Name);
    end;

    Self.Address.URI := Trim(Fetch(S, '>'));
  end
  else begin
    // any semicolons in a URI not in angle brackets indicate that the
    // HEADER has the parameters, not the URI
    PreserveSemicolon := Pos(';', S) > 0;
    Self.Address.URI := Trim(Fetch(S, ';'));
  end;

  // watch out for HEADER parameters versus URI parameters

  if PreserveSemicolon then
    S := ';' + S;
  inherited SetValue(S);
end;

//* TIdSipAddressHeader Private methods ****************************************

function TIdSipAddressHeader.NeedsQuotes(Name: String): Boolean;
var
  DontQuote: Boolean;
  Token:     String;
begin
  if (Name = '') then
    Result := false
  else begin
    DontQuote := true;

    while (Name <> '') do begin
      Token := Fetch(Name, ' ');
      DontQuote := DontQuote and TIdSipParser.IsToken(Token);
    end;

    Result := not DontQuote;
  end;
end;

function TIdSipAddressHeader.QuoteStringIfNecessary(const Name: String): String;
begin
  Result := Name;
  if Self.NeedsQuotes(Name) then
    Result := '"' + Result + '"'
end;

procedure TIdSipAddressHeader.SetAddress(const Value: TIdURI);
begin
  fAddress.URI := Value.URI;
end;

//******************************************************************************
//* TIdSipCSeqHeader                                                           *
//******************************************************************************
//* TIdSipCSeqHeader Protected methods *****************************************

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
    raise EBadHeader.Create(Self.Name);

  Self.SequenceNo := N;

  Token := Trim(S);
  if not TIdSipParser.IsMethod(Token) then
    raise EBadHeader.Create(Self.Name);

    Self.Method := Token;
end;

//******************************************************************************
//* TIdSipDateHeader                                                           *
//******************************************************************************
//* TIdSipDateHeader Protected methods *****************************************

function TIdSipDateHeader.GetValue: String;
begin
  Result := inherited GetValue;
end;

procedure TIdSipDateHeader.SetValue(const Value: String);
begin
  inherited SetValue(Value);

  if TIdSipParser.IsNumber(Value) then
    Self.SetRelativeTime(Value)
  else
    Self.SetAbsoluteTime(Value);
end;

//* TIdSipDateHeader Private methods *******************************************

procedure TIdSipDateHeader.SetAbsoluteTime(Value: String);
begin
  fIsRelativeTime := false;

  
end;

procedure TIdSipDateHeader.SetRelativeTime(const Value: String);
var
  N: Cardinal;
  E: Integer;
begin
  fIsRelativeTime := true;

  Val(Value, N, E);
  if (E <> 0) then
    raise EBadHeader.Create(Self.Name);

  fRelativeTime := N;
  fAbsoluteTime := 0;
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
    raise EBadHeader.Create(Self.Name);

  fNumericValue := StrToInt(Value);

  inherited SetValue(Value);
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
end;

procedure TIdSipViaHeader.SetValue(const Value: String);
var
  Token: String;
  S:     String;
begin
  inherited SetValue(Value);

  S := Value;
  S := Fetch(S, ';', false);

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
//* TIdSipHeaders                                                              *
//******************************************************************************
//* TIdSipHeaders Public methods ***********************************************

class function TIdSipHeaders.GetHeaderName(Header: String): String;
begin
  Result := Trim(Fetch(Header, ':'));
end;

class function TIdSipHeaders.IsCallID(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, CallIDHeaderFull)
         or IsEqual(Name, CallIDHeaderShort);
end;

class function TIdSipHeaders.IsContact(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, ContactHeaderFull)
         or IsEqual(Name, ContactHeaderShort);
end;

class function TIdSipHeaders.IsContentLength(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, ContentLengthHeaderFull)
         or IsEqual(Name, ContentLengthHeaderShort);
end;

class function TIdSipHeaders.IsCSeq(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, CSeqHeader);
end;

class function TIdSipHeaders.IsFrom(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, FromHeaderFull)
         or IsEqual(Name, FromHeaderShort);
end;

class function TIdSipHeaders.IsMaxForwards(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, MaxForwardsHeader);
end;

class function TIdSipHeaders.IsTo(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, ToHeaderFull)
         or IsEqual(Name, ToHeaderShort);
end;

class function TIdSipHeaders.IsVia(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, ViaHeaderFull)
         or IsEqual(Name, ViaHeaderShort);
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

  Result.Name := HeaderName;

  Self.List.Add(Result);
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

function TIdSipHeaders.Count: Integer;
begin
  Result := Self.List.Count;
end;

function TIdSipHeaders.HasHeader(const HeaderName: String): Boolean;
begin
  Result := Self.IndexOf(HeaderName) <> -1;
end;

//* TIdSipHeaders Private methods **********************************************

class function TIdSipHeaders.HeaderTypes: TObjectList;
begin
  if not Assigned(GIdSipHeadersMap) then begin
    GIdSipHeadersMap := TObjectList.Create(true);
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContactHeaderFull,  TIdSipAddressHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContactHeaderShort, TIdSipAddressHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(CSeqHeader,         TIdSipCSeqHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ExpiresHeader,      TIdSipNumericHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(FromHeaderFull,     TIdSipAddressHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(FromHeaderShort,    TIdSipAddressHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(MaxForwardsHeader,  TIdSipNumericHeader));    
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ToHeaderFull,       TIdSipAddressHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ToHeaderShort,      TIdSipAddressHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ViaHeaderFull,      TIdSipViaHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ViaHeaderShort,     TIdSipViaHeader));
  end;

  Result := GIdSipHeadersMap;
end;

function TIdSipHeaders.ConstructHeader(const HeaderName: String): TIdSipHeader;
var
  I: Integer;
begin
  Result := nil;
  I := 0;
  while (I < Self.HeaderTypes.Count) and not Assigned(Result) do
    if IsEqual(TIdSipHeaderMap(Self.HeaderTypes[I]).HeaderName, HeaderName) then
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

function TIdSipHeaders.GetItems(const I: Integer): TIdSipHeader;
begin
  Result := Self.List[I] as TIdSipHeader;
end;

function TIdSipHeaders.GetValues(const Name: String): String;
var
  I: Integer;
begin
  I := Self.IndexOf(Name);

  if (I = -1) then begin
    Result := Self.Add(Name).Value;
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
//* TIdSipPath                                                                 *
//******************************************************************************
//* TIdSipPath Public methods **************************************************

constructor TIdSipPath.Create(const Headers: TIdSipHeaders);
begin
  inherited Create;

  Self.Headers := Headers;
end;

procedure TIdSipPath.Add(Hop: TIdSipViaHeader);
begin
  Self.Headers.Add(ViaHeaderFull).Assign(Hop);
end;

function TIdSipPath.FirstHop: TIdSipViaHeader;
var
  I: Integer;
begin
  Result := nil;
  I := Self.Headers.Count - 1;

  while (I >= 0) and not Assigned(Result) do
    if TIdSipHeaders.IsVia(Self.Headers.Items[I].Name) then
      Result := Self.Headers.Items[I] as TIdSipViaHeader
    else
      Dec(I);
end;

function TIdSipPath.LastHop: TIdSipViaHeader;
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
end;

function TIdSipPath.Length: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Self.Headers.Count - 1 do
    if TIdSipHeaders.IsVia(Self.Headers.Items[I].Name) then
      Inc(Result);
end;

//******************************************************************************
//* TIdSipMessage                                                              *
//******************************************************************************
//* TIdSipMessage Public methods ***********************************************

constructor TIdSipMessage.Create;
begin
  inherited Create;

  fHeaders := TIdSipHeaders.Create;
  fPath := TIdSipPath.Create(Self.Headers);

//  Self.MaxForwards := DefaultMaxForwards;
  Self.SIPVersion  := IdSipParser.SIPVersion;
end;

destructor TIdSipMessage.Destroy;
begin
  fPath.Free;
  fHeaders.Free;

  inherited Destroy;
end;

function TIdSipMessage.AsString: String;
begin
  Result := Self.FirstLine;

  Result := Result + Self.Headers.AsString;

  Result := Result + EOL;
  Result := Result + Self.Body;
end;

procedure TIdSipMessage.ReadBody(const S: TStream);
begin
  // TODO: It is the responsibility of the transport to ensure that
  // Content-Length is set before this method is called.
  SetLength(fBody, Self.ContentLength);
  S.Read(fBody[1], Self.ContentLength);
end;

//* TIdSipMessage Private methods **********************************************

function TIdSipMessage.GetCallID: String;
begin
  Result := Self.Headers[CallIDHeaderFull].Value;
end;

function TIdSipMessage.GetContentLength: Cardinal;
begin
//  if (Self.Headers[ContentLengthHeaderFull].Value = '') then
//    Self.ContentLength := 0;

  Result := StrToInt(Self.Headers[ContentLengthHeaderFull].Value);
end;

function TIdSipMessage.GetCSeq: TIdSipCSeqHeader;
begin
  Result := Self.Headers[CSeqHeader] as TIdSipCSeqHeader;
end;

function TIdSipMessage.GetFrom: TIdSipAddressHeader;
begin
  Result := Self.Headers[FromHeaderFull] as TIdSipAddressHeader;
end;

function TIdSipMessage.GetMaxForwards: Byte;
begin
  if (Self.Headers[MaxForwardsHeader].Value = '') then
    Self.MaxForwards := DefaultMaxForwards;

  Result := StrToInt(Self.Headers[MaxForwardsHeader].Value);
end;

function TIdSipMessage.GetTo: TIdSipAddressHeader;
begin
  Result := Self.Headers[ToHeaderFull] as TIdSipAddressHeader;
end;

procedure TIdSipMessage.SetCallID(const Value: String);
begin
  Self.Headers[CallIDHeaderFull].Value := Value;
end;

procedure TIdSipMessage.SetContentLength(const Value: Cardinal);
begin
  Self.Headers[ContentLengthHeaderFull].Value := IntToStr(Value);
end;

procedure TIdSipMessage.SetCSeq(const Value: TIdSipCSeqHeader);
begin
  Self.CSeq.Assign(Value);
end;

procedure TIdSipMessage.SetFrom(const Value: TIdSipAddressHeader);
begin
  Self.Headers[FromHeaderFull].Assign(Value);
end;

procedure TIdSipMessage.SetMaxForwards(const Value: Byte);
begin
  Self.Headers[MaxForwardsHeader].Value := IntToStr(Value);
end;

procedure TIdSipMessage.SetPath(const Value: TIdSipPath);
begin
//  Self.Path.Assign(Value);
end;

procedure TIdSipMessage.SetTo(const Value: TIdSipAddressHeader);
begin
  Self.Headers[ToHeaderFull].Assign(Value);
end;

//*******************************************************************************
//* TIdSipRequest                                                               *
//*******************************************************************************
//* TIdSipRequest Public methods ************************************************

function TIdSipRequest.MalformedException: ExceptClass;
begin
  Result := EBadRequest;
end;

//* TIdSipRequest Protected methods ********************************************

function TIdSipRequest.FirstLine: String;
begin
  Result := Format(RequestLine, [Self.Method, Self.Request, Self.SIPVersion]);
end;

//*******************************************************************************
//* TIdSipResponse                                                              *
//*******************************************************************************
//* TIdSipResponse Public methods ***********************************************

function TIdSipResponse.MalformedException: ExceptClass;
begin
  Result := EBadResponse;
end;

//* TIdSipResponse Protected methods *******************************************

function TIdSipResponse.FirstLine: String;
begin
  Result := Format(StatusLine, [Self.SIPVersion, Self.StatusCode, Self.StatusText]);
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

class function TIdSipParser.IsMethod(Method: String): Boolean;
begin
  Result := Self.IsToken(Method);
end;

class function TIdSipParser.IsNumber(Number: String): Boolean;
var
  I: Integer;
begin
  Result := Number <> '';

  if (Result) then
    for I := 1 to Length(Number) do begin
      Result := Result and (Number[I] in ['0'..'9']);

      if not Result then Break;
    end;
end;

class function TIdSipParser.IsToken(const Token: String): Boolean;
var
  I: Integer;
begin
  Result := Token <> '';

  if Result then
    for I := 1 to Length(Token) do begin
      Result := Result and (Token[I] in ['a'..'z', 'A'..'Z', '0'..'9',
                                         '-', '.', '!', '%', '*', '_',
                                       '+', '`', '''', '~']);
      if not Result then Break;
    end;
end;

constructor TIdSipParser.Create;
begin
  inherited Create;

  Self.ResetCurrentLine;
end;

function TIdSipParser.CanonicaliseName(HeaderName: String): String;
begin
       if IsEqual(ContactHeaderFull,        HeaderName) then Result := ContactHeaderFull
  else if IsEqual(ContactHeaderShort,       HeaderName) then Result := ContactHeaderFull
  else if IsEqual(ContentLengthHeaderFull,  HeaderName) then Result := ContentLengthHeaderFull
  else if IsEqual(ContentLengthHeaderShort, HeaderName) then Result := ContentLengthHeaderFull
  else if IsEqual(CSeqHeader,               HeaderName) then Result := CSeqHeader
  else if IsEqual(FromHeaderFull,           HeaderName) then Result := FromHeaderFull
  else if IsEqual(FromHeaderShort,          HeaderName) then Result := FromHeaderFull
  else if IsEqual(MaxForwardsHeader,        HeaderName) then Result := MaxForwardsHeader
  else if IsEqual(SubjectHeader,            HeaderName) then Result := SubjectHeader
  else if IsEqual(ToHeaderFull,             HeaderName) then Result := ToHeaderFull
  else if IsEqual(ToHeaderShort,            HeaderName) then Result := ToHeaderFull
  else if IsEqual(ViaHeaderFull,            HeaderName) then Result := ViaHeaderFull
  else if IsEqual(ViaHeaderShort,           HeaderName) then Result := ViaHeaderFull
  else
    Result := HeaderName;
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
  // this is wrong. We need the original request's details - via headers, etc.
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
var
  Name: String;
  S:    String;
begin
  S := Header;
  Name := Trim(Fetch(S, ':'));
  Name := Self.CanonicaliseName(Name);

  Msg.Headers.Add(Name).Value := Trim(S);
end;

procedure TIdSipParser.IncCurrentLine;
begin
  Inc(fCurrentLine);
end;

procedure TIdSipParser.InitialiseMessage(Msg: TIdSipMessage);
begin
  Msg.Headers.Clear;
  Msg.SipVersion := '';
end;

procedure TIdSipParser.ParseContactHeader(const Msg: TIdSipMessage; ContactParms: String);
begin
  while (ContactParms <> '') do
    Msg.Headers.Add(ContactHeaderFull).Value := Trim(Fetch(ContactParms, ','));
end;

procedure TIdSipParser.ParseHeader(const Msg: TIdSipMessage; const Header: String);
var
  N: Integer;
begin
  try
    if TIdSipHeaders.IsCallID(Header) then
      Msg.CallID := Self.GetHeaderValue(Header)
    else if TIdSipHeaders.IsContact(Header) then
      Self.ParseContactHeader(Msg, Self.GetHeaderValue(Header))
    else if TIdSipHeaders.IsContentLength(Header) then
      Msg.ContentLength := Self.GetHeaderNumberValue(Msg, Header)
    else if TIdSipHeaders.IsMaxForwards(Header) then begin
      N := Self.GetHeaderNumberValue(Msg, Header);
      if (N < 0) or (N > 255) then
        raise Msg.MalformedException.Create(Format(MalformedToken, [Self.GetHeaderName(Header), Header]));
      Msg.MaxForwards := N;
    end
  //  else if TIdSipHeaders.IsTo(Header) then begin
  //    Msg.Headers.Add(ToHeaderFull).Value := Self.GetHeaderValue(Header);
  //  end
    else if TIdSipHeaders.IsVia(Header) then
      Self.ParseViaHeader(Msg, Self.GetHeaderValue(Header))
    else
      Self.AddHeader(Msg, Header);
  except
    on E: EBadHeader do
      raise Msg.MalformedException.Create(Format(MalformedToken, [E.Message, Header]));
  end;
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
begin
  while (ViaParms <> '') do
    Msg.Headers.Add(ViaHeaderFull).Value := Fetch(ViaParms, ',');
end;

procedure TIdSipParser.ResetCurrentLine;
begin
  fCurrentLine := 1;
end;

initialization
finalization
  GIdSipHeadersMap.Free;
end.
