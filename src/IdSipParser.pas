unit IdSipParser;

interface

uses
  Classes, IdGlobal, SysUtils;

type
  TIdSipMessage = class(TObject)
  private
    fBody:          String;
    fContentLength: Cardinal;
    fOtherHeaders:  TStrings;
    fSIPVersion:    String;
  public
    constructor Create;
    destructor  Destroy; override;

    function  AsString: String; virtual; abstract;
    procedure ReadBody(const S: TStream);

    property Body:          String   read fBody write fBody;
    property ContentLength: Cardinal read fContentLength write fContentLength;
    property OtherHeaders:  TStrings read fOtherHeaders;
    property SIPVersion:    String   read fSIPVersion write fSIPVersion;
  end;

  TIdSipRequest = class(TIdSipMessage)
  private
    fMethod:     String;
    fRequest:    String;
  public
    function AsString: String; override;

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

    property StatusCode: Integer read fStatusCode write SetStatusCode;
    property StatusText: String  read fStatusText write fStatusText;
  end;

  TIdSipParser = class(TObject)
  private
    fCurrentLine: Cardinal;
    fSource:      TStream;

    procedure IncCurrentLine;
    procedure ParseHeaders(const Msg: TIdSipMessage);
    procedure ParseRequestLine(const Request: TIdSipRequest);
    procedure ParseStatusLine(const Response: TIdSipResponse);
    procedure ResetCurrentLine;
  public
    constructor Create;
    function  CurrentLine: Cardinal;
    function  Eof: Boolean;
    function  IsContentLength(Header: String): Boolean;
    function  IsEqual(const S1, S2: String): Boolean;
    function  IsMethod(Method: String): Boolean;
    function  IsNumber(Number: String): Boolean;
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

const
  BadStatusCode             = -1;
  DamagedLineEnd            = 'Damaged line end at line %d, expected %s but was %s';
  EmptyInputStream          = 'Empty input stream';
  InvalidSipVersion         = 'Invalid Sip-Version: ''%s''';
  InvalidStatusCode         = 'Invalid Status-Code: ''%s''';
  MalformedContentLength    = 'Malformed Content-Length: ''%s''';
  MalformedMethod           = 'Malformed Method: ''%s''';
  MalformedRequestLine      = 'Malformed Request-Line: ''%s''';
  MissingSipVersion         = 'Missing SIP-Version';
  RequestLine               = '%s %s %s' + EOL;
  RequestUriNoAngleBrackets = 'Request-URI may not be enclosed in <>';
  RequestUriNoSpaces        = 'Request-URI may not contain spaces';
  StatusLine                = '%s %d %s' + EOL;
  UnexpectedMessageLength   = 'Expected message-body length of %d but was %d';

const
  ContentLengthFull  = 'Content-Length';
  ContentLengthShort = 'l';
  MethodAck          = 'ACK';
  MethodBye          = 'BYE';
  MethodCancel       = 'CANCEL';
  MethodInvite       = 'INVITE';
  MethodOptions      = 'OPTIONS';
  MethodRegister     = 'REGISTER';
  SipName            = 'SIP';
  SIPVersion         = 'SIP/2.0';

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

implementation

uses
  StrUtils;

//******************************************************************************
//* TIdSipMessage                                                              *
//******************************************************************************
//* TIdSipMessage Public methods ***********************************************

constructor TIdSipMessage.Create;
begin
  inherited Create;

  fOtherHeaders := TStringList.Create;
end;

destructor TIdSipMessage.Destroy;
begin
  fOtherHeaders.Free;

  inherited Destroy;
end;

procedure TIdSipMessage.ReadBody(const S: TStream);
begin
  SetLength(fBody, Self.ContentLength);
  S.Read(fBody[1], Self.ContentLength);
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

  Result := Result + ContentLengthFull + ':' + CHAR32 + IntToStr(Self.ContentLength) + EOL;
  Result := Result + EOL;
  Result := Result + Self.Body;
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

  Result := Result + ContentLengthFull + ':' + CHAR32 + IntToStr(Self.ContentLength) + EOL;
  Result := Result + EOL;
  Result := Result + Self.Body;
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

function TIdSipParser.IsContentLength(Header: String): Boolean;
var
  Name: String;
begin
  Name := Fetch(Header, ':');
  Name := Trim(Name);
  Result := Self.IsEqual(Name, ContentLengthFull)
         or Self.IsEqual(Name, ContentLengthShort);
end;

function TIdSipParser.IsEqual(const S1, S2: String): Boolean;
begin
  Result := Lowercase(S1) = Lowercase(S2);
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

function TIdSipParser.IsSipVersion(Version: String): Boolean;
var
  Token: String;
begin
  Token := Fetch(Version, '/');
  Result := Token = SipName;

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
  if not Self.Eof then begin
    Self.ResetCurrentLine;
    Self.ParseRequestLine(Request);
    Self.ParseHeaders(Request);
  end;
end;

procedure TIdSipParser.ParseResponse(const Response: TIdSipResponse);
begin
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

procedure TIdSipParser.IncCurrentLine;
begin
  Inc(fCurrentLine);
end;

procedure TIdSipParser.ParseHeaders(const Msg: TIdSipMessage);
  procedure AddHeader(const Msg: TIdSipMessage; Header: String);
  var
    Err: Integer;
    N:   Cardinal;
    S:   String;
  begin
    if Self.IsContentLength(Header) then begin
      S := Header;
      Fetch(S, ':');
      S := Trim(S);

      Val(S, N, Err);
      if (Err = 0) then
        Msg.ContentLength := N
      else
        raise EBadRequest.Create(Format(MalformedContentLength, [Header]));
    end
    else
      Msg.OtherHeaders.Add(Header);
  end;
var
  FoldedHeader: String;
  Line:         String;
begin
  FoldedHeader := Self.ReadLn;
  if (FoldedHeader <> '') then begin
    Line := Self.ReadLn;
    while (Line <> '') do begin
      if (Line[1] in [' ', #9]) then begin
        FoldedHeader := FoldedHeader + ' ' + RightStr(Line, Length(Line) - 1);
        Line := Self.ReadLn;
      end
      else begin
        AddHeader(Msg, FoldedHeader);
        FoldedHeader := Line;
        Line := Self.ReadLn;
      end;
    end;
    if (FoldedHeader <> '') then
      AddHeader(Msg, FoldedHeader);
  end;
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
      raise EBadRequest.Create(RequestUriNoSpaces)
    else if (Tokens.Count < 3) then
      raise EBadRequest.Create(Format(MalformedRequestLine, [Line]));

    Request.Method := Tokens[0];
    // we want to check the Method
    if not Self.IsMethod(Request.Method) then
      raise EBadRequest.Create(Format(MalformedMethod, [Request.Method]));

    Request.Request := Tokens[1];

    if (Request.Request[1] = '<') and (Request.Request[Length(Request.Request)] = '>') then
      raise EBadRequest.Create(RequestUriNoAngleBrackets);

    Request.SIPVersion := Tokens[2];

    if not Self.IsSipVersion(Request.SIPVersion) then
      raise EBadRequest.Create(Format(InvalidSipVersion, [Request.SIPVersion]));
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
    raise EBadRequest.Create(Format(InvalidSipVersion, [Response.SIPVersion]));

  StatusCode := Fetch(Line);
  if not Self.IsNumber(StatusCode) then
    raise EBadRequest.Create(Format(InvalidStatusCode, [StatusCode]));

  Response.StatusCode := StrToIntDef(StatusCode, BadStatusCode);

  Response.StatusText := Line;
end;

procedure TIdSipParser.ResetCurrentLine;
begin
  fCurrentLine := 1;
end;

end.




















































