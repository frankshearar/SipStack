unit IdSipParser;

{*
 * Some implementation principles we follow:
 *  * The original headers may be folded, may contain all manner of guff. We
 *    don't make any attempt to store the raw header - we parse it, and when
 *    we write out the headers we write them in the simplest possible way. As
 *    a result we CANNOT duplicate the exact form of the original message, even
 *    though the new message will have identical, semantically speaking.
 *  * We do (because we have to) keep the order of headers. Any newly created
 *    headers are simply appended.
 *  * Any and all parsing errors are raised as exceptions that descend from
 *    EParser as soon as we can.
 *  * New headers can be created that weren't present in the original message.
 *    These messages will, by default, have the empty string as value. For example,
 *    querying the value of Content-Type will create a TIdSipHeader with Value ''.
 *  * Each header is regarded as using a particular language, and are parsers for
 *    that language (in the SetValue method).
 *  * Comma-separated headers are always separated into separate headers.
 *}

interface

uses
  IdGlobal, IdSimpleParser, IdSipMessage;

const
  SIPVersion = 'SIP/2.0';

type
  TIdSipParser = class(TIdSimpleParser)
  private
    procedure AddHeader(const Msg: TIdSipMessage; Header: String);
    procedure CheckContentLengthContentType(const Msg: TIdSipMessage);
    procedure CheckCSeqMethod(const Request: TIdSipRequest);
    procedure CheckRequiredHeaders(const Request: TIdSipRequest);
    procedure InitialiseMessage(Msg: TIdSipMessage);
    procedure ParseCompoundHeader(const Msg: TIdSipMessage; const Header: String; Parms: String);
    procedure ParseHeader(const Msg: TIdSipMessage; const Header: String);
    procedure ParseHeaders(const Msg: TIdSipMessage);
    procedure ParseRequestLine(const Request: TIdSipRequest);
    procedure ParseStatusLine(const Response: TIdSipResponse);
  public
    class function IsIPv6Reference(const Token: String): Boolean;
    class function IsMethod(Method: String): Boolean;
    class function IsQuotedString(const Token: String): Boolean;
    class function IsQValue(const Token: String): Boolean;
    class function IsSipVersion(Version: String): Boolean;
    class function IsToken(const Token: String): Boolean;
    class function IsTransport(const Token: String): Boolean;
    class function IsWord(const Token: String): Boolean;

    function  GetHeaderName(Header: String): String;
    function  GetHeaderNumberValue(const Msg: TIdSipMessage; const Header: String): Cardinal;
    function  GetHeaderValue(Header: String): String;
    function  MakeBadRequestResponse(const Reason: String): TIdSipResponse;
    function  ParseAndMakeMessage: TIdSipMessage;
    procedure ParseRequest(const Request: TIdSipRequest);
    procedure ParseResponse(const Response: TIdSipResponse);
  end;

  EBadHeader = class(EParser);
  EBadRequest = class(EParser);
  EBadResponse = class(EParser);

const
  LegalTokenChars = Alphabet + Digits
                  + ['-', '.', '!', '%', '*', '_',
                     '+', '`', '''', '~'];
  LegalWordChars = LegalTokenChars
                 + ['(', ')', '<', '>', ':', '\', '"', '/', '[',
                    ']', '?', '{', '}'];
  LWSChars = [' ', #9, #10, #13];

const
  BadStatusCode             = -1;
  CSeqMethodMismatch        = 'CSeq header method doesn''t match request method';
  InvalidSipVersion         = 'Invalid Sip-Version: ''%s''';
  InvalidStatusCode         = 'Invalid Status-Code: ''%s''';
  MissingCallID             = 'Missing Call-ID header';
  MissingContentType        = 'Missing Content-Type header with a non-empty message-body';
  MissingCSeq               = 'Missing CSeq header';
  MissingFrom               = 'Missing From header';
  MissingMaxForwards        = 'Missing Max-Forwards header';
  MissingTo                 = 'Missing To header';
  MissingSipVersion         = 'Missing SIP-Version';
  MissingVia                = 'Missing Via header';
  RequestLine               = '%s %s %s' + EOL;
  RequestUriNoAngleBrackets = 'Request-URI may not be enclosed in <>';
  RequestUriNoSpaces        = 'Request-URI may not contain spaces';
  StatusLine                = '%s %d %s' + EOL;
  UnexpectedMessageLength   = 'Expected message-body length of %d but was %d';
  UnmatchedQuotes           = 'Unmatched quotes';

// for IdResourceStrings ?
const
  AcceptHeader               = 'Accept';
  AcceptEncodingHeader       = 'Accept-Encoding';
  AcceptLanguageHeader       = 'Accept-Language';
  AlertInfoHeader            = 'Alert-Info';
  AllowHeader                = 'Allow';
  AuthenticationInfoHeader   = 'Authentication-Info';
  AuthorizationHeader        = 'Authorization';
  BranchMagicCookie          = 'z9hG4bK';
  BranchParam                = 'branch';
  CallIDHeaderFull           = 'Call-ID';
  CallIDHeaderShort          = 'i';
  CallInfoHeader             = 'Call-Info';
  ContactHeaderFull          = 'Contact';
  ContactHeaderShort         = 'm';
  ContentDispositionHeader   = 'Content-Disposition';
  ContentEncodingHeaderFull  = 'Content-Encoding';
  ContentEncodingHeaderShort = 'e';
  ContentLanguageHeader      = 'Content-Language';
  ContentLengthHeaderFull    = 'Content-Length';
  ContentLengthHeaderShort   = 'l';
  ContentTypeHeaderFull      = 'Content-Type';
  ContentTypeHeaderShort     = 'c';
  CSeqHeader                 = 'CSeq';
  DefaultMaxForwards         = 70;
  DateHeader                 = 'Date';
  ErrorInfoHeader            = 'Error-Info';
  ExpiresHeader              = 'Expires';
  ExpiresParam               = ExpiresHeader;
  FromHeaderFull             = 'From';
  FromHeaderShort            = 'f';
  InReplyToHeader            = 'In-Reply-To';
  MaddrParam                 = 'maddr';
  MaxForwardsHeader          = 'Max-Forwards';
  MethodAck                  = 'ACK';
  MethodBye                  = 'BYE';
  MethodCancel               = 'CANCEL';
  MethodInvite               = 'INVITE';
  MethodOptions              = 'OPTIONS';
  MethodRegister             = 'REGISTER';
  MIMEVersionHeader          = 'MIME-Version';
  MinExpiresHeader           = 'Min-Expires';
  OrganizationHeader         = 'Organization';
  PriorityHeader             = 'Priority';
  ProxyAuthenticateHeader    = 'Proxy-Authenticate';
  ProxyAuthorizationHeader   = 'Proxy-Authorization';
  ProxyRequireHeader         = 'Proxy-Require';
  QParam                     = 'q';
  ReceivedParam              = 'received';
  RecordRouteHeader          = 'Record-Route';
  ReplyToHeader              = 'Reply-To';
  RequireHeader              = 'Require';
  RetryAfterHeader           = 'Retry-After';
  RouteHeader                = 'Route';
  ServerHeader               = 'Server';
  SipName                    = 'SIP';
  SubjectHeaderFull          = 'Subject';
  SubjectHeaderShort         = 's';
  SupportedHeaderFull        = 'Supported';
  SupportedHeaderShort       = 'k';
  TagParam                   = 'tag';
  TimestampHeader            = 'Timestamp';
  ToHeaderFull               = 'To';
  ToHeaderShort              = 't';
  TTLParam                   = 'ttl';
  UnsupportedHeader          = 'Unsupported';
  UserAgentHeader            = 'User-Agent';
  ViaHeaderFull              = 'Via';
  ViaHeaderShort             = 'v';
  WarningHeader              = 'Warning';
  WWWAuthenticateHeader      = 'WWW-Authenticate';

// for IdAssignedNumbers
const
  IdPORT_SIP     = 5060;
  IdPORT_SIP_TLS = 5061;

// for IdResourceStrings
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

function DecodeQuotedStr(const S: String; var Dest: String): Boolean;
function IsEqual(const S1, S2: String): Boolean;
function ShortMonthToInt(const Month: String): Integer;

implementation

uses
  Classes, DateUtils, StrUtils, SysUtils;

//******************************************************************************
//* Unit public procedures & functions                                         *
//******************************************************************************

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
      Result := Result and false;

    // We use "<" and not "<=" because if a \ is the last character we have
    // a malformed string. Too, this allows use to Dest[I + 1]
    I := 1;
    while (I < Length(Dest)) and Result do begin
      Result := Dest[I] <> '"';
      FoundSlash := Dest[I] = '\';
      if (FoundSlash) then begin
        Delete(Dest, I, 1);

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

function IsEqual(const S1, S2: String): Boolean;
begin
  Result := Lowercase(S1) = Lowercase(S2);
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

//******************************************************************************
//* TIdSipParser                                                               *
//******************************************************************************
//* TIdSipParser Public methods ************************************************

class function TIdSipParser.IsIPv6Reference(const Token: String): Boolean;
begin
  Result := (Copy(Token, 1, 1) = '[')
        and (Copy(Token, Length(Token), 1) = ']')
        and Self.IsIPv6Address(Copy(Token, 2, Length(Token) - 2));
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
    Result := DecodeQuotedStr(Copy(Token, 2, Length(Token) - 2), S)
              and (Token[1] = '"')
              and (Token[Length(Token)] = '"');
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
  try
    StrToTransport(Token);
    Result := true;
  except
    Result := false;
  end;
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
  if (IndyPos(':', Header) = 0) then
    Result := ''
  else begin
    Result := Header;
    Fetch(Result, ':');
    Result := Trim(Result);
  end;
end;

function TIdSipParser.MakeBadRequestResponse(const Reason: String): TIdSipResponse;
begin
  // This is wrong. We need the original request's details - via headers, etc.
  // Sometimes we cannot get this information, though, especially if the
  // original message we malformed and unparseable.

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
  Self.CheckRequiredHeaders(Request);
  Self.CheckContentLengthContentType(Request);
  Self.CheckCSeqMethod(Request);
end;

procedure TIdSipParser.ParseResponse(const Response: TIdSipResponse);
begin
  Self.InitialiseMessage(Response);
  if not Self.Eof then begin
    Self.ResetCurrentLine;
    Self.ParseStatusLine(Response);
    Self.ParseHeaders(Response);
  end;
  Self.CheckContentLengthContentType(Response);
end;

//* TIdSipParser Private methods ***********************************************

procedure TIdSipParser.AddHeader(const Msg: TIdSipMessage; Header: String);
var
  Name: String;
  S:    String;
begin
  S := Header;
  Name := Trim(Fetch(S, ':'));
  Name := TIdSipHeaders.CanonicaliseName(Name);

  Msg.Headers.Add(Name).Value := Trim(S);
end;

procedure TIdSipParser.CheckContentLengthContentType(const Msg: TIdSipMessage);
begin
  if (Msg.ContentLength > 0) and (Msg.ContentType = '') then
    raise Msg.MalformedException.Create(MissingContentType);
end;

procedure TIdSipParser.CheckCSeqMethod(const Request: TIdSipRequest);
begin
  if (Request.CSeq.Method <> Request.Method) then
    raise Request.MalformedException.Create(CSeqMethodMismatch);
end;

procedure TIdSipParser.CheckRequiredHeaders(const Request: TIdSipRequest);
begin
  if not Request.HasHeader(CallIDHeaderFull) then
    raise Request.MalformedException.Create(MissingCallID);

  if not Request.HasHeader(CSeqHeader) then
    raise Request.MalformedException.Create(MissingCSeq);

  if not Request.HasHeader(FromHeaderFull) then
    raise Request.MalformedException.Create(MissingFrom);

  if not Request.HasHeader(MaxForwardsHeader) then
    raise Request.MalformedException.Create(MissingMaxForwards);

  if not Request.HasHeader(ToHeaderFull) then
    raise Request.MalformedException.Create(MissingTo);

  if not Request.HasHeader(ViaHeaderFull) then
    raise Request.MalformedException.Create(MissingVia);
end;

procedure TIdSipParser.InitialiseMessage(Msg: TIdSipMessage);
begin
  Msg.Headers.Clear;
  Msg.SipVersion := '';
end;


procedure TIdSipParser.ParseCompoundHeader(const Msg: TIdSipMessage; const Header: String; Parms: String);
begin
  while (Parms <> '') do
    Msg.Headers.Add(Header).Value := Fetch(Parms, ',');
end;

procedure TIdSipParser.ParseHeader(const Msg: TIdSipMessage; const Header: String);
begin
  try
    if TIdSipHeaders.IsCompoundHeader(Header) then
      Self.ParseCompoundHeader(Msg, Self.GetHeaderName(Header), Self.GetHeaderValue(Header))
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
  Line := Self.ReadFirstNonBlankLine;

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
  Line := Self.ReadFirstNonBlankLine;

  Response.SIPVersion := Fetch(Line);
  if not Self.IsSipVersion(Response.SIPVersion) then
    raise Response.MalformedException.Create(Format(InvalidSipVersion, [Response.SIPVersion]));

  StatusCode := Fetch(Line);
  if not Self.IsNumber(StatusCode) then
    raise Response.MalformedException.Create(Format(InvalidStatusCode, [StatusCode]));

  Response.StatusCode := StrToIntDef(StatusCode, BadStatusCode);

  Response.StatusText := Line;
end;

end.
