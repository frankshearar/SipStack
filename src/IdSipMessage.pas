unit IdSipMessage;

interface

uses
  Classes, Contnrs, IdDateTimeStamp, IdGlobal, IdSimpleParser, IdSipDialogID,
  IdSipHeaders, IdSipInterfacedObject, IdURI, SyncObjs, SysUtils;

type
  TIdSipRequest = class;
  TIdSipResponse = class;

  TIdSipNotifyEvent = TNotifyEvent;
  TIdSipResponseEvent = procedure(Sender: TObject; const R: TIdSipResponse) of object;
  TIdSipRequestEvent = procedure(Sender: TObject; const R: TIdSipRequest) of object;

  IIdSipMessageVisitor = interface
    ['{E2900B55-A1CA-47F1-9DB0-D72D6A846EA0}']
    procedure VisitRequest(const Request: TIdSipRequest);
    procedure VisitResponse(const Response: TIdSipResponse);
  end;

  IIdSipMessageListener = interface
    ['{941E4681-89F9-4491-825C-F6458F7E663C}']
    procedure OnReceiveRequest(const Request: TIdSipRequest);
    procedure OnReceiveResponse(const Response: TIdSipResponse);
  end;

  TIdSipMessage = class(TPersistent)
  private
    fBody:       String;
    fPath:       TIdSipViaPath;
    fHeaders:    TIdSipHeaders;
    fSIPVersion: String;

    function  GetCallID: String;
    function  GetContentLength: Cardinal;
    function  GetContentType: String;
    function  GetCSeq: TIdSipCSeqHeader;
    function  GetFrom: TIdSipFromHeader;
    function  GetMaxForwards: Byte;
    function  GetTo: TIdSipToHeader;
    procedure SetCallID(const Value: String);
    procedure SetContentLength(const Value: Cardinal);
    procedure SetContentType(const Value: String);
    procedure SetCSeq(const Value: TIdSipCSeqHeader);
    procedure SetFrom(const Value: TIdSipFromHeader);
    procedure SetMaxForwards(const Value: Byte);
    procedure SetPath(const Value: TIdSipViaPath);
    procedure SetTo(const Value: TIdSipToHeader);
  protected
    function FirstLine: String; virtual; abstract;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure Accept(const Visitor: IIdSipMessageVisitor); virtual;
    function  AddHeader(const HeaderName: String): TIdSipHeader; overload;
    procedure AddHeader(const Header: TIdSipHeader); overload;
    procedure AddHeaders(const Headers: TIdSipHeaderList);
    procedure Assign(Src: TPersistent); override;
    function  AsString: String; virtual;
    procedure ClearHeaders;
    function  FirstContact: TIdSipContactHeader;
    function  FirstHeader(const HeaderName: String): TIdSipHeader;
    function  HeaderAt(const Index: Cardinal): TIdSipHeader;
    function  HeaderCount: Integer;
    function  HasHeader(const HeaderName: String): Boolean;
    function  IsEqualTo(const Msg: TIdSipMessage): Boolean; virtual; abstract;
    function  IsRequest: Boolean; virtual; abstract;
    function  LastHop: TIdSipViaHeader;
    function  MalformedException: ExceptClass; virtual; abstract;
    procedure ReadBody(const S: TStream);
    procedure RemoveHeader(const Header: TIdSipHeader);
    procedure RemoveAllHeadersNamed(const Name: String);

    property Body:          String           read fBody write fBody;
    property CallID:        String           read GetCallID write SetCallID;
    property ContentLength: Cardinal         read GetContentLength write SetContentLength;
    property ContentType:   String           read GetContentType write SetContentType;
    property CSeq:          TIdSipCSeqHeader read GetCSeq write SetCSeq;
    property From:          TIdSipFromHeader read GetFrom write SetFrom;
    property Headers:       TIdSipHeaders    read fHeaders;
    property MaxForwards:   Byte             read GetMaxForwards write SetMaxForwards;
    property Path:          TIdSipViaPath    read fPath write SetPath;
    property SIPVersion:    String           read fSIPVersion write fSIPVersion;
    property ToHeader:      TIdSipToHeader   read GetTo write SetTo;
  end;

  TIdSipMessageClass = class of TIdSipMessage;

  TIdSipRequest = class(TIdSipMessage)
  private
    fMethod:     String;
    fRequestUri: TIdURI;

    procedure SetRequestUri(const Value: TIdURI);
  protected
    function FirstLine: String; override;
  public
    constructor Create; override;

    procedure Accept(const Visitor: IIdSipMessageVisitor); override;
    procedure Assign(Src: TPersistent); override;
    function  AsString: String; override;
    function  HasSipsUri: Boolean;
    function  IsAck: Boolean;
    function  IsBye: Boolean;
    function  IsCancel: Boolean;
    function  IsEqualTo(const Msg: TIdSipMessage): Boolean; override;
    function  IsInvite: Boolean;
    function  IsRequest: Boolean; override;
    function  MalformedException: ExceptClass; override;
    function  Match(const Msg: TIdSipMessage): Boolean;

    property Method:     String read fMethod write fMethod;
    property RequestUri: TIdURI read fRequestUri write SetRequestUri;
  end;

  TIdSipResponse = class(TIdSipMessage)
  private
    fStatusCode: Integer;
    fStatusText: String;

    procedure SetStatusCode(const Value: Integer);
  protected
    function FirstLine: String; override;
  public
    procedure Accept(const Visitor: IIdSipMessageVisitor); override;
    procedure Assign(Src: TPersistent); override;
    function  IsEqualTo(const Msg: TIdSipMessage): Boolean; override;
    function  IsFinal: Boolean;
    function  IsProvisional: Boolean;
    function  IsRequest: Boolean; override;
    function  MalformedException: ExceptClass; override;

    property StatusCode: Integer read fStatusCode write SetStatusCode;
    property StatusText: String  read fStatusText write fStatusText;
  end;

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
  TIdSipParser = class(TIdSimpleParser, IIdSipMessageVisitor)
  private
    procedure AddHeader(const Msg: TIdSipMessage; Header: String);
    procedure CheckContentLengthContentType(const Msg: TIdSipMessage);
    procedure CheckCSeqMethod(const Request: TIdSipRequest);
    procedure CheckRequiredRequestHeaders(const Msg: TIdSipMessage);
    procedure CheckRequiredResponseHeaders(const Msg: TIdSipMessage);
    function  CreateResponseOrRequest(const Token: String): TIdSipMessage;
    procedure InitializeMessage(Msg: TIdSipMessage);
    procedure ParseCompoundHeader(const Msg: TIdSipMessage; const Header: String; Parms: String);
    procedure ParseHeader(const Msg: TIdSipMessage; const Header: String);
    procedure ParseHeaders(const Msg: TIdSipMessage);
    procedure ParseRequestLine(const Request: TIdSipRequest);
    procedure ParseStatusLine(const Response: TIdSipResponse);

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    class function IsIPv6Reference(const Token: String): Boolean;
    class function IsMethod(Method: String): Boolean;
    class function IsQuotedString(const Token: String): Boolean;
    class function IsQValue(const Token: String): Boolean;
    class function IsScheme(const Scheme: String): Boolean;
    class function IsSipVersion(Version: String): Boolean;
    class function IsToken(const Token: String): Boolean;
    class function IsTransport(const Token: String): Boolean;
    class function IsWord(const Token: String): Boolean;

    function  GetHeaderName(Header: String): String;
    function  GetHeaderNumberValue(const Msg: TIdSipMessage; const Header: String): Cardinal;
    function  GetHeaderValue(Header: String): String;
    function  ParseAndMakeMessage: TIdSipMessage; overload;
    function  ParseAndMakeMessage(const Src: String): TIdSipMessage; overload;
    function  ParseAndMakeMessage(const Src: String; const MessageType: TIdSipMessageClass): TIdSipMessage; overload;
    function  ParseAndMakeRequest: TIdSipRequest; overload;
    function  ParseAndMakeRequest(const Src: String): TIdSipRequest; overload;
    function  ParseAndMakeResponse: TIdSipResponse; overload;
    function  ParseAndMakeResponse(const Src: String): TIdSipResponse; overload;
    procedure ParseMessage(const Msg: TIdSipMessage);
    procedure ParseRequest(const Request: TIdSipRequest);
    procedure ParseResponse(const Response: TIdSipResponse);

    procedure VisitRequest(const Request: TIdSipRequest);
    procedure VisitResponse(const Response: TIdSipResponse);
  end;

  // I am the Subject half of the Observer pattern as related to
  // IIdSipMessageListener (the Observer, naturally enough).
  TIdSipMessageSubject = class(TIdSipInterfacedObject)
  private
    MsgListenerLock: TCriticalSection;
    MsgListeners:    TList;
  protected
    procedure NotifyMessageListeners(const Request: TIdSipRequest); overload;
    procedure NotifyMessageListeners(const Response: TIdSipResponse); overload;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddMessageListener(const Listener: IIdSipMessageListener);
    procedure RemoveMessageListener(const Listener: IIdSipMessageListener);
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

function DecodeQuotedStr(const S: String; var Dest: String): Boolean;
function IsEqual(const S1, S2: String): Boolean;
function ShortMonthToInt(const Month: String): Integer;

implementation

uses
  IdSipConsts;

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
//* TIdSipMessage                                                              *
//******************************************************************************
//* TIdSipMessage Public methods ***********************************************

constructor TIdSipMessage.Create;
begin
  inherited Create;

  fHeaders := TIdSipHeaders.Create;
  fPath := TIdSipViaPath.Create(Self.Headers);

  Self.SIPVersion  := IdSipConsts.SIPVersion;
end;

destructor TIdSipMessage.Destroy;
begin
  fPath.Free;
  fHeaders.Free;

  inherited Destroy;
end;

procedure TIdSipMessage.Accept(const Visitor: IIdSipMessageVisitor);
begin
end;

function TIdSipMessage.AddHeader(const HeaderName: String): TIdSipHeader;
begin
  Result := Self.Headers.Add(HeaderName)
end;

procedure TIdSipMessage.AddHeader(const Header: TIdSipHeader);
begin
  Self.Headers.Add(Header);
end;

procedure TIdSipMessage.AddHeaders(const Headers: TIdSipHeaderList);
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

function TIdSipMessage.FirstContact: TIdSipContactHeader;
begin
  Result := Self.FirstHeader(ContactHeaderFull) as TIdSipContactHeader;
end;

function TIdSipMessage.FirstHeader(const HeaderName: String): TIdSipHeader;
begin
  Result := Self.Headers[HeaderName];
end;

function TIdSipMessage.HeaderAt(const Index: Cardinal): TIdSipHeader;
begin
  Result := Self.Headers.Items[Index];
end;

function TIdSipMessage.HeaderCount: Integer;
begin
  Result := Self.Headers.Count;
end;

function TIdSipMessage.HasHeader(const HeaderName: String): Boolean;
begin
  Result := Self.Headers.HasHeader(HeaderName);
end;

function TIdSipMessage.LastHop: TIdSipViaHeader;
begin
  Result := Self.Path.LastHop;
end;

procedure TIdSipMessage.ReadBody(const S: TStream);
const
  BufLen = 100;
var
  Buf:         array[1..BufLen] of Char;
  BytesToRead: Integer;
  Read:        Integer;
begin
  // It is the responsibility of the transport to ensure that
  // Content-Length is set before this method is called!

  if (Self.ContentLength > 0) then begin
    BytesToRead := Self.ContentLength;

    repeat
      Read := S.Read(Buf, Min(BufLen, BytesToRead));
      Dec(BytesToRead, Read);

      Self.Body := Self.Body + Copy(Buf, 1, Read);
    until (Read < BufLen) or (BytesToRead <= 0);
  end;
end;

procedure TIdSipMessage.RemoveHeader(const Header: TIdSipHeader);
begin
  Self.Headers.Remove(Header);
end;

procedure TIdSipMessage.RemoveAllHeadersNamed(const Name: String);
begin
  Self.Headers.RemoveAll(Name);
end;

//* TIdSipMessage Private methods **********************************************

function TIdSipMessage.GetCallID: String;
begin
  Result := Self.FirstHeader(CallIDHeaderFull).Value;
end;

function TIdSipMessage.GetContentLength: Cardinal;
begin
  Result := StrToInt(Self.FirstHeader(ContentLengthHeaderFull).Value);
end;

function TIdSipMessage.GetContentType: String;
begin
  Result := Self.FirstHeader(ContentTypeHeaderFull).Value;
end;

function TIdSipMessage.GetCSeq: TIdSipCSeqHeader;
begin
  Result := Self.FirstHeader(CSeqHeader) as TIdSipCSeqHeader;
end;

function TIdSipMessage.GetFrom: TIdSipFromHeader;
begin
  Result := Self.FirstHeader(FromHeaderFull) as TIdSipFromHeader;
end;

function TIdSipMessage.GetMaxForwards: Byte;
begin
  if (Self.FirstHeader(MaxForwardsHeader).Value = '') then
    Self.MaxForwards := DefaultMaxForwards;

  Result := StrToInt(Self.FirstHeader(MaxForwardsHeader).Value);
end;

function TIdSipMessage.GetTo: TIdSipToHeader;
begin
  Result := Self.FirstHeader(ToHeaderFull) as TIdSipToHeader;
end;

procedure TIdSipMessage.SetCallID(const Value: String);
begin
  Self.FirstHeader(CallIDHeaderFull).Value := Value;
end;

procedure TIdSipMessage.SetContentLength(const Value: Cardinal);
begin
  Self.FirstHeader(ContentLengthHeaderFull).Value := IntToStr(Value);
end;

procedure TIdSipMessage.SetContentType(const Value: String);
begin
  Self.FirstHeader(ContentTypeHeaderFull).Value := Value;
end;

procedure TIdSipMessage.SetCSeq(const Value: TIdSipCSeqHeader);
begin
  Self.CSeq.Assign(Value);
end;

procedure TIdSipMessage.SetFrom(const Value: TIdSipFromHeader);
begin
  Self.FirstHeader(FromHeaderFull).Assign(Value);
end;

procedure TIdSipMessage.SetMaxForwards(const Value: Byte);
begin
  Self.FirstHeader(MaxForwardsHeader).Value := IntToStr(Value);
end;

procedure TIdSipMessage.SetPath(const Value: TIdSipViaPath);
var
  I: Integer;
begin
  Self.Path.Clear;

  for I := 0 to Value.Count - 1 do
    Self.Path.Add(Value.Items[I]);
end;

procedure TIdSipMessage.SetTo(const Value: TIdSipToHeader);
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

  fRequestUri := TIdURI.Create('');
  Self.ContentLength := 0;
end;

procedure TIdSipRequest.Accept(const Visitor: IIdSipMessageVisitor);
begin
  Visitor.VisitRequest(Self);
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

function TIdSipRequest.AsString: String;
begin
  if not Self.HasHeader(MaxForwardsHeader) then
    Self.MaxForwards := DefaultMaxForwards;

  Result := inherited AsString;
end;

function TIdSipRequest.HasSipsUri: Boolean;
var
  S: String;
begin
  S := Self.RequestUri.GetFullURI;
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

function TIdSipRequest.IsEqualTo(const Msg: TIdSipMessage): Boolean;
var
  Request: TIdSipRequest;
begin
  if (Msg is Self.ClassType) then begin
    Request := Msg as TIdSipRequest;

    Result := (Self.SIPVersion            = Request.SIPVersion)
          and (Self.Method                = Request.Method)
          and (Self.RequestUri.GetFullURI = Request.RequestUri.GetFullURI)
          and (Self.Headers.IsEqualTo(Request.Headers));
  end
  else
    Result := false;
end;

function TIdSipRequest.IsInvite: Boolean;
begin
  Result := Self.Method = MethodInvite;
end;

function TIdSipRequest.IsRequest: Boolean;
begin
  Result := true;
end;

function TIdSipRequest.MalformedException: ExceptClass;
begin
  Result := EBadRequest;
end;

function TIdSipRequest.Match(const Msg: TIdSipMessage): Boolean;
var
  Request:  TIdSipRequest;
  Response: TIdSipResponse;
begin

  if (Msg is TIdSipRequest) then begin
    Request := Msg as TIdSipRequest;
    // Add RFC 2543 matching

    // cf. RFC 3261 section 17.2.3
    if Request.LastHop.IsRFC3261Branch then begin
      Result := (Request.LastHop.Branch = Self.LastHop.Branch)
            and (Request.LastHop.SentBy = Self.LastHop.SentBy);

      if Request.IsACK then
        Result := Result and Self.IsInvite
      else
        Result := Result and (Request.Method = Self.Method);
    end
    else begin
      raise Exception.Create('matching of SIP/1.0 messages not implemented yet');
    end;
  end
  else if (Msg is TIdSipResponse) then begin
    Response := Msg as TIdSipResponse;
    // Add RFC 2543 matching

    // cf. RFC 3261 section 17.1.3
    Result := (Response.Path.Length > 0)
          and (Self.Path.Length > 0);

    Result := Result
          and (Response.LastHop.Branch = Self.LastHop.Branch);

    if (Response.CSeq.Method = MethodAck) then
      Result := Result
            and (Self.IsInvite)
    else
      Result := Result
            and (Response.CSeq.Method = Self.Method);
  end
  else Result := false;
end;

//* TIdSipRequest Protected methods ********************************************

function TIdSipRequest.FirstLine: String;
begin
  Result := Format(RequestLine, [Self.Method, Self.RequestUri.GetFullURI, Self.SIPVersion]);
end;

//* TIdSipRequest Private methods **********************************************

procedure TIdSipRequest.SetRequestUri(const Value: TIdURI);
begin
  Self.fRequestUri.URI := Value.GetFullURI
end;

//*******************************************************************************
//* TIdSipResponse                                                              *
//*******************************************************************************
//* TIdSipResponse Public methods ***********************************************

procedure TIdSipResponse.Accept(const Visitor: IIdSipMessageVisitor);
begin
  Visitor.VisitResponse(Self);
end;

procedure TIdSipResponse.Assign(Src: TPersistent);
var
  R: TIdSipResponse;
begin
  inherited Assign(Src);

  R := Src as TIdSipResponse;

  Self.StatusCode := R.StatusCode;
  Self.StatusText := R.StatusText;
end;

function TIdSipResponse.IsEqualTo(const Msg: TIdSipMessage): Boolean;
var
  Response: TIdSipResponse;
begin
  if (Msg is Self.ClassType) then begin
    Response := Msg as TIdSipResponse;

    Result := (Self.SIPVersion = Response.SipVersion)
          and (Self.StatusCode = Response.StatusCode)
          and (Self.StatusText = Response.StatusText)
          and (Self.Headers.IsEqualTo(Response.Headers));
  end
  else
    Result := false;
end;

function TIdSipResponse.IsFinal: Boolean;
begin
  Result := Self.StatusCode div 100 > 1;
end;

function TIdSipResponse.IsProvisional: Boolean;
begin
  Result := Self.StatusCode div 100 = 1;
end;

function TIdSipResponse.IsRequest: Boolean;
begin
  Result := false;
end;

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

class function TIdSipParser.IsScheme(const Scheme: String): Boolean;
var
  I: Integer;
begin
  Result := Length(Scheme) > 0;

  if Result then begin
    Result := Result and Self.IsLetter(Scheme[1]);

    I := 2;
    while (I <= Length(Scheme)) and Result do begin
      Result := Result and (Self.IsAlphaNumeric(Scheme[I]) or (Scheme[I] in ['+', '-', '.']));
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

function TIdSipParser.ParseAndMakeMessage: TIdSipMessage;
var
  FirstLine: String;
  FirstToken: String;
begin
  if not Self.Eof then begin
    FirstLine := Self.PeekLine;
    FirstToken := Fetch(FirstLine);
    FirstToken := Fetch(FirstToken, '/');

    // It's safe to do this because we know a SIP response starts with "SIP/",
    // and the "/" is not allowed in a Method.
    Result := Self.CreateResponseOrRequest(FirstToken);
    try
      Self.ParseMessage(Result);
    except
      Result.Free;

      raise;
    end;
  end
  else
    raise EParser.Create(EmptyInputStream);
end;

function TIdSipParser.ParseAndMakeMessage(const Src: String): TIdSipMessage;
var
  OriginalSrc: TStream;
  S:           TStringStream;
begin
  OriginalSrc := Self.Source;
  try
    S := TStringStream.Create(Src);
    try
      Self.Source := S;

      Result := Self.ParseAndMakeMessage;
      try
        Result.Body := S.ReadString(Result.ContentLength);
      except
        Result.Free;

        raise;
      end;
    finally
      S.Free;
    end;
  finally
    Self.Source := OriginalSrc;
  end;
end;

function TIdSipParser.ParseAndMakeMessage(const Src: String; const MessageType: TIdSipMessageClass): TIdSipMessage;
var
  OriginalSrc: TStream;
  S:           TStringStream;
begin
  OriginalSrc := Self.Source;
  try
    S := TStringStream.Create(Src);
    try
      Self.Source := S;

      Result := MessageType.Create;
      try
        Self.ParseMessage(Result);
        Result.Body := S.ReadString(Result.ContentLength);
      except
        Result.Free;

        raise;
      end;
    finally
      S.Free;
    end;
  finally
    Self.Source := OriginalSrc;
  end;
end;

function TIdSipParser.ParseAndMakeRequest: TIdSipRequest;
begin
  Result := TIdSipRequest.Create;
  try
    Self.ParseRequest(Result);
  except
    Result.Free;

    raise;
  end;
end;

function TIdSipParser.ParseAndMakeRequest(const Src: String): TIdSipRequest;
begin
  Result := Self.ParseAndMakeMessage(Src, TIdSipRequest) as TIdSipRequest;
end;

function TIdSipParser.ParseAndMakeResponse: TIdSipResponse;
begin
  Result := TIdSipResponse.Create;
  try
    Self.ParseResponse(Result);
  except
    Result.Free;

    raise;
  end;
end;

function TIdSipParser.ParseAndMakeResponse(const Src: String): TIdSipResponse;
begin
  Result := Self.ParseAndMakeMessage(Src, TIdSipResponse) as TIdSipResponse;
end;

procedure TIdSipParser.ParseMessage(const Msg: TIdSipMessage);
begin
  Msg.Accept(Self);
end;

procedure TIdSipParser.ParseRequest(const Request: TIdSipRequest);
begin
  Self.InitializeMessage(Request);

  if not Self.Eof then begin
    Self.ResetCurrentLine;
    Self.ParseRequestLine(Request);
    Self.ParseHeaders(Request);
  end;

  Self.CheckRequiredRequestHeaders(Request);
  Self.CheckContentLengthContentType(Request);
  Self.CheckCSeqMethod(Request);
end;

procedure TIdSipParser.ParseResponse(const Response: TIdSipResponse);
begin
  Self.InitializeMessage(Response);

  if not Self.Eof then begin
    Self.ResetCurrentLine;
    Self.ParseStatusLine(Response);
    Self.ParseHeaders(Response);
  end;

  Self.CheckContentLengthContentType(Response);
//  Self.CheckRequiredResponseHeaders(Response);
end;

procedure TIdSipParser.VisitRequest(const Request: TIdSipRequest);
begin
  Self.ParseRequest(Request);
end;

procedure TIdSipParser.VisitResponse(const Response: TIdSipResponse);
begin
  Self.ParseResponse(Response);
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

  Msg.AddHeader(Name).Value := Trim(S);
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

procedure TIdSipParser.CheckRequiredRequestHeaders(const Msg: TIdSipMessage);
begin
  Self.CheckRequiredResponseHeaders(Msg);

  if not Msg.HasHeader(MaxForwardsHeader) then
    raise Msg.MalformedException.Create(MissingMaxForwards);
end;

procedure TIdSipParser.CheckRequiredResponseHeaders(const Msg: TIdSipMessage);
begin
  if not Msg.HasHeader(CallIDHeaderFull) then
    raise Msg.MalformedException.Create(MissingCallID);

  if not Msg.HasHeader(CSeqHeader) then
    raise Msg.MalformedException.Create(MissingCSeq);

  if not Msg.HasHeader(FromHeaderFull) then
    raise Msg.MalformedException.Create(MissingFrom);

  if not Msg.HasHeader(ToHeaderFull) then
    raise Msg.MalformedException.Create(MissingTo);

  if not Msg.HasHeader(ViaHeaderFull) then
    raise Msg.MalformedException.Create(MissingVia);
end;

function TIdSipParser.CreateResponseOrRequest(const Token: String): TIdSipMessage;
begin
  if (Token = SipName) then
    Result := TIdSipResponse.Create
  else
    Result := TIdSipRequest.Create;
end;

procedure TIdSipParser.InitializeMessage(Msg: TIdSipMessage);
begin
  Msg.ClearHeaders;
  Msg.SipVersion := '';
end;


procedure TIdSipParser.ParseCompoundHeader(const Msg: TIdSipMessage; const Header: String; Parms: String);
begin
  while (Parms <> '') do
    Msg.AddHeader(Header).Value := Fetch(Parms, ',');
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
end;

procedure TIdSipParser.ParseRequestLine(const Request: TIdSipRequest);
var
  Line:   String;
  Tokens: TStrings;
  URI:    String;
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

    URI := Tokens[1];

    if (URI[1] = '<') and (URI[Length(URI)] = '>') then
      raise Request.MalformedException.Create(RequestUriNoAngleBrackets);

    Request.RequestUri.URI := URI;

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

function TIdSipParser.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := 0;
end;

function TIdSipParser._AddRef: Integer;
begin
  Result := -1;
end;

function TIdSipParser._Release: Integer;
begin
  Result := -1;
end;

//******************************************************************************
//* TIdSipMessageSubject                                                       *
//******************************************************************************
//* TIdSipMessageSubject Public methods ****************************************

constructor TIdSipMessageSubject.Create;
begin
  inherited Create;

  Self.MsgListenerLock := TCriticalSection.Create;
  Self.MsgListeners    := TList.Create;
end;

destructor TIdSipMessageSubject.Destroy;
begin
  Self.MsgListeners.Free;
  Self.MsgListenerLock.Free;

  inherited Destroy;
end;

procedure TIdSipMessageSubject.AddMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.MsgListenerLock.Acquire;
  try
    Self.MsgListeners.Add(Pointer(Listener));
  finally
    Self.MsgListenerLock.Release;
  end;
end;

procedure TIdSipMessageSubject.RemoveMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.MsgListenerLock.Acquire;
  try
    Self.MsgListeners.Remove(Pointer(Listener));
  finally
    Self.MsgListenerLock.Release;
  end;
end;

//* TIdSipMessageSubject Protected methods *************************************

procedure TIdSipMessageSubject.NotifyMessageListeners(const Request: TIdSipRequest);
var
  I: Integer;
begin
  Self.MsgListenerLock.Acquire;
  try
    for I := 0 to Self.MsgListeners.Count - 1 do
      IIdSipMessageListener(Self.MsgListeners[I]).OnReceiveRequest(Request);
  finally
    Self.MsgListenerLock.Release;
  end;
end;

procedure TIdSipMessageSubject.NotifyMessageListeners(const Response: TIdSipResponse);
var
  I: Integer;
begin
  Self.MsgListenerLock.Acquire;
  try
    for I := 0 to Self.MsgListeners.Count - 1 do
      IIdSipMessageListener(Self.MsgListeners[I]).OnReceiveResponse(Response);
  finally
    Self.MsgListenerLock.Release;
  end;
end;

end.
