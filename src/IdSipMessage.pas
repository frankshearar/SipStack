unit IdSipMessage;

interface

uses
  Classes, Contnrs, IdDateTimeStamp, IdGlobal, IdInterfacedObject,
  IdSimpleParser, IdSipHeaders, SyncObjs, SysUtils;

type
  TIdSipRequest = class;
  TIdSipResponse = class;

  TIdSipNotifyEvent = TNotifyEvent;

  IIdSipMessageVisitor = interface
    ['{E2900B55-A1CA-47F1-9DB0-D72D6A846EA0}']
    procedure VisitRequest(Request: TIdSipRequest);
    procedure VisitResponse(Response: TIdSipResponse);
  end;

  TIdSipMessage = class(TPersistent)
  private
    fBody:       String;
    fPath:       TIdSipViaPath;
    fHeaders:    TIdSipHeaders;
    fSIPVersion: String;

    function  GetCallID: String;
    function  GetContentDisposition: TIdSipContentDispositionHeader;
    function  GetContentLength: Cardinal;
    function  GetContentType: String;
    function  GetCSeq: TIdSipCSeqHeader;
    function  GetFrom: TIdSipFromHeader;
    function  GetTo: TIdSipToHeader;
    function  Minimum(A, B: Cardinal): Cardinal;
    function  QuickestContactExpiry: Cardinal;
    function  QuickestExpiresHeader: Cardinal;
    procedure SetCallID(const Value: String);
    procedure SetContentDisposition(Value: TIdSipContentDispositionHeader);
    procedure SetContentLength(Value: Cardinal);
    procedure SetContentType(const Value: String);
    procedure SetCSeq(Value: TIdSipCSeqHeader);
    procedure SetFrom(Value: TIdSipFromHeader);
    procedure SetPath(Value: TIdSipViaPath);
    procedure SetTo(Value: TIdSipToHeader);
  protected
    function FirstLine: String; virtual; abstract;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure Accept(Visitor: IIdSipMessageVisitor); virtual;
    function  AddHeader(const HeaderName: String): TIdSipHeader; overload;
    procedure AddHeader(Header: TIdSipHeader); overload;
    procedure AddHeaders(Headers: TIdSipHeaderList);
    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    procedure ClearHeaders;
    function  ContactCount: Cardinal;
    function  FirstContact: TIdSipContactHeader;
    function  FirstExpires: TIdSipNumericHeader;
    function  FirstHeader(const HeaderName: String): TIdSipHeader;
    function  FirstMinExpires: TIdSipNumericHeader;
    function  HasExpiry: Boolean;
    function  HasHeader(const HeaderName: String): Boolean;
    function  HeaderCount: Integer;
    function  QuickestExpiry: Cardinal;
    function  IsEqualTo(Msg: TIdSipMessage): Boolean; virtual; abstract;
    function  IsRequest: Boolean; virtual; abstract;
    function  LastHop: TIdSipViaHeader;
    function  MalformedException: ExceptClass; virtual; abstract;
    procedure ReadBody(Src: TStream);
    procedure RemoveHeader(Header: TIdSipHeader);
    procedure RemoveAllHeadersNamed(const Name: String);

    property Body:               String                         read fBody write fBody;
    property CallID:             String                         read GetCallID write SetCallID;
    property ContentDisposition: TIdSipContentDispositionHeader read GetContentDisposition write SetContentDisposition;
    property ContentLength:      Cardinal                       read GetContentLength write SetContentLength;
    property ContentType:        String                         read GetContentType write SetContentType;
    property CSeq:               TIdSipCSeqHeader               read GetCSeq write SetCSeq;
    property From:               TIdSipFromHeader               read GetFrom write SetFrom;
    property Headers:            TIdSipHeaders                  read fHeaders;
    property Path:               TIdSipViaPath                  read fPath write SetPath;
    property SIPVersion:         String                         read fSIPVersion write fSIPVersion;
    property ToHeader:           TIdSipToHeader                 read GetTo write SetTo;
  end;

  TIdSipMessageClass = class of TIdSipMessage;

  TIdSipRequest = class(TIdSipMessage)
  private
    fMethod:     String;
    fRequestUri: TIdSipURI;

    function  GetMaxForwards: Byte;
    procedure SetMaxForwards(Value: Byte);
    procedure SetRequestUri(Value: TIdSipURI);
  protected
    function FirstLine: String; override;
  public
    constructor Create; override;

    procedure Accept(Visitor: IIdSipMessageVisitor); override;
    function  AddressOfRecord: String;
    procedure Assign(Src: TPersistent); override;
    function  DefaultMaxForwards: Cardinal;
    function  HasSipsUri: Boolean;
    function  IsAck: Boolean;
    function  IsBye: Boolean;
    function  IsCancel: Boolean;
    function  IsEqualTo(Msg: TIdSipMessage): Boolean; override;
    function  IsInvite: Boolean;
    function  IsRegister: Boolean;
    function  IsRequest: Boolean; override;
    function  MalformedException: ExceptClass; override;
    function  Match(Msg: TIdSipMessage): Boolean;
    function  RequiresResponse: Boolean;

    property MaxForwards: Byte      read GetMaxForwards write SetMaxForwards;
    property Method:      String    read fMethod write fMethod;
    property RequestUri:  TIdSipURI read fRequestUri write SetRequestUri;
  end;

  TIdSipResponse = class(TIdSipMessage)
  private
    fStatusCode: Integer;
    fStatusText: String;

    procedure SetStatusCode(Value: Integer);
  protected
    function FirstLine: String; override;
  public
    procedure Accept(Visitor: IIdSipMessageVisitor); override;
    procedure Assign(Src: TPersistent); override;
    function  IsEqualTo(Msg: TIdSipMessage): Boolean; override;
    function  IsFinal: Boolean;
    function  IsOK: Boolean;
    function  IsProvisional: Boolean;
    function  IsRequest: Boolean; override;
    function  IsTrying: Boolean;
    function  MalformedException: ExceptClass; override;

    property StatusCode: Integer read fStatusCode write SetStatusCode;
    property StatusText: String  read fStatusText write fStatusText;
  end;

  TIdSipParserError = procedure(const RawMessage, Reason: String) of object;

  {*
   * Some implementation principles we follow:
   *  * The original headers may be folded, may contain all manner of guff. We
   *    don't make any attempt to store the raw header - we parse it, and when
   *    we write out the headers we write them in the simplest possible way. As
   *    a result we CANNOT duplicate the exact form of the original message, even
   *    though the new message will be identical, semantically speaking.
   *  * We do (because we have to) keep the order of headers. Any newly created
   *    headers are simply appended.
   *  * Any and all parsing errors are raised as exceptions that descend from
   *    EParser as soon as we can.
   *  * New headers can be created that weren't present in the original message.
   *    These messages will, by default, have the empty string as value. For example,
   *    querying the value of Content-Type will create a TIdSipHeader with Value ''.
   *  * Each header is regarded as using a particular language, and the header
   *    classes are parsers for that language (in the SetValue method).
   *  * Comma-separated headers are always separated into separate headers.
   *}
  TIdSipParser = class(TIdSimpleParser, IIdSipMessageVisitor)
  private
    fOnParserError: TIdSipParserError;

    procedure AddHeader(Msg: TIdSipMessage; Header: String);
    procedure CheckContentLengthContentType(Msg: TIdSipMessage);
    procedure CheckCSeqMethod(Request: TIdSipRequest);
    procedure CheckRequiredRequestHeaders(Msg: TIdSipMessage);
    procedure CheckRequiredResponseHeaders(Msg: TIdSipMessage);
    function  CreateResponseOrRequest(const Token: String): TIdSipMessage;
    procedure DoOnParseError(const Reason: String);
    procedure FailParse(Msg: TIdSipMessage;
                        const Reason: String);
    procedure InitializeMessage(Msg: TIdSipMessage);
    procedure ParseCompoundHeader(Msg: TIdSipMessage;
                                  const Header: String;
                                  Parms: String);
    procedure ParseHeader(Msg: TIdSipMessage; const Header: String);
    procedure ParseHeaders(Msg: TIdSipMessage);
    procedure ParseRequestLine(Request: TIdSipRequest);
    procedure ParseStatusLine(Response: TIdSipResponse);

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

    procedure VisitRequest(Request: TIdSipRequest);
    procedure VisitResponse(Response: TIdSipResponse);

    property OnParserError: TIdSipParserError read fOnParserError write fOnParserError;
  end;

  EBadHeader = class(EParserError);
  EBadRequest = class(EParserError);
  EBadResponse = class(EParserError);

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
function FirstChar(const S: String): String;
function IsEqual(const S1, S2: String): Boolean;
function LastChar(const S: String): String;
function ShortMonthToInt(const Month: String): Integer;
function WithoutFirstAndLastChars(const S: String): String;

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

function FirstChar(const S: String): String;
begin
  Result := Copy(S, 1, 1);
end;

function IsEqual(const S1, S2: String): Boolean;
begin
  Result := Lowercase(S1) = Lowercase(S2);
end;

function LastChar(const S: String): String;
begin
  Result := Copy(S, Length(S), 1);
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

function WithoutFirstAndLastChars(const S: String): String;
begin
  Result := Copy(S, 2, Length(S) - 2);
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

procedure TIdSipMessage.Accept(Visitor: IIdSipMessageVisitor);
begin
end;

function TIdSipMessage.AddHeader(const HeaderName: String): TIdSipHeader;
begin
  Result := Self.Headers.Add(HeaderName)
end;

procedure TIdSipMessage.AddHeader(Header: TIdSipHeader);
begin
  Self.Headers.Add(Header);
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

procedure TIdSipMessage.ReadBody(Src: TStream);
const
  BufLen = 100;
var
  Buf:         array[1..BufLen] of Char;
  BytesToRead: Integer;
  Read:        Integer;
begin
  // The transport must set Content-Length before this method gets called!

  if (Self.ContentLength > 0) then begin
    BytesToRead := Self.ContentLength;

    repeat
      Read := Src.Read(Buf, Min(BufLen, BytesToRead));
      Dec(BytesToRead, Read);

      Self.Body := Self.Body + Copy(Buf, 1, Read);
    until (Read < BufLen) or (BytesToRead <= 0);
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

//* TIdSipMessage Private methods **********************************************

function TIdSipMessage.GetCallID: String;
begin
  Result := Self.FirstHeader(CallIDHeaderFull).Value;
end;

function TIdSipMessage.GetContentDisposition: TIdSipContentDispositionHeader;
begin
  Result := Self.FirstHeader(ContentDispositionHeader) as TIdSipContentDispositionHeader;
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

function TIdSipMessage.GetTo: TIdSipToHeader;
begin
  Result := Self.FirstHeader(ToHeaderFull) as TIdSipToHeader;
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

procedure TIdSipMessage.SetContentDisposition(Value: TIdSipContentDispositionHeader);
begin
  Self.ContentDisposition.Assign(Value);
end;

procedure TIdSipMessage.SetContentLength(Value: Cardinal);
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

  fRequestUri := TIdSipURI.Create('');
  Self.ContentLength := 0;
end;

procedure TIdSipRequest.Accept(Visitor: IIdSipMessageVisitor);
begin
  Visitor.VisitRequest(Self);
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

function TIdSipRequest.DefaultMaxForwards: Cardinal;
begin
  Result := 70;
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

function TIdSipRequest.IsEqualTo(Msg: TIdSipMessage): Boolean;
var
  Request: TIdSipRequest;
begin
  if (Msg is Self.ClassType) then begin
    Request := Msg as TIdSipRequest;

    Result := (Self.SIPVersion     = Request.SIPVersion)
          and (Self.Method         = Request.Method)
          and (Self.RequestUri.URI = Request.RequestUri.URI)
          and (Self.Headers.IsEqualTo(Request.Headers));
  end
  else
    Result := false;
end;

function TIdSipRequest.IsInvite: Boolean;
begin
  Result := Self.Method = MethodInvite;
end;

function TIdSipRequest.IsRegister: Boolean;
begin
  Result := Self.Method = MethodRegister;
end;

function TIdSipRequest.IsRequest: Boolean;
begin
  Result := true;
end;

function TIdSipRequest.MalformedException: ExceptClass;
begin
  Result := EBadRequest;
end;

function TIdSipRequest.Match(Msg: TIdSipMessage): Boolean;
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

//* TIdSipRequest Private methods **********************************************

function TIdSipRequest.GetMaxForwards: Byte;
begin
  if (Self.FirstHeader(MaxForwardsHeader).Value = '') then
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

//*******************************************************************************
//* TIdSipResponse                                                              *
//*******************************************************************************
//* TIdSipResponse Public methods ***********************************************

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

  Self.StatusCode := R.StatusCode;
  Self.StatusText := R.StatusText;
end;

function TIdSipResponse.IsEqualTo(Msg: TIdSipMessage): Boolean;
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

function TIdSipResponse.IsOK: Boolean;
begin
  Result := Self.StatusCode div 100 = 2;
end;

function TIdSipResponse.IsProvisional: Boolean;
begin
  Result := Self.StatusCode div 100 = 1;
end;

function TIdSipResponse.IsRequest: Boolean;
begin
  Result := false;
end;

function TIdSipResponse.IsTrying: Boolean;
begin
  Result := Self.StatusCode = SIPTrying;
end;

function TIdSipResponse.MalformedException: ExceptClass;
begin
  Result := EBadResponse;
end;

//* TIdSipResponse Protected methods *******************************************

function TIdSipResponse.FirstLine: String;
begin
  Result := Format(StatusLine,
                   [Self.SIPVersion, Self.StatusCode, Self.StatusText]);
end;

//* TIdSipResponse Private methods **********************************************

procedure TIdSipResponse.SetStatusCode(Value: Integer);
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
  Name  := Self.GetHeaderName(Header);
  Value := Self.GetHeaderValue(Header);
  Val(Value, Result, E);
  if (E <> 0) then
    Self.FailParse(Msg, Format(MalformedToken, [Name, Header]));
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

    // We can do this safely because we know a SIP response starts with "SIP/",
    // and a Method can't contain the character "/".
    Result := Self.CreateResponseOrRequest(FirstToken);
    try
      Self.ParseMessage(Result);
    except
      Result.Free;

      raise;
    end;
  end
  else
    raise EParserError.Create(EmptyInputStream);
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
  try
    Msg.Accept(Self);
  except
    on E: EParserError do begin
      Self.DoOnParseError(E.Message);
      raise;
    end;
  end;
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

procedure TIdSipParser.VisitRequest(Request: TIdSipRequest);
begin
  Self.ParseRequest(Request);
end;

procedure TIdSipParser.VisitResponse(Response: TIdSipResponse);
begin
  Self.ParseResponse(Response);
end;

//* TIdSipParser Private methods ***********************************************

procedure TIdSipParser.AddHeader(Msg: TIdSipMessage; Header: String);
var
  Name: String;
  S:    String;
begin
  S := Header;
  Name := Trim(Fetch(S, ':'));
  Name := TIdSipHeaders.CanonicaliseName(Name);

  Msg.AddHeader(Name).Value := Trim(S);
end;

procedure TIdSipParser.CheckContentLengthContentType(Msg: TIdSipMessage);
begin
  if (Msg.ContentLength > 0) and (Msg.ContentType = '') then
    Self.FailParse(Msg, MissingContentType);
end;

procedure TIdSipParser.CheckCSeqMethod(Request: TIdSipRequest);
begin
  if (Request.CSeq.Method <> Request.Method) then
    Self.FailParse(Request, CSeqMethodMismatch);
end;

procedure TIdSipParser.CheckRequiredRequestHeaders(Msg: TIdSipMessage);
var
  Request: TIdSipRequest;
begin
  Self.CheckRequiredResponseHeaders(Msg);

  Request := Msg as TIdSipRequest;
  if not Request.HasHeader(MaxForwardsHeader) then
    Request.MaxForwards := Request.DefaultMaxForwards;

//  if not Msg.HasHeader(MaxForwardsHeader) then
//    Self.FailParse(Msg, MissingMaxForwards);
end;

procedure TIdSipParser.CheckRequiredResponseHeaders(Msg: TIdSipMessage);
begin
  if not Msg.HasHeader(CallIDHeaderFull) then
    Self.FailParse(Msg, MissingCallID);

  if not Msg.HasHeader(CSeqHeader) then
    Self.FailParse(Msg, MissingCSeq);

  if not Msg.HasHeader(FromHeaderFull) then
    Self.FailParse(Msg, MissingFrom);

  if not Msg.HasHeader(ToHeaderFull) then
    Self.FailParse(Msg, MissingTo);

  if not Msg.HasHeader(ViaHeaderFull) then
    Self.FailParse(Msg, MissingVia);
end;

function TIdSipParser.CreateResponseOrRequest(const Token: String): TIdSipMessage;
begin
  if (Token = SipName) then
    Result := TIdSipResponse.Create
  else
    Result := TIdSipRequest.Create;
end;

procedure TIdSipParser.DoOnParseError(const Reason: String);
var
  S: TStringStream;
begin
  Self.Source.Seek(0, soFromBeginning);
  S := TStringStream.Create('');
  try
    S.CopyFrom(Self.Source, 0);

    if Assigned(Self.OnParserError) then
      Self.OnParserError(S.DataString, Reason);
  finally
    S.Free;
  end;
end;

procedure TIdSipParser.FailParse(Msg: TIdSipMessage; const Reason: String);
begin
  raise Msg.MalformedException.Create(Reason);
end;

procedure TIdSipParser.InitializeMessage(Msg: TIdSipMessage);
begin
  Msg.ClearHeaders;
  Msg.SipVersion := '';
end;


procedure TIdSipParser.ParseCompoundHeader(Msg: TIdSipMessage;
                                           const Header: String;
                                           Parms: String);
begin
  while (Parms <> '') do
    Msg.AddHeader(Header).Value := Fetch(Parms, ',');
end;

procedure TIdSipParser.ParseHeader(Msg: TIdSipMessage; const Header: String);
begin
  try
    if TIdSipHeaders.IsCompoundHeader(Header) then
      Self.ParseCompoundHeader(Msg, Self.GetHeaderName(Header), Self.GetHeaderValue(Header))
    else
      Self.AddHeader(Msg, Header);
  except
    on E: EBadHeader do
      Self.FailParse(Msg, Format(MalformedToken, [E.Message, Header]));
  end;
end;

procedure TIdSipParser.ParseHeaders(Msg: TIdSipMessage);
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

procedure TIdSipParser.ParseRequestLine(Request: TIdSipRequest);
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
      Self.FailParse(Request, RequestUriNoSpaces)
    else if (Tokens.Count < 3) then
      Self.FailParse(Request, Format(MalformedToken, ['Request-Line', Line]));

    Request.Method := Tokens[0];
    // we want to check the Method
    if not Self.IsMethod(Request.Method) then
      Self.FailParse(Request, Format(MalformedToken, ['Method', Request.Method]));

    URI := Tokens[1];

    if (URI <> '') and (URI[1] = '<') and (URI[Length(URI)] = '>') then
      Self.FailParse(Request, RequestUriNoAngleBrackets);

    Request.RequestUri.URI := URI;

    Request.SIPVersion := Tokens[2];

    if not Self.IsSipVersion(Request.SIPVersion) then
      Self.FailParse(Request, Format(InvalidSipVersion, [Request.SIPVersion]));
  finally
    Tokens.Free;
  end;
end;

procedure TIdSipParser.ParseStatusLine(Response: TIdSipResponse);
var
  Line:   String;
  StatusCode: String;
begin
  // chew up leading blank lines (Section 7.5)
  Line := Self.ReadFirstNonBlankLine;

  Response.SIPVersion := Fetch(Line);
  if not Self.IsSipVersion(Response.SIPVersion) then
    Self.FailParse(Response, Format(InvalidSipVersion, [Response.SIPVersion]));

  StatusCode := Fetch(Line);
  if not Self.IsNumber(StatusCode) then
    Self.FailParse(Response, Format(InvalidStatusCode, [StatusCode]));

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

end.
