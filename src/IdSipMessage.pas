unit IdSipMessage;

interface

uses
  Classes, Contnrs, IdDateTimeStamp, IdGlobal, IdURI, SysUtils;

type
  TIdSipQValue = 0..1000;
  TIdSipTransportType = (sttSCTP, sttTCP, sttTLS, sttUDP);

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
    procedure SetName(const Value: String); virtual;
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
    procedure SetValue(const Value: String); override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    function DecodeQuotedStr(const S: String): String;
    function EncodeQuotedStr(const S: String): String;

    property Address:     TIdURI read fAddress write SetAddress;
    property DisplayName: String read fDisplayName write fDisplayName;
  end;

  TIdSipCallIdHeader = class(TIdSipHeader)
  protected
    function  GetName: String; override;
    procedure SetValue(const Value: String); override;
  end;

  TIdSipContactHeader = class(TIdSipAddressHeader)
  private
    function  GetExpires: Cardinal;
    function  GetQ: TIdSipQValue;
    procedure SetExpires(const Value: Cardinal);
    procedure SetQ(const Value: TIdSipQValue);
  protected
    function  GetName: String; override;
    procedure SetValue(const Value: String); override;
  public
    property Expires: Cardinal     read GetExpires write SetExpires;
    property Q:       TIdSipQValue read GetQ write SetQ;
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
    property Tag: String read GetTag write SetTag;
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
    fAddress:     String;
    fDisplayName: String;

    procedure SetAddress(const Value: String);
  protected
    function  GetName: String; override;
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
  public
    function DecodeQuotedStr(const S: String): String;
    function EncodeQuotedStr(const S: String): String;

    property Address:     String read fAddress write SetAddress;
    property DisplayName: String read fDisplayName write fDisplayName;
  end;

  TIdSipRecordRouteHeader = class(TIdSipRouteHeader)
  protected
    function  GetName: String; override;
  end;

  TIdSipViaHeader = class(TIdSipHeader)
  private
    fHost:       String;
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
    function DefaultPortForTransport(const T: TIdSipTransportType): Cardinal;
    function IsDefaultPortForTransport(const Port: Cardinal; const T: TIdSipTransportType): Boolean;
    function IsEqualTo(const Header: TIdSipHeader): Boolean; override;

    property Branch:     String              read GetBranch write SetBranch;
    property Host:       String              read fHost write fHost;
    property Maddr:      String              read GetMaddr write SetMaddr;
    property Port:       Cardinal            read fPort write fPort;
    property Received:   String              read GetReceived write SetReceived;
    property SipVersion: String              read fSipVersion write fSipVersion;
    property Transport:  TIdSipTransportType read fTransport write fTransport;
    property TTL:        Byte                read GetTTL write SetTTL;
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
    class function IsCommaSeparatedHeader(Header: String): Boolean;
    class function IsContact(Header: String): Boolean;
    class function IsContentLength(Header: String): Boolean;
    class function IsCSeq(Header: String): Boolean;
    class function IsFrom(Header: String): Boolean;
    class function IsMaxForwards(Header: String): Boolean;
    class function IsRecordRoute(Header: String): Boolean;
    class function IsRoute(Header: String): Boolean;
    class function IsTo(Header: String): Boolean;
    class function IsVia(Header: String): Boolean;

    constructor Create; virtual;
    destructor  Destroy; override;

    function  Add(const HeaderName: String): TIdSipHeader; overload;
    procedure Add(const Header: TIdSipHeader); overload;
    function  AsString: String;
    procedure Clear;
    function  Count: Integer;

    function  HasHeader(const HeaderName: String): Boolean;

    property  Headers[const Name: String]:  TIdSipHeader read GetHeaders; default;
    property  Items[const I: Integer]:      TIdSipHeader read GetItems;
    property  Values[const Header: String]: String read GetValues write SetValues;
  end;

  TIdSipViaPath = class(TObject)
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

  TIdSipHeadersFilter = class(TObject)
  private
    HeaderName: String;
    Headers:    TIdSipHeaders;

    function GetItems(const Index: Integer): TIdSipHeader;
  public
    constructor Create(const Headers: TIdSipHeaders; const HeaderName: String);

    procedure Add(Header: TIdSipHeader);
    function  Count: Integer;

    property Items[const Index: Integer]: TIdSipHeader read GetItems;
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
    function  GetFrom: TIdSipFromToHeader;
    function  GetMaxForwards: Byte;
    function  GetTo: TIdSipFromToHeader;
    procedure SetCallID(const Value: String);
    procedure SetContentLength(const Value: Cardinal);
    procedure SetContentType(const Value: String);
    procedure SetCSeq(const Value: TIdSipCSeqHeader);
    procedure SetFrom(const Value: TIdSipFromToHeader);
    procedure SetMaxForwards(const Value: Byte);
    procedure SetPath(const Value: TIdSipViaPath);
    procedure SetTo(const Value: TIdSipFromToHeader);
  protected
    function FirstLine: String; virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Assign(Src: TPersistent); override;
    function  AsString: String;
    function  HasHeader(const HeaderName: String): Boolean;
    function  MalformedException: ExceptClass; virtual; abstract;
    procedure ReadBody(const S: TStream);

    property Body:          String              read fBody write fBody;
    property CallID:        String              read GetCallID write SetCallID;
    property ContentLength: Cardinal            read GetContentLength write SetContentLength;
    property ContentType:   String              read GetContentType write SetContentType;
    property CSeq:          TIdSipCSeqHeader    read GetCSeq write SetCSeq;
    property From:          TIdSipFromToHeader  read GetFrom write SetFrom;
    property Headers:       TIdSipHeaders       read fHeaders;
    property MaxForwards:   Byte                read GetMaxForwards write SetMaxForwards;
    property Path:          TIdSipViaPath          read fPath write SetPath;
    property SIPVersion:    String              read fSIPVersion write fSIPVersion;
    property ToHeader:      TIdSipFromToHeader  read GetTo write SetTo;
  end;

  TIdSipRequest = class(TIdSipMessage)
  private
    fMethod:     String;
    fRequest:    String;
  protected
    function FirstLine: String; override;
  public
    procedure Assign(Src: TPersistent); override;
    function  MalformedException: ExceptClass; override;

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
    procedure Assign(Src: TPersistent); override;
    function  MalformedException: ExceptClass; override;

    property StatusCode: Integer read fStatusCode write SetStatusCode;
    property StatusText: String  read fStatusText write fStatusText;
  end;

implementation

uses
  IdSipParser;

var
  GIdSipHeadersMap: TObjectList;

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

function QuoteStringIfNecessary(const Name: String): String;
begin
  Result := Name;
  if NeedsQuotes(Name) then
    Result := '"' + Result + '"'
end;

//******************************************************************************
//* TIdSipHeader                                                               *
//******************************************************************************
//* TIdSipHeader Public methods ************************************************

constructor TIdSipHeader.Create;
begin
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

procedure TIdSipHeader.SetName(const Value: String);
begin
  fName := Value;
end;

procedure TIdSipHeader.SetValue(const Value: String);
var
  ParamName:  String;
  ParamValue: String;
  S:          String;
begin
  Self.Parameters.Clear;

  S := Value;

  if (IndyPos(';', S) = 0) then begin
    fValue := S;
  end
  else begin
    fValue := Trim(Fetch(S, ';'));
    if (IndyPos(';', S) = 0) then begin
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
      Self.FailParse;

    if (Length(Result) >= 2) and (Result[Length(Result)] = '\') and (Result[Length(Result) - 1] <> '\') then
      Self.FailParse;

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

  if (IndyPos('<', S) > 0) then begin
    if (S[1] = '"') then begin
      Name := Trim(Fetch(S, '<'));
      Delete(Name, 1, 1);

      if (IndyPos('"', Name) = 0) then
        Self.FailParse;

      Name := Copy(Name, 1, RPos('"', Name, -1) - 1);

      // There was an encoded ", which MUST NOT match the opening "
      if (Name <> '') and (Name[Length(Name)] = '\') then
        Self.FailParse;

        Self.DisplayName := Self.DecodeQuotedStr(Name);
    end else begin
      Self.DisplayName := Trim(Fetch(S, '<'));
      if NeedsQuotes(Self.DisplayName) then
        Self.FailParse;
    end;

    Self.Address.URI := Trim(Fetch(S, '>'));
  end
  else begin
    // any semicolons in a URI not in angle brackets indicate that the
    // HEADER has the parameters, not the URI
    PreserveSemicolon := IndyPos(';', S) > 0;
    Self.Address.URI := Trim(Fetch(S, ';'));
  end;

  if PreserveSemicolon then
    S := ';' + S;
    
  inherited SetValue(S);
end;

//* TIdSipAddressHeader Private methods ****************************************

procedure TIdSipAddressHeader.SetAddress(const Value: TIdURI);
begin
  fAddress.URI := Value.URI;
end;

//******************************************************************************
//* TIdSipCallIdHeader                                                         *
//******************************************************************************
//* TIdSipCallIdHeader Protected methods ***************************************

function TIdSipCallIdHeader.GetName: String;
begin
  Result := CallIdHeaderFull;
end;

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
//* TIdSipContactHeader                                                        *
//******************************************************************************
//* TIdSipContactHeader Protected methods **************************************

function TIdSipContactHeader.GetName: String;
begin
  Result := ContactHeaderFull;
end;

procedure TIdSipContactHeader.SetValue(const Value: String);
begin
  inherited SetValue(Value);

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

  // this is a bit crap. What if someone uses "xxx, 1 Jan 1899 00:00:00 GMT"?
  if (Self.Time.AsTDateTime = 0) then
    Self.FailParse;
end;

//******************************************************************************
//* TIdSipFromToHeader                                                         *
//******************************************************************************
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

function TIdSipRouteHeader.DecodeQuotedStr(const S: String): String;
var
  I: Integer;
  FoundSlash: Boolean;
begin
  // in summary:
  // '\' is illegal, '%s\' is illegal.

  Result := S;

  if (Result <> '') then begin
    if (Result = '\') then
      Self.FailParse;

    if (Length(Result) >= 2) and (Result[Length(Result)] = '\') and (Result[Length(Result) - 1] <> '\') then
      Self.FailParse;

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

function TIdSipRouteHeader.EncodeQuotedStr(const S: String): String;
begin
  Result := S;
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll, rfIgnoreCase]);
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

  URI := Self.Address;
  URI := '<' + URI + '>';

  if (Result = '') then
    Result := URI
  else
    Result := Result + ' ' + URI;
end;

procedure TIdSipRouteHeader.SetValue(const Value: String);
var
  Name: String;
  S:    String;
  PreserveSemicolon: Boolean;
begin
  PreserveSemicolon := false;
  Self.DisplayName := '';
  Self.Address     := '';

  S := Trim(Value);

  if (IndyPos('<', S) > 0) then begin
    if (S[1] = '"') then begin
      Name := Trim(Fetch(S, '<'));
      Delete(Name, 1, 1);

      if (IndyPos('"', Name) = 0) then
        Self.FailParse;

      Name := Copy(Name, 1, RPos('"', Name, -1) - 1);

      // There was an encoded ", which MUST NOT match the opening "
      if (Name <> '') and (Name[Length(Name)] = '\') then
        Self.FailParse;

        Self.DisplayName := Self.DecodeQuotedStr(Name);
    end else begin
      Self.DisplayName := Trim(Fetch(S, '<'));
      if NeedsQuotes(Self.DisplayName) then
        Self.FailParse;
    end;

    Self.Address := Trim(Fetch(S, '>'));
  end
  else
    Self.FailParse;

  if PreserveSemicolon then
    S := ';' + S;

  inherited SetValue(S);
end;

//* TIdSipRouteHeader Private methods ******************************************

procedure TIdSipRouteHeader.SetAddress(const Value: String);
begin
  fAddress := Value;
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

//* TIdSipViaHeader Protected methods ******************************************

function TIdSipViaHeader.GetName: String;
begin
  Result := ViaHeaderFull;
end;

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
  Self.Host := Fetch(Token, ':');

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

class function TIdSipHeaders.IsCommaSeparatedHeader(Header: String): Boolean;
begin
  Result := Self.IsContact(Header)
         or Self.IsRecordRoute(Header)
         or Self.IsRoute(Header)
         or Self.IsVia(Header);
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

class function TIdSipHeaders.IsRecordRoute(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, RecordRouteHeader);
end;

class function TIdSipHeaders.IsRoute(Header: String): Boolean;
var
  Name: String;
begin
  Name := Self.GetHeaderName(Header);
  Result := IsEqual(Name, RouteHeader);
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

procedure TIdSipHeaders.Add(const Header: TIdSipHeader);
var
  H: TIdSipHeader;
begin
  H := Self.ConstructHeader(Header.Name);
  H.Name := Header.Name;
  H.Assign(Header);

  Self.List.Add(H);
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
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(CallIDHeaderFull,        TIdSipCallIDHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(CallIDHeaderShort,       TIdSipCallIDHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContactHeaderFull,       TIdSipContactHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContactHeaderShort,      TIdSipContactHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ContentLengthHeaderFull, TIdSipNumericHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(CSeqHeader,              TIdSipCSeqHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(DateHeader,              TIdSipDateHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ExpiresHeader,           TIdSipNumericHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(FromHeaderFull,          TIdSipFromToHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(FromHeaderShort,         TIdSipFromToHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(MaxForwardsHeader,       TIdSipMaxForwardsHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(RecordRouteHeader,       TIdSipRecordRouteHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(RouteHeader,             TIdSipRouteHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ToHeaderFull,            TIdSipFromToHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ToHeaderShort,           TIdSipFromToHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ViaHeaderFull,           TIdSipViaHeader));
    GIdSipHeadersMap.Add(TIdSipHeaderMap.Create(ViaHeaderShort,          TIdSipViaHeader));
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
//* TIdSipViaPath                                                              *
//******************************************************************************
//* TIdSipViaPath Public methods ***********************************************

constructor TIdSipViaPath.Create(const Headers: TIdSipHeaders);
begin
  inherited Create;

  Self.Headers := Headers;
end;

procedure TIdSipViaPath.Add(Hop: TIdSipViaHeader);
begin
  Self.Headers.Add(ViaHeaderFull).Assign(Hop);
end;

function TIdSipViaPath.FirstHop: TIdSipViaHeader;
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
end;

function TIdSipViaPath.Length: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Self.Headers.Count - 1 do
    if TIdSipHeaders.IsVia(Self.Headers.Items[I].Name) then
      Inc(Result);
end;

//******************************************************************************
//* TIdSipHeadersFilter                                                        *
//******************************************************************************
//* TIdSipHeadersFilter Public methods *****************************************

constructor TIdSipHeadersFilter.Create(const Headers: TIdSipHeaders; const HeaderName: String);
begin
  Self.HeaderName := HeaderName;
  Self.Headers    := Headers;
end;

procedure TIdSipHeadersFilter.Add(Header: TIdSipHeader);
begin
  Self.Headers.Add(Header);
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

//* TIdSipHeadersFilter Private methods ****************************************

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
//* TIdSipMessage                                                              *
//******************************************************************************
//* TIdSipMessage Public methods ***********************************************

constructor TIdSipMessage.Create;
begin
  inherited Create;

  fHeaders := TIdSipHeaders.Create;
  fPath := TIdSipViaPath.Create(Self.Headers);

//  Self.MaxForwards := DefaultMaxForwards;
  Self.SIPVersion  := IdSipParser.SIPVersion;
end;

destructor TIdSipMessage.Destroy;
begin
  fPath.Free;
  fHeaders.Free;

  inherited Destroy;
end;

procedure TIdSipMessage.Assign(Src: TPersistent);
var
  S: TIdSipMessage;
  I: Integer;
begin
  if (Src is Self.ClassType) then begin
    S := Src as TIdSipMessage;

    Self.SIPVersion := S.SIPVersion;

    Self.Headers.Clear;
    for I := 0 to S.Headers.Count - 1 do
      Self.Headers.Add(S.Headers.Items[I]);
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

function TIdSipMessage.HasHeader(const HeaderName: String): Boolean;
begin
  Result := Self.Headers.HasHeader(HeaderName);
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

function TIdSipMessage.GetContentType: String;
begin
  Result := Self.Headers[ContentTypeHeaderFull].Value;
end;

function TIdSipMessage.GetCSeq: TIdSipCSeqHeader;
begin
  Result := Self.Headers[CSeqHeader] as TIdSipCSeqHeader;
end;

function TIdSipMessage.GetFrom: TIdSipFromToHeader;
begin
  Result := Self.Headers[FromHeaderFull] as TIdSipFromToHeader;
end;

function TIdSipMessage.GetMaxForwards: Byte;
begin
  if (Self.Headers[MaxForwardsHeader].Value = '') then
    Self.MaxForwards := DefaultMaxForwards;

  Result := StrToInt(Self.Headers[MaxForwardsHeader].Value);
end;

function TIdSipMessage.GetTo: TIdSipFromToHeader;
begin
  Result := Self.Headers[ToHeaderFull] as TIdSipFromToHeader;
end;

procedure TIdSipMessage.SetCallID(const Value: String);
begin
  Self.Headers[CallIDHeaderFull].Value := Value;
end;

procedure TIdSipMessage.SetContentLength(const Value: Cardinal);
begin
  Self.Headers[ContentLengthHeaderFull].Value := IntToStr(Value);
end;

procedure TIdSipMessage.SetContentType(const Value: String);
begin
  Self.Headers[ContentTypeHeaderFull].Value := Value;
end;

procedure TIdSipMessage.SetCSeq(const Value: TIdSipCSeqHeader);
begin
  Self.CSeq.Assign(Value);
end;

procedure TIdSipMessage.SetFrom(const Value: TIdSipFromToHeader);
begin
  Self.Headers[FromHeaderFull].Assign(Value);
end;

procedure TIdSipMessage.SetMaxForwards(const Value: Byte);
begin
  Self.Headers[MaxForwardsHeader].Value := IntToStr(Value);
end;

procedure TIdSipMessage.SetPath(const Value: TIdSipViaPath);
begin
//  Self.Path.Assign(Value);
end;

procedure TIdSipMessage.SetTo(const Value: TIdSipFromToHeader);
begin
  Self.Headers[ToHeaderFull].Assign(Value);
end;

//*******************************************************************************
//* TIdSipRequest                                                               *
//*******************************************************************************
//* TIdSipRequest Public methods ************************************************

procedure TIdSipRequest.Assign(Src: TPersistent);
var
  R: TIdSipRequest;
begin
  inherited Assign(Src);

  R := Src as TIdSipRequest;
  
  Self.Method  := R.Method;
  Self.Request := R.Request;
end;

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

procedure TIdSipResponse.Assign(Src: TPersistent);
var
  R: TIdSipResponse;
begin
  inherited Assign(Src);

  R := Src as TIdSipResponse;
  
  Self.StatusCode := R.StatusCode;
  Self.StatusText := R.StatusText;
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

initialization
finalization
  GIdSipHeadersMap.Free;
end.
