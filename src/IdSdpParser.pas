unit IdSdpParser;

interface

uses
  IdEmailAddress, IdSimpleParser, IdURI;

type
  TIdIPVersion = (Id_IPv4, Id_IPv6);

  TIdSdpConnection = class(TObject)
  private
    fAddress:     String;
    fAddressType: TIdIPVersion;
    fNetType:     String;
  public
    property AddressType: TIdIPVersion read fAddressType write fAddressType;
    property Address:     String       read fAddress write fAddress;
    property NetType:     String       read fNetType write fNetType;
  end;

  TIdSdpPayload = class(TObject)
  private
    fConnection:   TIdSdpConnection;
    fEmailAddress: TIdEmailAddressItem;
    fInfo:         String;
    fOrigin:       String;
    fPhoneNumber:  String;
    fSessionName:  String;
    fURI:          TIdURI;
    fVersion:      Cardinal;

    function GetConnection: TIdSdpConnection;
    function GetEmailAddress: TIdEmailAddressItem;
    function GetURI: TIdURI;
  public
    destructor Destroy; override;

    property Connection:   TIdSdpConnection    read GetConnection;
    property EmailAddress: TIdEMailAddressItem read GetEmailAddress;
    property Info:         String              read fInfo write fInfo;
    property Origin:       String              read fOrigin write fOrigin;
    property PhoneNumber:  String              read fPhoneNumber write fPhoneNumber;
    property SessionName:  String              read fSessionName write fSessionName;
    property URI:          TIdURI              read GetURI;
    property Version:      Cardinal            read fVersion write fVersion;
  end;

  TIdSdpParser = class(TIdSimpleParser)
  private
    LastHeader:            Char;
    ParsingSessionHeaders: Boolean;

    procedure AssertHeaderOrder;
    procedure ParseConnection(const Payload: TIdSdpPayload);
    procedure ParseEmail(const Payload: TIdSdpPayload);
    procedure ParseHeader(var Name, Value: String);
    procedure ParseInfo(const Payload: TIdSdpPayload);
    procedure ParseOrigin(const Payload: TIdSdpPayload);
    procedure ParsePhone(const Payload: TIdSdpPayload);
    procedure ParseSessionHeaders(const Payload: TIdSdpPayload);
    procedure ParseSessionOptionalHeaders(const Payload: TIdSdpPayload);
    procedure ParseSessionName(const Payload: TIdSdpPayload);
    procedure ParseURI(const Payload: TIdSdpPayload);
    procedure ParseVersion(const Payload: TIdSdpPayload);
  public
    class function IsPhone(const Token: String): Boolean;
    class function IsPhoneNumber(const Header: String): Boolean;
    class function IsText(const Token: String): Boolean;

    procedure Parse(const Payload: TIdSdpPayload);
  end;

const
  BadHeaderOrder        = 'Headers in the wrong order';
  MissingOrigin         = 'Missing origin-field';
  MissingSessionName    = 'Missing session-name-field';
  MissingVersion        = 'Missing proto-version';
  TooManyHeaders        = 'Header ''%s'' occured multiple times';
  UnknownOptionalHeader = 'Unknown optional header: ''%s''';

const
  AddressTypeIP4 = 'IP4';
  AddressTypeIP6 = 'IP6';
  SafeChars = ['a'..'z', 'A'..'Z', '0'..'9', '''', '-', '.', '/', ':', '?', '#',
               '$', '&', '*', ';', '=', '@', '[', ']', '^', '_', '`', '{', '|',
               '}', '+', '~', '"'];
  EmailSafeChars = SafeChars + [' ', #9];
  SessionHeaderOrder = 'vosiuepcbtka';
  //                            ** * <-- * indicates that this header can occur multiple times
  MediaHeaderOrder   = 'micbka';
  //                      ** * <-- * indicates that this header can occur multiple times
const
  RSSDPConnectionName  = 'c';
  RSSDPEmailName       = 'e';
  RSSDPOriginName      = 'o';
  RSSDPInformationName = 'i';
  RSSDPPhoneName       = 'p';
  RSSDPSessionName     = 's';
  RSSDPUriName         = 'u';
  RSSDPVersionName     = 'v';

function StrToAddressType(const S: String): TIdIPVersion;
function AddressTypeToStr(const Version: TIdIPVersion): String;

implementation

uses
  IdGlobal, SysUtils;

//******************************************************************************
//* Unit public functions and procedures                                       *
//******************************************************************************

function StrToAddressType(const S: String): TIdIPVersion;
begin
       if (S = AddressTypeIP4) then Result := Id_IPv4
  else if (S = AddressTypeIP6) then Result := Id_IPv6
  else
    raise EConvertError.Create('Couldn''t convert ''' + S + ''' to type TIdIPVersion');
end;

function AddressTypeToStr(const Version: TIdIPVersion): String;
begin
  case Version of
    Id_IPv4: Result := AddressTypeIP4;
    Id_IPv6: Result := AddressTypeIP6;
  else
    raise EConvertError.Create('Couldn''t convert ' + ' to type String');
  end;
end;

//******************************************************************************
//* TIdSdpPayload                                                              *
//******************************************************************************
//* TIdSdpPayload Public methods ***********************************************

destructor TIdSdpPayload.Destroy;
begin
  fConnection.Free;
  fEmailAddress.Free;
  fURI.Free;

  inherited Destroy;
end;

//* TIdSdpPayload Private methods **********************************************

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

class function TIdSdpParser.IsText(const Token: String): Boolean;
var
  I: Integer;
begin
  Result := (Token <> '');

  if Result then
    for I := 1 to Length(Token) do
      Result := Result and (Token[I] in [#1..#9, #$b, #$c, #$e..#$ff]);
end;

procedure TIdSdpParser.Parse(const Payload: TIdSdpPayload);
begin
  Self.ParseSessionHeaders(Payload);
end;

//* TIdSdpParser Private methods ***********************************************

procedure TIdSdpParser.AssertHeaderOrder;
var
  CurrentHeader: Char;
  HeaderOrder:   String;
begin
  // Self.PeekChar is the header we have. Call this CurrentHeader.
  // Let's look in the appropriate header order to see if this header
  // occurs in the wrong place. The "wrong place" means that we have
  // already processed a successor header. Call this header LastHeader

  CurrentHeader := Self.Peek;

  if Self.ParsingSessionHeaders then
    HeaderOrder := SessionHeaderOrder
  else
    HeaderOrder := MediaHeaderOrder;

  if (IndyPos(LastHeader, HeaderOrder) > IndyPos(CurrentHeader, HeaderOrder)) then
     raise EParser.Create(BadHeaderOrder);
end;

procedure TIdSdpParser.ParseConnection(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

  Payload.Connection.NetType     := Fetch(Value, ' ');
  Payload.Connection.AddressType := StrToAddressType(Fetch(Value, ' '));
  Payload.Connection.Address     := Value;

{
   connection-field =    ["c=" nettype space addrtype space
                         connection-address CRLF]
                         ;a connection field must be present
                         ;in every media description or at the
                         ;session-level

   nettype =             "IN"
                         ;list to be extended
   addrtype =            "IP4" | "IP6"
                         ;list to be extended

   connection-address =  multicast-address
                         | addr
   multicast-address =   3*(decimal-uchar ".") decimal-uchar "/" ttl
                         [ "/" integer ]
                         ;multicast addresses may be in the range
                         ;224.0.0.0 to 239.255.255.255
   ttl =                 decimal-uchar
   addr =                FQDN | unicast-address
   FQDN =                4*(alpha-numeric|"-"|".")
                         ;fully qualified domain name as specified in RFC1035
   unicast-address =     IP4-address | IP6-address
   IP4-address =         b1 "." decimal-uchar "." decimal-uchar "." b4
   b1 =                  decimal-uchar
                         ;less than "224"; not "0" or "127"
   b4 =                  decimal-uchar
                         ;not "0"
   IP6-address =         ;to be defined
}
  Self.LastHeader := RSSDPConnectionName;
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
  Self.LastHeader := RSSDPEmailName;
end;

procedure TIdSdpParser.ParseHeader(var Name, Value: String);
var
  Line: String;
begin
  Line  := Self.ReadLn;
  Value := Line;
  Name  := Fetch(Value, '=');

  if (Name <> Trim(Name)) or (Value <> Trim(Value)) then
    raise EParser.Create(Format(MalformedToken, [Trim(Name), Line]));
end;

procedure TIdSdpParser.ParseInfo(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

  if not Self.IsText(Value) then
    raise EParser.Create(Format(MalformedToken, [RSSDPInformationName, Value]));

  Payload.Info := Value;
  Self.LastHeader := RSSDPInformationName;
end;

procedure TIdSdpParser.ParseOrigin(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.ParseHeader(Name, Value);

  if (Name <> RSSDPOriginName) then
    raise EParser.Create(MissingOrigin);

  Payload.Origin := Value;
  Self.LastHeader := RSSDPOriginName;
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
  Self.LastHeader := RSSDPPhoneName;
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
  while not Self.Eof and (NextHeader <> '') do begin
    case NextHeader[1] of
      RSSDPConnectionName:  Self.ParseConnection(Payload);
      RSSDPEmailName:       Self.ParseEmail(Payload);
      RSSDPInformationName: Self.ParseInfo(Payload);
      RSSDPPhoneName:       Self.ParsePhone(Payload);
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
  Self.LastHeader := RSSDPSessionName;
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
  Self.LastHeader := RSSDPUriName;
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
  Self.LastHeader := RSSDPVersionName;
end;

end.
