unit IdSdpParser;

interface

uses
  Contnrs, IdAssignedNumbers, IdEmailAddress, IdSimpleParser, IdURI;

type
  TIdIPVersion = (Id_IPv4, Id_IPv6);
  TIdSdpBandwidthType = (btCT, btAS, btRS, btRR);

  TIdSdpBandwidth = class(TObject)
  private
    fBandwidth:     Cardinal;
    fBandwidthType: TIdSdpBandwidthType;
  public
    property Bandwidth:     Cardinal            read fBandwidth write fBandwidth;
    property BandwidthType: TIdSdpBandwidthType read fBandwidthType write fBandwidthType;
  end;

  TIdSdpBandwidths = class(TObject)
  private
    List: TObjectList;

    function GetItems(Index: Integer): TIdSdpBandwidth;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(BW: TIdSdpBandwidth);
    procedure Clear;
    function  Count: Integer;

    property Items[Index: Integer]: TIdSdpBandwidth read GetItems; default;
  end;

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

  TIdSdpOrigin = class(TObject)
  private
    fAddress:        String;
    fAddressType:    TIdIPVersion;
    fNetType:        String;
    fSessionID:      String;
    fSessionVersion: String;
    fUsername:       String;
  public
    property Address:        String       read fAddress write fAddress;
    property AddressType:    TIdIPVersion read fAddressType write fAddressType;
    property NetType:        String       read fNetType write fNetType;
    property SessionID:      String       read fSessionID write fSessionID;
    property SessionVersion: String       read fSessionVersion write fSessionVersion;
    property Username:       String       read fUsername write fUsername;
  end;

  TIdSdpPayload = class(TObject)
  private
    fBandwidths:   TIdSdpBandwidths;
    fConnection:   TIdSdpConnection;
    fEmailAddress: TIdEmailAddressItem;
    fInfo:         String;
    fOrigin:       TIdSdpOrigin;
    fPhoneNumber:  String;
    fSessionName:  String;
    fURI:          TIdURI;
    fVersion:      Cardinal;

    function GetBandwidths: TIdSdpBandwidths;
    function GetConnection: TIdSdpConnection;
    function GetEmailAddress: TIdEmailAddressItem;
    function GetOrigin: TIdSdpOrigin;
    function GetURI: TIdURI;
  public
    destructor Destroy; override;

    function HasConnection: Boolean;

    property Bandwidths:   TIdSdpBandwidths    read GetBandwidths;
    property Connection:   TIdSdpConnection    read GetConnection;
    property EmailAddress: TIdEMailAddressItem read GetEmailAddress;
    property Info:         String              read fInfo write fInfo;
    property Origin:       TIdSdpOrigin        read GetOrigin;
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
    procedure ParseBandwidth(const Payload: TIdSdpPayload);
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
    class function IsAddressType(const Token: String): Boolean;
    class function IsBandwidthType(const Token: String): Boolean;
    class function IsNetType(const Token: String): Boolean;
    class function IsNumber(const Token: String): Boolean;
    class function IsPhone(const Token: String): Boolean;
    class function IsPhoneNumber(const Header: String): Boolean;
    class function IsText(const Token: String): Boolean;

    procedure Parse(const Payload: TIdSdpPayload);
  end;

const
  BadHeaderOrder        = 'Headers in the wrong order';
  ConvertEnumErrorMsg   = 'Couldn''t convert a %s with Ord() = %d to type %s';
  ConvertStrErrorMsg    = 'Couldn''t convert ''%s'' to type %s';
  MissingOrigin         = 'Missing origin-field';
  MissingSessionName    = 'Missing session-name-field';
  MissingVersion        = 'Missing proto-version';
  TooManyHeaders        = 'Header ''%s'' occured multiple times';
  UnknownOptionalHeader = 'Unknown optional header: ''%s''';

const
  SafeChars = ['a'..'z', 'A'..'Z', '0'..'9', '''', '-', '.', '/', ':', '?', '#',
               '$', '&', '*', ';', '=', '@', '[', ']', '^', '_', '`', '{', '|',
               '}', '+', '~', '"'];
  EmailSafeChars = SafeChars + [' ', #9];
  SessionHeaderOrder = 'vosiuepcbtka';
  //                            ** * <-- * indicates that this header can occur multiple times
  MediaHeaderOrder   = 'micbka';
  //                      ** * <-- * indicates that this header can occur multiple times

// for IdAssignedNumbers
const
  // IANA assigned bwtype
  Id_SDP_CT = 'CT';
  Id_SDP_AS = 'AS';
  Id_SDP_RS = 'RS';
  Id_SDP_RR = 'RR';
  // IANA assigned nettype
  Id_SDP_IN = 'IN';
  // IANA assigned addrtype
  Id_SDP_IP4 = 'IP4';
  Id_SDP_IP6 = 'IP6';

// for IdResourceStrings
const
  RSSDPBandwidthName   = 'b';
  RSSDPConnectionName  = 'c';
  RSSDPEmailName       = 'e';
  RSSDPOriginName      = 'o';
  RSSDPInformationName = 'i';
  RSSDPPhoneName       = 'p';
  RSSDPSessionName     = 's';
  RSSDPUriName         = 'u';
  RSSDPVersionName     = 'v';

function AddressTypeToStr(const Version: TIdIPVersion): String;
function BandwidthTypeToStr(const BwType: TIdSdpBandwidthType): String;
function StrToAddressType(const S: String): TIdIPVersion;
function StrToBandwidthType(const S: String): TIdSdpBandwidthType;

implementation

uses
  IdGlobal, SysUtils;

//******************************************************************************
//* Unit public functions and procedures                                       *
//******************************************************************************

function AddressTypeToStr(const Version: TIdIPVersion): String;
begin
  case Version of
    Id_IPv4: Result := Id_SDP_IP4;
    Id_IPv6: Result := Id_SDP_IP6;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg, ['TIdIPVersion', Ord(Version), 'String']));
  end;
end;

function BandwidthTypeToStr(const BwType: TIdSdpBandwidthType): String;
begin
  case BwType of
    btCT: Result := Id_SDP_CT;
    btAS: Result := Id_SDP_AS;
    btRS: Result := Id_SDP_RS;
    btRR: Result := Id_SDP_RR;
  else
    raise EConvertError.Create(Format(ConvertEnumErrorMsg, ['TIdSdpBandwidthType', Ord(BwType), 'String']));
  end;
end;

function StrToAddressType(const S: String): TIdIPVersion;
begin
       if (S = Id_SDP_IP4) then Result := Id_IPv4
  else if (S = Id_SDP_IP6) then Result := Id_IPv6
  else
    raise EConvertError.Create(Format(ConvertStrErrorMsg, [S, 'TIdIPVersion']));
end;

function StrToBandwidthType(const S: String): TIdSdpBandwidthType;
begin
       if (S = Id_SDP_CT) then Result := btCT
  else if (S = Id_SDP_AS) then Result := btAS
  else if (S = Id_SDP_RS) then Result := btRS
  else if (S = Id_SDP_RR) then Result := btRR
  else
    raise EConvertError.Create(Format(ConvertStrErrorMsg, [S, 'TIdSdpBandwidthType']));
end;

//******************************************************************************
//* TIdSdpBandwidths                                                           *
//******************************************************************************
//* TIdSdpBandwidths Public methods ********************************************

constructor TIdSdpBandwidths.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TIdSdpBandwidths.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

procedure TIdSdpBandwidths.Add(BW: TIdSdpBandwidth);
begin
  Self.List.Add(BW);
end;

procedure TIdSdpBandwidths.Clear;
begin
  Self.List.Clear;
end;

function TIdSdpBandwidths.Count: Integer;
begin
  Result := Self.List.Count;
end;

//* TIdSdpBandwidths Private methods *******************************************

function TIdSdpBandwidths.GetItems(Index: Integer): TIdSdpBandwidth;
begin
  Result := Self.List[Index] as TIdSdpBandwidth;
end;

//******************************************************************************
//* TIdSdpPayload                                                              *
//******************************************************************************
//* TIdSdpPayload Public methods ***********************************************

destructor TIdSdpPayload.Destroy;
begin
  fBandwidths.Free;
  fConnection.Free;
  fEmailAddress.Free;
  fOrigin.Free;
  fURI.Free;

  inherited Destroy;
end;

function TIdSdpPayload.HasConnection: Boolean;
begin
  Result := Assigned(fConnection);
end;

//* TIdSdpPayload Private methods **********************************************

function TIdSdpPayload.GetBandwidths: TIdSdpBandwidths;
begin
  if not Assigned(fBandwidths) then
    fBandwidths := TIdSdpBandwidths.Create;

  Result := fBandwidths;
end;

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

function TIdSdpPayload.GetOrigin: TIdSdpOrigin;
begin
  if not Assigned(fOrigin) then
    fOrigin := TIdSdpOrigin.Create;

  Result := fOrigin;
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

class function TIdSdpParser.IsAddressType(const Token: String): Boolean;
begin
  Result := (Token = Id_SDP_IP4) or (Token = Id_SDP_IP6);
end;

class function TIdSdpParser.IsBandwidthType(const Token: String): Boolean;
begin
  Result := (Token = Id_SDP_CT)
         or (Token = Id_SDP_AS)
         or (Token = Id_SDP_RS)
         or (Token = Id_SDP_RR);
end;

class function TIdSdpParser.IsNetType(const Token: String): Boolean;
begin
  Result := (Token = Id_SDP_IN);
end;

class function TIdSdpParser.IsNumber(const Token: String): Boolean;
var
  I: Integer;
begin
  Result := Token <> '';

  if Result then
    for I := 1 to Length(Token) do
      Result := Result and (Token[I] in ['0'..'9']);
end;

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
    for I := 1 to Length(Token) do begin
      Result := Result and not (Token[I] in [#0, #10, #13]);
    end;
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

procedure TIdSdpParser.ParseBandwidth(const Payload: TIdSdpPayload);
var
  BW:            TIdSdpBandwidth;
  Name:          String;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);

  BW := TIdSdpBandwidth.Create;
  Payload.Bandwidths.Add(BW);

  Token := Fetch(Value, ':');

  if not Self.IsBandwidthType(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));

  BW.BandwidthType := StrToBandwidthType(Token);

  if not Self.IsNumber(Value) then
    raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));
  BW.Bandwidth     := StrToInt(Value);

  Self.LastHeader := RSSDPBandwidthName;
end;

procedure TIdSdpParser.ParseConnection(const Payload: TIdSdpPayload);
var
  Name:          String;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  Self.AssertHeaderOrder;
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  Token := Fetch(Value, ' ');
  Payload.Connection.NetType := Token;
  if (Payload.Connection.NetType = '') then
    raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));

  Token := Fetch(Value, ' ');
  if not Self.IsAddressType(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));

  Payload.Connection.AddressType := StrToAddressType(Token);

  Payload.Connection.Address := Value;
  if  (Payload.Connection.Address = '') then
    raise EParser.Create(Format(MalformedToken, [RSSDPConnectionName, OriginalValue]));

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

  if (Name = '') then
    raise EParser.Create(Format(MalformedToken, ['Header', Line]));

  if (Value = '') then
    raise EParser.Create(Format(MalformedToken, [Name, Line]));

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
  Name:          String;
  OriginalValue: String;
  Token:         String;
  Value:         String;
begin
  Self.ParseHeader(Name, Value);
  OriginalValue := Value;

  if (Name <> RSSDPOriginName) then
    raise EParser.Create(MissingOrigin);

  Payload.Origin.Username := Fetch(Value, ' ');
  if (Payload.Origin.Username = '') then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));

  Token := Fetch(Value, ' ');
  if not Self.IsNumber(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));
  Payload.Origin.SessionID := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsNumber(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));
  Payload.Origin.SessionVersion := Token;    

  Token := Fetch(Value, ' ');
  if not Self.IsNetType(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));
  Payload.Origin.NetType := Token;

  Token := Fetch(Value, ' ');
  if not Self.IsAddressType(Token) then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));

  Payload.Origin.AddressType := StrToAddressType(Token);

  Payload.Origin.Address := Value;
  if (Payload.Origin.Address = '') then
    raise EParser.Create(Format(MalformedToken, [RSSDPOriginName, OriginalValue]));

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
      RSSDPBandwidthName:   Self.ParseBandwidth(Payload);
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
