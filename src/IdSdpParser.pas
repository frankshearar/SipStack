unit IdSdpParser;

interface

uses
  IdEmailAddress, IdSimpleParser, IdURI;

type
  TIdSdpPayload = class(TObject)
  private
    fEmailAddress: TIdEmailAddressItem;
    fInfo:         String;
    fOrigin:       String;
    fPhoneNumber:  String;
    fSessionName:  String;
    fURI:          TIdURI;
    fVersion:      Cardinal;

    function GetEmailAddress: TIdEmailAddressItem;
    function GetURI: TIdURI;
  public
    destructor Destroy; override;

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
    procedure ParseEmail(const Payload: TIdSdpPayload);
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
  BadHeaderOrder     = 'Headers in the wrong order';
  MissingOrigin      = 'Missing origin-field';
  MissingSessionName = 'Missing session-name-field';
  MissingVersion     = 'Missing proto-version';

const
  SafeChars = ['a'..'z', 'A'..'Z', '0'..'9', '''', '-', '.', '/', ':', '?', '#',
               '$', '&', '*', ';', '=', '@', '[', ']', '^', '_', '`', '{', '|',
               '}', '+', '~', '"'];
  EmailSafeChars = SafeChars + [' ', #9];             

  SessionHeaderOrder = 'vosiuepcbtka';
  //                            ** * <-- * indicates that this header can occur multiple times
  MediaHeaderOrder   = 'micbka';
  //                      ** * <-- * indicates that this header can occur multiple times
const
  RSSDPEmailName       = 'e';
  RSSDPOriginName      = 'o';
  RSSDPInformationName = 'i';
  RSSDPPhoneName       = 'p';
  RSSDPSessionName     = 's';
  RSSDPUriName         = 'u';
  RSSDPVersionName     = 'v';

implementation

uses
  IdGlobal, SysUtils;

//******************************************************************************
//* TIdSdpPayload                                                              *
//******************************************************************************
//* TIdSdpPayload Public methods ***********************************************

destructor TIdSdpPayload.Destroy;
begin
  fEmailAddress.Free;
  fURI.Free;

  inherited Destroy;
end;
//* TIdSdpPayload Private methods **********************************************

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

procedure TIdSdpParser.ParseEmail(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Value := Self.ReadLn;

  Name := Fetch(Value, '=');

//  if not Self.IsEmailAddress(Value) then
//    raise EParser.Create(Format(MalformedToken, [RSSDPEmailName, Value]));

  Payload.EmailAddress.Text := Value;
  Self.LastHeader := RSSDPEmailName;
end;

procedure TIdSdpParser.ParseInfo(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Self.AssertHeaderOrder;
  Value := Self.ReadLn;

  Name := Fetch(Value, '=');

  if not Self.IsText(Value) then
    raise EParser.Create(Format(MalformedToken, [RSSDPInformationName, Value]));

  Payload.Info := Value;
  Self.LastHeader := RSSDPInformationName;
end;

procedure TIdSdpParser.ParseOrigin(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Value := Self.ReadLn;

  Name := Fetch(Value, '=');
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
  Value := Self.ReadLn;

  Name := Fetch(Value, '=');

  if not Self.IsPhoneNumber(Value) then
    raise EParser.Create(Format(MalformedToken, [RSSDPPhoneName, Value]));

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
      RSSDPEmailName:       Self.ParseEmail(Payload);
      RSSDPInformationName: Self.ParseInfo(Payload);
      RSSDPPhoneName:       Self.ParsePhone(Payload);
      RSSDPUriName:         Self.ParseUri(Payload);
    else
      raise EParser.Create('Unknown header: ' + NextHeader);
    end;

    NextHeader := Self.PeekLine;
  end;
end;

procedure TIdSdpParser.ParseSessionName(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Value := Self.ReadLn;

  Name := Fetch(Value, '=');
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
  Value := Self.ReadLn;

  Name := Fetch(Value, '=');

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

  Value := Self.ReadLn;

  Name := Fetch(Value, '=');
  if (Name <> RSSDPVersionName) then
    raise EParser.Create(MissingVersion);

  Val(Value, N, E);

  if (E <> 0) then
    raise EParser.Create(Format(MalformedToken, [RSSDPVersionName, Value]));

  Payload.Version := N;
  Self.LastHeader := RSSDPVersionName;
end;

end.
