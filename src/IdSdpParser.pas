unit IdSdpParser;

interface

uses
  IdSimpleParser, IdURI;

type
  TIdSdpPayload = class(TObject)
  private
    fOrigin:      String;
    fInfo:        String;
    fSessionName: String;
    fURI:         TIdURI;
    fVersion:     Cardinal;

    function GetURI: TIdURI;
  public
    destructor Destroy; override;

    property Origin:      String   read fOrigin write fOrigin;
    property Info:        String   read fInfo write fInfo;
    property SessionName: String   read fSessionName write fSessionName;
    property URI:         TIdURI   read  GetURI;
    property Version:     Cardinal read fVersion write fVersion;
  end;

  TIdSdpParser = class(TIdSimpleParser)
  private
    procedure ParseInfo(const Payload: TIdSdpPayload);
    procedure ParseOrigin(const Payload: TIdSdpPayload);
    procedure ParseSessionName(const Payload: TIdSdpPayload);
    procedure ParseURI(const Payload: TIdSdpPayload);
    procedure ParseVersion(const Payload: TIdSdpPayload);
  public
    class function IsInfo(const S: String): Boolean;
    class function IsText(const S: String): Boolean;
    class function IsUri(const S: String): Boolean;

    procedure Parse(const Payload: TIdSdpPayload);
  end;

const
  MissingOrigin      = 'Missing origin-field';
  MissingSessionName = 'Missing session-name-field';
  MissingVersion     = 'Missing proto-version';

const
  RSSDPOriginName      = 'o';
  RSSDPInformationName = 'i';
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
  fURI.Free;

  inherited Destroy;
end;
//* TIdSdpPayload Private methods **********************************************

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

class function TIdSdpParser.IsInfo(const S: String): Boolean;
begin
  Result := (Copy(S, 1, 2) = RSSDPInformationName + '=');
end;

class function TIdSdpParser.IsText(const S: String): Boolean;
var
  I: Integer;
begin
  Result := (S <> '');

  if Result then
    for I := 1 to Length(S) do
      Result := Result and (S[I] in [#1..#9, #$b, #$c, #$e..#$ff]);
end;

class function TIdSdpParser.IsUri(const S: String): Boolean;
begin
  Result := (Copy(S, 1, 2) = RSSDPUriName + '=');
end;

procedure TIdSdpParser.Parse(const Payload: TIdSdpPayload);
begin
  Self.ParseVersion(Payload);
  Self.ParseOrigin(Payload);
  Self.ParseSessionName(Payload);

  Self.ParseInfo(Payload);
  Self.ParseUri(Payload);
end;

//* TIdSdpParser Private methods ***********************************************

procedure TIdSdpParser.ParseInfo(const Payload: TIdSdpPayload);
var
  Line:        String;
  Name, Value: String;
begin
  if Self.IsInfo(Self.PeekLine) then begin
    Line  := Self.ReadLn;
    Value := Line;

    Name := Fetch(Value, '=');

    if not Self.IsText(Value) then
      raise EParser.Create(Format(MalformedToken, [RSSDPInformationName, Value]));

    Payload.Info := Value;
  end else begin
    Payload.Info := '';
  end;
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
end;

procedure TIdSdpParser.ParseURI(const Payload: TIdSdpPayload);
var
  Line:        String;
  Name, Value: String;
begin
  if Self.IsURI(Self.PeekLine) then begin
    Line  := Self.ReadLn;
    Value := Line;

    Name := Fetch(Value, '=');

//    if not Self.IsUri(Value) then
//      raise EParser.Create(Format(MalformedToken, [RSSDPUriName, Value]));

    Payload.URI.URI := Value;
  end else begin
    Payload.URI.URI := '';
  end;
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
end;

end.
