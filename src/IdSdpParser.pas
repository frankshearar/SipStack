unit IdSdpParser;

interface

uses
  IdSimpleParser;

type
  TIdSdpPayload = class(TObject)
  private
    fVersion: String;
  public
    property Version: String read fVersion write fVersion;
  end;

  TIdSdpParser = class(TIdSimpleParser)
  private
    procedure ParseOrigin(const Payload: TIdSdpPayload);
    procedure ParseSessionName(const Payload: TIdSdpPayload);
    procedure ParseVersion(const Payload: TIdSdpPayload);
  public
    procedure Parse(const Payload: TIdSdpPayload);
  end;

const
  MissingOrigin      = 'Missing origin-field';
  MissingSessionName = 'Missing session-name-field';
  MissingVersion     = 'Missing proto-version';

const
  RSSDPOriginName  = 'o';
  RSSDPSessionName = 's';
  RSSDPVersionName = 'v';

implementation

uses
  IdGlobal;

//******************************************************************************
//* TIdSdpParser                                                               *
//******************************************************************************
//* TIdSdpParser Public methods ************************************************

procedure TIdSdpParser.Parse(const Payload: TIdSdpPayload);
begin
  Self.ParseVersion(Payload);
  Self.ParseOrigin(Payload);
  Self.ParseSessionName(Payload);
end;

//* TIdSdpParser Private methods ***********************************************

procedure TIdSdpParser.ParseOrigin(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Value := Self.ReadLn;

  Name := Fetch(Value, '=');
  if (Name <> RSSDPOriginName) then
    raise EParser.Create(MissingOrigin);
end;

procedure TIdSdpParser.ParseSessionName(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  Value := Self.ReadLn;

  Name := Fetch(Value, '=');
  if (Name <> RSSDPSessionName) then
    raise EParser.Create(MissingSessionName);
end;

procedure TIdSdpParser.ParseVersion(const Payload: TIdSdpPayload);
var
  Name, Value: String;
begin
  if Self.Eof then
    raise EParser.Create(EmptyInputStream);

  Value := Self.ReadLn;

  Name := Fetch(Value, '=');
  if (Name <> RSSDPVersionName) then
    raise EParser.Create(MissingVersion);
end;

end.
