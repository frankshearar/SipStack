unit IdSipTransport;

interface

uses
  Classes, IdSipConsts, IdSipMessage, IdSipTcpServer, IdSipUdpServer,
  IdTCPClient, IdTCPServer, IdUDPClient, IdUDPServer, SysUtils;

type
  TIdSipNotifyEvent = TNotifyEvent;
  TIdSipResponseEvent = procedure(Sender: TObject; const R: TIdSipResponse) of object;
  TIdSipRequestEvent = procedure(Sender: TObject; const R: TIdSipRequest) of object;

  TIdSipAbstractTransport = class(TObject)
  private
    fOnRequest:  TIdSipRequestEvent;
    fOnResponse: TIdSipResponseEvent;
  protected
    procedure DoOnRequest(Sender: TObject; const R: TIdSipRequest);
    procedure DoOnResponse(Sender: TObject; const R: TIdSipResponse);
    procedure ExtractHostAndPort(const URL: String; var Host: String; var Port: Cardinal);
    function  GetPort: Cardinal; virtual; abstract;
    procedure SetPort(const Value: Cardinal); virtual; abstract;
  public
    constructor Create(const Port: Cardinal = IdPORT_SIP); virtual;

    // This should raise an appropriate exception if a transport or suchlike
    // error occurs.
    procedure SendRequest(const R: TIdSipRequest); virtual; abstract;
    procedure SendResponse(const R: TIdSipResponse); virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;

    property OnRequest:  TIdSipRequestEvent  read fOnRequest write fOnRequest;
    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
    property Port:       Cardinal            read GetPort write SetPort;
  end;

  TIdSipTransportClass = class of TIdSipAbstractTransport;

  TIdSipTCPTransport = class(TIdSipAbstractTransport)
  private
    Transport: TIdSipTcpServer;

    procedure DoOnTcpRequest(AThread: TIdPeerThread; const Request: TIdSipRequest);
    procedure DoOnTcpResponse(Sender: TObject; const Response: TIdSipResponse);
  protected
    function  GetPort: Cardinal; override;
    procedure SetPort(const Value: Cardinal); override;
  public
    constructor Create(const Port: Cardinal = IdPORT_SIP); override;
    destructor  Destroy; override;

    procedure SendRequest(const R: TIdSipRequest); override;
    procedure SendResponse(const R: TIdSipResponse); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TIdSipTLSTransport = class(TIdSipTCPTransport);

  TIdSipUDPTransport = class(TIdSipAbstractTransport)
  private
    Transport: TIdSipUdpServer;

    procedure DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
  protected
    function  GetPort: Cardinal; override;
    procedure SetPort(const Value: Cardinal); override;
  public
    constructor Create(const Port: Cardinal = IdPORT_SIP); override;
    destructor  Destroy; override;

    procedure SendRequest(const R: TIdSipRequest); override;
    procedure SendResponse(const R: TIdSipResponse); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TIdSipMockTransport = class(TIdSipAbstractTransport)
  private
    fACKCount:          Cardinal;
    fLastACK:           TIdSipRequest;
    fFailWith:          ExceptClass;
    fSentRequestCount:  Cardinal;
    fSentResponseCount: Cardinal;
  public
    constructor Create(const Port: Cardinal = IdPORT_SIP); override;
    destructor  Destroy; override;

    procedure FireOnRequest(const R: TIdSipRequest);
    procedure FireOnResponse(const R: TIdSipResponse);
    procedure RaiseException(const E: ExceptClass);
    procedure ResetACKCount;
    procedure ResetSentRequestCount;
    procedure ResetSentResponseCount;
    procedure SendRequest(const R: TIdSipRequest); override;
    procedure SendResponse(const R: TIdSipResponse); override;
    procedure Start; override;
    procedure Stop; override;

    property ACKCount:          Cardinal      read fACKCount;
    property FailWith:          ExceptClass   read fFailWith write fFailWith;
    property LastACK:           TIdSipRequest read fLastACK;
    property SentRequestCount:  Cardinal      read fSentRequestCount;
    property SentResponseCount: Cardinal      read fSentResponseCount;
  end;

const
  DefaultTimeout = 1000; // milliseconds

implementation

uses
  IdSipTcpClient, IdURI;

//******************************************************************************
//* TIdSipAbstractTransport                                                    *
//******************************************************************************
//* TIdSipAbstractTransport Public methods *************************************

constructor TIdSipAbstractTransport.Create(const Port: Cardinal = IdPORT_SIP);
begin
  inherited Create;
end;

//* TIdSipAbstractTransport Protected methods **********************************

procedure TIdSipAbstractTransport.DoOnRequest(Sender: TObject; const R: TIdSipRequest);
begin
  if Assigned(Self.OnRequest) then
    Self.OnRequest(Sender, R);
end;

procedure TIdSipAbstractTransport.DoOnResponse(Sender: TObject; const R: TIdSipResponse);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Sender, R);
end;

procedure TIdSipAbstractTransport.ExtractHostAndPort(const URL: String; var Host: String; var Port: Cardinal);
var
  URI: TIdURI;
begin
  Host := '';
  Port := 0;

  URI := TIdURI.Create(URL);
  try
    Host := URI.Host;
    Port := StrToIntDef(URI.Port, IdPORT_SIP);
  finally
    URI.Free;
  end;
end;


//******************************************************************************
//* TIdSipTcpTransport                                                         *
//******************************************************************************
//* TIdSipTcpTransport Public methods ******************************************

constructor TIdSipTcpTransport.Create(const Port: Cardinal = IdPORT_SIP);
begin
  inherited Create(Port);

  Self.Transport := TIdSipTcpServer.Create(nil);
  Self.SetPort(Port);
  Self.Transport.OnRequest  := Self.DoOnTcpRequest;
  Self.Transport.OnResponse := Self.DoOnTcpResponse;
end;

destructor TIdSipTcpTransport.Destroy;
begin
  Self.Transport.Free;

  inherited Destroy;
end;

procedure TIdSipTcpTransport.SendRequest(const R: TIdSipRequest);
var
  Client: TIdSipTcpClient;
  Host:   String;
  Port:   Cardinal;
begin
  Client := TIdSipTcpClient.Create(nil);
  try
    Self.ExtractHostAndPort(R.RequestUri, Host, Port);
    Client.Host := Host;
    Client.Port := Port;
    Client.OnResponse := Self.DoOnTcpResponse;

    Client.Connect(DefaultTimeout);
    try
      Client.Send(R);
    finally
      Client.Disconnect;
    end;
  finally
    Client.Free;
  end;
end;

procedure TIdSipTcpTransport.SendResponse(const R: TIdSipResponse);
begin
end;

procedure TIdSipTcpTransport.Start;
begin
  Self.Transport.Active := true;
end;

procedure TIdSipTcpTransport.Stop;
begin
  Self.Transport.Active := false;
end;

//* TIdSipTcpTransport Protected methods ***************************************

function TIdSipTcpTransport.GetPort: Cardinal;
begin
  Result := Self.Transport.DefaultPort;
end;

procedure TIdSipTcpTransport.SetPort(const Value: Cardinal);
begin
  Self.Transport.DefaultPort := Value;
end;

//* TIdSipTcpTransport Private methods *****************************************

procedure TIdSipTcpTransport.DoOnTcpRequest(AThread: TIdPeerThread; const Request: TIdSipRequest);
begin
  Self.DoOnRequest(AThread, Request);
end;

procedure TIdSipTcpTransport.DoOnTcpResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Sender, Response);
end;

//******************************************************************************
//* TIdSipUdpTransport                                                         *
//******************************************************************************
//* TIdSipUdpTransport Public methods ******************************************

constructor TIdSipUdpTransport.Create(const Port: Cardinal = IdPORT_SIP);
begin
  inherited Create(Port);

  Self.Transport := TIdSipUdpServer.Create(nil);
  Self.SetPort(Port);
  Self.Transport.OnRequest  := Self.DoOnReceiveRequest;
  Self.Transport.OnResponse := Self.DoOnReceiveResponse;
end;

destructor TIdSipUdpTransport.Destroy;
begin
  Self.Transport.Free;

  inherited Destroy;
end;

procedure TIdSipUdpTransport.SendRequest(const R: TIdSipRequest);
var
  Client:   TIdUdpClient;
  Host:     String;
  Port:     Cardinal;
  P:        TIdSipParser;
  Response: TIdSipResponse;
  Reply:    String;
begin
  Client := TIdUdpClient.Create(nil);
  try
    Self.ExtractHostAndPort(R.RequestUri, Host, Port);
    Client.Host := Host;
    Client.Port := Port;

    Client.Send(R.AsString);

    P := TIdSipParser.Create;
    try
      Reply := Client.ReceiveString(DefaultTimeout);
      while (Reply <> '') do begin
        Response := P.ParseAndMakeResponse(Reply);
        try
          Self.DoOnReceiveResponse(Self, Response);
        finally
          Response.Free;
        end;

        Reply := Client.ReceiveString(DefaultTimeout);
      end;
    finally
      P.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure TIdSipUdpTransport.SendResponse(const R: TIdSipResponse);
begin
end;

procedure TIdSipUdpTransport.Start;
begin
  Self.Transport.Active := true;
end;

procedure TIdSipUdpTransport.Stop;
begin
  Self.Transport.Active := false;
end;

//* TIdSipTcpTransport Protected methods ***************************************

function TIdSipUdpTransport.GetPort: Cardinal;
begin
  Result := Self.Transport.DefaultPort;
end;

procedure TIdSipUdpTransport.SetPort(const Value: Cardinal);
begin
  Self.Transport.DefaultPort := Value;
end;

//* TIdSipTcpTransport Private methods *****************************************

procedure TIdSipUdpTransport.DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.DoOnRequest(Sender, Request);
end;

procedure TIdSipUdpTransport.DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Sender, Response);
end;

//******************************************************************************
//* TIdSipMockTransport                                                        *
//******************************************************************************
//* TIdSipMockTransport Public methods *****************************************

constructor TIdSipMockTransport.Create(const Port: Cardinal = IdPORT_SIP); 
begin
  inherited Create(Port);

  Self.ResetSentRequestCount;
  Self.fLastACK := TIdSipRequest.Create;
end;

destructor TIdSipMockTransport.Destroy;
begin
  Self.fLastACK.Free;

  inherited Destroy;
end;

procedure TIdSipMockTransport.FireOnRequest(const R: TIdSipRequest);
begin
  Self.DoOnRequest(Self, R);
end;

procedure TIdSipMockTransport.FireOnResponse(const R: TIdSipResponse);
begin
  Self.DoOnResponse(Self, R);
end;

procedure TIdSipMockTransport.RaiseException(const E: ExceptClass);
begin
  raise E.Create('TIdSipMockTransport');
end;

procedure TIdSipMockTransport.ResetACKCount;
begin
  Self.fACKCount := 0;
end;

procedure TIdSipMockTransport.ResetSentRequestCount;
begin
  Self.fSentRequestCount := 0;
end;

procedure TIdSipMockTransport.ResetSentResponseCount;
begin
  Self.fSentResponseCount := 0;
end;

procedure TIdSipMockTransport.SendRequest(const R: TIdSipRequest);
begin
  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendRequest');

  if (R.Method = MethodAck) then begin
    Self.LastACK.Assign(R);
    Inc(Self.fACKCount)
  end
  else
    Inc(Self.fSentRequestCount);

  Self.DoOnRequest(Self, R);
end;

procedure TIdSipMockTransport.SendResponse(const R: TIdSipResponse);
begin
  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendResponse');

  Self.DoOnResponse(Self, R);

  Inc(Self.fSentResponseCount);
end;

procedure TIdSipMockTransport.Start;
begin
end;

procedure TIdSipMockTransport.Stop;
begin
end;

end.
