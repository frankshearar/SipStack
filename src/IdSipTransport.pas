unit IdSipTransport;

interface

uses
  Classes, Contnrs, IdSipConsts, IdSipHeaders, IdSipMessage, IdSipTcpClient,
  IdSipTcpServer, IdSipUdpServer, IdTCPClient, IdTCPServer, IdUDPClient,
  IdUDPServer, SyncObjs, SysUtils;

const
  DefaultTimeout = 5000;

type
  TIdSipNotifyEvent = TNotifyEvent;
  TIdSipResponseEvent = procedure(Sender: TObject; const R: TIdSipResponse) of object;
  TIdSipRequestEvent = procedure(Sender: TObject; const R: TIdSipRequest) of object;

  TIdSipAbstractTransport = class(TObject)
  private
    fHostName:   String;
    fOnRequest:  TIdSipRequestEvent;
    fOnResponse: TIdSipResponseEvent;

    procedure RewriteOwnVia(Message: TIdSipMessage);
  protected
    procedure DoOnRequest(Sender: TObject; const R: TIdSipRequest);
    procedure DoOnResponse(Sender: TObject; const R: TIdSipResponse);
    procedure ExtractHostAndPort(const URL: String; var Host: String; var Port: Cardinal);
    function  GetPort: Cardinal; virtual; abstract;
    procedure SetPort(const Value: Cardinal); virtual; abstract;
  public
    constructor Create(const Port: Cardinal = IdPORT_SIP); virtual;

    function GetTransportType: TIdSipTransportType; virtual; abstract;
    // This should raise an appropriate exception if a transport or suchlike
    // error occurs.
    procedure SendRequest(const R: TIdSipRequest;
                          const Timeout: Cardinal = DefaultTimeout); virtual;
    procedure SendResponse(const R: TIdSipResponse); virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;

    property OnRequest:  TIdSipRequestEvent  read fOnRequest write fOnRequest;
    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
    property HostName:   String              read fHostName write fHostName;
    property Port:       Cardinal            read GetPort write SetPort;
  end;

  TIdSipTransportClass = class of TIdSipAbstractTransport;

  TIdSipTCPTransport = class(TIdSipAbstractTransport)
  private
    Clients: TObjectList;
    Lock:    TCriticalSection;
    Server:  TIdSipTcpServer;

    function  AddClient: TIdSipTcpClient;
    procedure DoOnClientFinished(Sender: TObject);
    procedure DoOnTcpRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure DoOnTcpResponse(Sender: TObject; const Response: TIdSipResponse);
    procedure RemoveClient(Client: TIdSipTcpClient);
  protected
    function  GetPort: Cardinal; override;
    procedure SetPort(const Value: Cardinal); override;
  public
    constructor Create(const Port: Cardinal = IdPORT_SIP); override;
    destructor  Destroy; override;

    function  GetTransportType: TIdSipTransportType; override;
    procedure SendRequest(const R: TIdSipRequest;
                          const Timeout: Cardinal = DefaultTimeout); override;
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

    function  GetTransportType: TIdSipTransportType; override;
    procedure SendRequest(const R: TIdSipRequest;
                          const Timeout: Cardinal = DefaultTimeout); override;
    procedure SendResponse(const R: TIdSipResponse); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TIdSipMockTransport = class(TIdSipAbstractTransport)
  private
    fACKCount:          Cardinal;
    fLastACK:           TIdSipRequest;
    fLastRequest:       TIdSipRequest;
    fLastResponse:      TIdSipResponse;
    fFailWith:          ExceptClass;
    fSentRequestCount:  Cardinal;
    fSentResponseCount: Cardinal;
    fTransportType:     TIdSipTransportType;
  public
    constructor Create(const Port: Cardinal = IdPORT_SIP); override;
    destructor  Destroy; override;

    procedure FireOnRequest(const R: TIdSipRequest);
    procedure FireOnResponse(const R: TIdSipResponse);
    function  GetTransportType: TIdSipTransportType; override;
    procedure RaiseException(const E: ExceptClass);
    procedure ResetACKCount;
    procedure ResetSentRequestCount;
    procedure ResetSentResponseCount;
    procedure SendRequest(const R: TIdSipRequest;
                          const Timeout: Cardinal = DefaultTimeout); override;
    procedure SendResponse(const R: TIdSipResponse); override;
    procedure Start; override;
    procedure Stop; override;

    property ACKCount:          Cardinal            read fACKCount;
    property FailWith:          ExceptClass         read fFailWith write fFailWith;
    property LastACK:           TIdSipRequest       read fLastACK;
    property LastRequest:       TIdSipRequest       read fLastRequest;
    property LastResponse:      TIdSipResponse      read fLastResponse;
    property SentRequestCount:  Cardinal            read fSentRequestCount;
    property SentResponseCount: Cardinal            read fSentResponseCount;
    property TransportType:     TIdSipTransportType read fTransportType write fTransportType;
  end;

implementation

uses
  IdURI;

//******************************************************************************
//* TIdSipAbstractTransport                                                    *
//******************************************************************************
//* TIdSipAbstractTransport Public methods *************************************

constructor TIdSipAbstractTransport.Create(const Port: Cardinal = IdPORT_SIP);
begin
  inherited Create;
end;

procedure TIdSipAbstractTransport.SendRequest(const R: TIdSipRequest;
                                              const Timeout: Cardinal = DefaultTimeout);
begin
  Self.RewriteOwnVia(R);
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

//* TIdSipAbstractTransport Private methods ************************************

procedure TIdSipAbstractTransport.RewriteOwnVia(Message: TIdSipMessage);
begin
  Message.LastHop.Transport := Self.GetTransportType;
  Message.LastHop.SentBy    := Self.HostName;
end;

//******************************************************************************
//* TIdSipTcpTransport                                                         *
//******************************************************************************
//* TIdSipTcpTransport Public methods ******************************************

constructor TIdSipTcpTransport.Create(const Port: Cardinal = IdPORT_SIP);
begin
  inherited Create(Port);

  Self.Clients := TObjectList.Create(true);
  Self.Lock    := TCriticalSection.Create;
  Self.Server  := TIdSipTcpServer.Create(nil);
  Self.SetPort(Port);
  Self.Server.OnRequest  := Self.DoOnTcpRequest;
  Self.Server.OnResponse := Self.DoOnTcpResponse;
end;

destructor TIdSipTcpTransport.Destroy;
begin
  Self.Server.Free;
  Self.Lock.Free;
  Self.Clients.Free;

  inherited Destroy;
end;

function TIdSipTcpTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttTCP;
end;

procedure TIdSipTcpTransport.SendRequest(const R: TIdSipRequest;
                                         const Timeout: Cardinal = DefaultTimeout);
var
  Client: TIdSipTcpClient;
  Host:   String;
  Port:   Cardinal;
begin
  inherited SendRequest(R, Timeout);

  Client := Self.AddClient;
  try
    Self.ExtractHostAndPort(R.RequestUri, Host, Port);
    Client.Host    := Host;
    Client.Port    := Port;
    Client.Timeout := Timeout;

    Client.Connect(Timeout);
    Client.Send(R);
  finally
    Self.RemoveClient(Client);
  end;
end;

procedure TIdSipTcpTransport.SendResponse(const R: TIdSipResponse);
var
  Client: TIdTcpClient;
begin
  Client := TIdTcpClient.Create(nil);
  try
    Client.Host := R.LastHop.SentBy;
    Client.Port := R.LastHop.Port;

    Client.Connect(DefaultTimeout);
    try
      Client.Write(R.AsString);
    finally
      Client.Disconnect;
    end;
  finally
    Client.Free;
  end;
end;

procedure TIdSipTcpTransport.Start;
begin
  Self.Server.Active := true;
end;

procedure TIdSipTcpTransport.Stop;
begin
  Self.Server.Active := false;
end;

//* TIdSipTcpTransport Protected methods ***************************************

function TIdSipTcpTransport.GetPort: Cardinal;
begin
  Result := Self.Server.DefaultPort;
end;

procedure TIdSipTcpTransport.SetPort(const Value: Cardinal);
begin
  Self.Server.DefaultPort := Value;
end;

//* TIdSipTcpTransport Private methods *****************************************

function TIdSipTcpTransport.AddClient: TIdSipTcpClient;
begin
  Result := TIdSipTcpClient.Create(nil);
  Self.Clients.Add(Result);

  Result.OnFinished := Self.DoOnClientFinished;
  Result.OnResponse := Self.DoOnTcpResponse;
end;

procedure TIdSipTcpTransport.DoOnClientFinished(Sender: TObject);
begin
  Self.RemoveClient(Sender as TIdSipTcpClient);
end;

procedure TIdSipTcpTransport.DoOnTcpRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.DoOnRequest(Sender, Request);
end;

procedure TIdSipTcpTransport.DoOnTcpResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Sender, Response);
end;

procedure TIdSipTcpTransport.RemoveClient(Client: TIdSipTcpClient);
begin
  Self.Lock.Acquire;
  try
    Self.Clients.Remove(Client);
  finally
    Self.Lock.Release;
  end;
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

function TIdSipUdpTransport.GetTransportType: TIdSipTransportType;
begin
  Result := sttUDP;
end;

procedure TIdSipUdpTransport.SendRequest(const R: TIdSipRequest;
                                         const Timeout: Cardinal = DefaultTimeout);
var
  Client:   TIdUdpClient;
  Host:     String;
  Port:     Cardinal;
  P:        TIdSipParser;
  Response: TIdSipResponse;
  Reply:    String;
begin
  inherited SendRequest(R, Timeout);

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
  Self.fLastRequest := TIdSipRequest.Create;
  Self.fLastResponse := TIdSipResponse.Create;
end;

destructor TIdSipMockTransport.Destroy;
begin
  Self.LastResponse.Free;
  Self.LastRequest.Free;
  Self.LastACK.Free;

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

function TIdSipMockTransport.GetTransportType: TIdSipTransportType;
begin
  Result := Self.TransportType;
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

procedure TIdSipMockTransport.SendRequest(const R: TIdSipRequest;
                                          const Timeout: Cardinal = DefaultTimeout);
begin
  inherited SendRequest(R, Timeout);

  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendRequest');

  if R.IsAck then begin
    Self.LastACK.Assign(R);
    Inc(Self.fACKCount)
  end
  else
    Inc(Self.fSentRequestCount);

  Self.LastRequest.Assign(R);

  Self.DoOnRequest(Self, R);
end;

procedure TIdSipMockTransport.SendResponse(const R: TIdSipResponse);
begin
  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendResponse');

  Self.LastResponse.Assign(R);

  Inc(Self.fSentResponseCount);

  Self.DoOnResponse(Self, R);
end;

procedure TIdSipMockTransport.Start;
begin
end;

procedure TIdSipMockTransport.Stop;
begin
end;

end.
