unit IdSipTransport;

interface

uses
  Classes, IdSipMessage, IdSipParser, IdSipTcpServer, IdSipUdpServer,
  IdTCPClient, IdTCPServer, IdUDPClient, IdUDPServer, SysUtils;

type
(*
  IIdSipMessageListener = interface
  ['{DFB6D261-16DE-4387-BD92-A01689D1851C}']
    procedure ProcessRequest(const R: TIdSipRequest);
    procedure ProcessResponse(const R: TIdSipResponse);
  end;
*)
  TIdSipNotifyEvent = TNotifyEvent;
  TIdSipResponseEvent = procedure(Sender: TObject; const R: TIdSipResponse) of object;
  TIdSipRequestEvent = procedure(Sender: TObject; const R: TIdSipRequest) of object;

  // todo:
  // * 18.1.1 - if the MTU of a message is too big then the message must be
  //   sent using something like TCP, and the message altered appropriately
  TIdSipAbstractTransport = class(TObject)
  private
    fOnRequest:  TIdSipRequestEvent;
    fOnResponse: TIdSipResponseEvent;
  protected
    procedure DoOnRequest(const R: TIdSipRequest);
    procedure DoOnResponse(const R: TIdSipResponse);
    procedure ExtractHostAndPort(const URL: String; var Host: String; var Port: Cardinal);
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
  end;

  TIdSipTransportClass = class of TIdSipAbstractTransport;

  TIdSipTCPTransport = class(TIdSipAbstractTransport)
  private
    Transport: TIdSipTcpServer;

    procedure DoOnMethod(AThread: TIdPeerThread;
                         AMessage: TIdSipMessage);
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
  IdURI;

//******************************************************************************
//* TIdSipAbstractTransport                                                    *
//******************************************************************************
//* TIdSipAbstractTransport Public methods *************************************

constructor TIdSipAbstractTransport.Create(const Port: Cardinal = IdPORT_SIP);
begin
  inherited Create;
end;

//* TIdSipAbstractTransport Protected methods **********************************

procedure TIdSipAbstractTransport.DoOnRequest(const R: TIdSipRequest);
begin
  if Assigned(Self.OnRequest) then
    Self.OnRequest(Self, R)
end;

procedure TIdSipAbstractTransport.DoOnResponse(const R: TIdSipResponse);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, R)
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
    if (URI.Port = '') then
      Port := IdPORT_SIP
    else
      Port := StrToInt(URI.Port);
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
  Self.Transport.DefaultPort := Port;
  Self.Transport.OnMethod := Self.DoOnMethod;
end;

destructor TIdSipTcpTransport.Destroy;
begin
  Self.Transport.Free;

  inherited Destroy;
end;

procedure TIdSipTcpTransport.SendRequest(const R: TIdSipRequest);
var
  Client: TIdTcpClient;
  Host:   String;
  Port:   Cardinal;
begin
  Client := TIdTcpClient.Create(nil);
  try
    Self.ExtractHostAndPort(R.RequestUri, Host, Port);
    Client.Host := Host;
    Client.Port := Port;

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

//* TIdSipTcpTransport Private methods *****************************************

procedure TIdSipTcpTransport.DoOnMethod(AThread: TIdPeerThread;
                                        AMessage: TIdSipMessage);
begin
  if AMessage is TIdSipRequest then
    Self.OnRequest(Self, AMessage as TIdSipRequest)
  else if AMessage is TIdSipResponse then
    Self.OnResponse(Self, AMessage as TIdSipResponse);
end;

//******************************************************************************
//* TIdSipUdpTransport                                                         *
//******************************************************************************
//* TIdSipUdpTransport Public methods ******************************************

constructor TIdSipUdpTransport.Create(const Port: Cardinal = IdPORT_SIP);
begin
  inherited Create(Port);

  Self.Transport := TIdSipUdpServer.Create(nil);
  Self.Transport.DefaultPort := Port;
  Self.Transport.OnRequest   := Self.DoOnReceiveRequest;
  Self.Transport.OnResponse  := Self.DoOnReceiveResponse;
end;

destructor TIdSipUdpTransport.Destroy;
begin
  Self.Transport.Free;

  inherited Destroy;
end;

procedure TIdSipUdpTransport.SendRequest(const R: TIdSipRequest);
var
  Client: TIdUdpClient;
  Host:   String;
  Port:   Cardinal;
begin
  Client := TIdUdpClient.Create(nil);
  try
    Self.ExtractHostAndPort(R.RequestUri, Host, Port);
    Client.Host := Host;
    Client.Port := Port;

    Client.Send(R.AsString);
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

//* TIdSipTcpTransport Private methods *****************************************

procedure TIdSipUdpTransport.DoOnReceiveRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.DoOnRequest(Request);
end;

procedure TIdSipUdpTransport.DoOnReceiveResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Response);
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
  Self.DoOnRequest(R);
end;

procedure TIdSipMockTransport.FireOnResponse(const R: TIdSipResponse);
begin
  Self.DoOnResponse(R);
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

  Self.DoOnRequest(R);
end;

procedure TIdSipMockTransport.SendResponse(const R: TIdSipResponse);
begin
  if Assigned(Self.FailWith) then
    raise Self.FailWith.Create('TIdSipMockTransport.SendResponse');

  Self.DoOnResponse(R);

  Inc(Self.fSentResponseCount);
end;

procedure TIdSipMockTransport.Start;
begin
end;

procedure TIdSipMockTransport.Stop;
begin
end;

end.
