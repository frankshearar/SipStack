unit IdSipUdpServer;

interface

uses
  Classes, IdSipParser, IdSocketHandle, IdUDPServer, IdSipTcpServer;

type
  TIdSipRequestEvent = procedure(Sender: TObject; const Request: TIdSipRequest) of object;
  TIdSipResponseEvent = procedure(Sender: TObject; const Response: TIdSipResponse) of object;

  TIdSipUdpServer = class(TIdUDPServer)
  private
    fOnRequest:  TIdSipRequestEvent;
    fOnResponse: TIdSipResponseEvent;
    Parser:      TIdSipParser;
    procedure DoOnRequest(const Request: TIdSipRequest);
    procedure DoOnResponse(const Response: TIdSipResponse);
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property OnRequest:  TIdSipRequestEvent read fOnRequest write fOnRequest;
    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
  end;

implementation

//*******************************************************************************
//* TestFoo                                                                     *
//*******************************************************************************
//* TestFoo Public methods ******************************************************

constructor TIdSipUdpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.DefaultPort   := IdPORT_SIP;
  Self.Parser        := TIdSipParser.Create;
  Self.ThreadedEvent := true;
end;

destructor TIdSipUdpServer.Destroy;
begin
  Self.Parser.Free;

  inherited Destroy;
end;

//* TestFoo Protected methods ***************************************************

procedure TIdSipUdpServer.DoUDPRead(AData: TStream; ABinding: TIdSocketHandle);
var
  Message: TIdSipMessage;
begin
  inherited DoUDPRead(AData, ABinding);

  Self.Parser.Source := AData;

  // what happens if the message is malformed?!
  try
    Message := Self.Parser.ParseAndMakeMessage;
    try
      Message.ReadBody(Self.Parser.Source);
      if (Message is TIdSipRequest) then
        Self.DoOnRequest(Message as TIdSipRequest)
      else
        Self.DoOnResponse(Message as TIdSipResponse);
    finally
      Message.Free;
    end;
  except
    on EBadRequest do begin
//      ABinding.PeerIP
//      ABinding.PeerPort
    end;
  end;
end;

//* TestFoo Private methods *****************************************************

procedure TIdSipUdpServer.DoOnRequest(const Request: TIdSipRequest);
begin
  if Assigned(Self.OnRequest) then
    Self.OnRequest(Self, Request);
end;

procedure TIdSipUdpServer.DoOnResponse(const Response: TIdSipResponse);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, Response);
end;

//* TestFoo Published methods ***************************************************

end.
