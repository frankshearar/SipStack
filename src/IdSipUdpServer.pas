unit IdSipUdpServer;

interface

uses
  Classes, IdSipConsts, IdSipMessage, IdSocketHandle, IdUDPServer;

type
  TPeerInfo = record
    PeerIP: string;
    PeerPort: Integer;
  end;

  TIdSipRequestEvent = procedure(Sender: TObject; const Request: TIdSipRequest) of object;
  TIdSipResponseEvent = procedure(Sender: TObject; const Response: TIdSipResponse) of object;

  TIdSipUdpServer = class(TIdUDPServer)
  private
    fOnRequest:  TIdSipRequestEvent;
    Parser:      TIdSipParser;
    procedure DoOnRequest(const Request: TIdSipRequest);

    procedure SendBadRequestResponse(PeerInfo: TPeerInfo;
                                     const Reason: String;
                                     Parser: TIdSipParser);
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property OnRequest: TIdSipRequestEvent read fOnRequest write fOnRequest;
  end;

implementation

uses
  SysUtils;

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
  RemainingBytes: Cardinal;
  Req:            TIdSipRequest;
  PeerInfo:       TPeerInfo;
begin
  inherited DoUDPRead(AData, ABinding);

  PeerInfo.PeerIP   := ABinding.PeerIP;
  PeerInfo.PeerPort := ABinding.PeerPort;

  Self.Parser.Source := AData;

  // what happens if the message is malformed?!
  try
    Req := Self.Parser.ParseAndMakeRequest;
    try
      RemainingBytes := AData.Size - AData.Position;
      // Yes, typecasting an Integer to a Cardinal is not clever. However, the
      // Abs() ensures that Length(Msg.Body) >= 0 and so we can rest in peace
      // knowing we shan't cause an overflow. And we just the compiler warning
      // about comparing signed and unsigned types.
      if Req.HasHeader(ContentLengthHeaderFull) and
        (RemainingBytes <> Req.ContentLength) then
        raise EBadRequest.Create(Format(UnexpectedMessageLength, [RemainingBytes, Req.ContentLength]));

      Req.ReadBody(Self.Parser.Source);

      if TIdSipParser.IsFQDN(Req.Path.LastHop.SentBy)
        or (Req.Path.LastHop.SentBy <> ABinding.IP) then
        Req.Path.LastHop.Received := ABinding.IP;

      Self.DoOnRequest(Req);
    finally
      Req.Free;
    end;
  except
    on E: EBadRequest do begin
      Self.SendBadRequestResponse(PeerInfo, E.Message, Parser);
    end;
  end;
end;

//* TestFoo Private methods *****************************************************

procedure TIdSipUdpServer.DoOnRequest(const Request: TIdSipRequest);
begin
  if Assigned(Self.OnRequest) then
    Self.OnRequest(Self, Request);
end;

procedure TIdSipUdpServer.SendBadRequestResponse(PeerInfo: TPeerInfo;
                                                 const Reason: String;
                                                 Parser: TIdSipParser);
var
  Msg: TIdSipResponse;
begin
  Msg := TIdSipResponse.Create;
  try
    Msg.StatusCode := SIPBadRequest;
    Msg.StatusText := Reason;
    Msg.SipVersion := SipVersion;

    Self.Send(PeerInfo.PeerIP, PeerInfo.PeerPort, Msg.AsString);
  finally
    Msg.Free;
  end;
end;

end.
