unit IdSipUdpServer;

interface

uses
  Classes, IdSipConsts, IdSipMessage, IdSocketHandle, IdUDPServer;

type
  TPeerInfo = record
    PeerIP:   String;
    PeerPort: Integer;
  end;

  TIdSipRequestEvent = procedure(Sender: TObject; const Request: TIdSipRequest) of object;
  TIdSipResponseEvent = procedure(Sender: TObject; const Response: TIdSipResponse) of object;

  TIdSipUdpServer = class(TIdUDPServer)
  private
    fOnRequest:  TIdSipRequestEvent;
    fOnResponse: TIdSipResponseEvent;
    Parser:      TIdSipParser;
    procedure DoOnRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure DoOnResponse(Sender: TObject; const Response: TIdSipResponse);

    procedure SendBadRequestResponse(PeerInfo: TPeerInfo;
                               const Reason: String;
                                     Parser: TIdSipParser);
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property OnRequest:  TIdSipRequestEvent  read fOnRequest write fOnRequest;
    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
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
  Msg:            TIdSipMessage;
  PeerInfo:       TPeerInfo;
begin
  inherited DoUDPRead(AData, ABinding);

  PeerInfo.PeerIP   := ABinding.PeerIP;
  PeerInfo.PeerPort := ABinding.PeerPort;

  Self.Parser.Source := AData;

  try
    Msg := Self.Parser.ParseAndMakeMessage;
    try
      RemainingBytes := AData.Size - AData.Position;
      if Msg.HasHeader(ContentLengthHeaderFull) and
        (RemainingBytes <> Msg.ContentLength) then
        raise EBadRequest.Create(Format(UnexpectedMessageLength, [RemainingBytes, Msg.ContentLength]));

      Msg.ReadBody(Self.Parser.Source);

      if TIdSipParser.IsFQDN(Msg.Path.LastHop.SentBy)
        or (Msg.Path.LastHop.SentBy <> ABinding.IP) then
        Msg.Path.LastHop.Received := ABinding.IP;

      if (Msg is TIdSipRequest) then
        Self.DoOnRequest(ABinding, Msg as TIdSipRequest)
      else
        Self.DoOnResponse(ABinding, Msg as TIdSipResponse);
    finally
      Msg.Free;
    end;
  except
    on E: EBadRequest do begin
      Self.SendBadRequestResponse(PeerInfo, E.Message, Parser);
    end;
    on E: EBadResponse do begin
      // drop it on the floor
    end;
  end;
end;

//* TestFoo Private methods *****************************************************

procedure TIdSipUdpServer.DoOnRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  if Assigned(Self.OnRequest) then
    Self.OnRequest(Sender, Request);
end;

procedure TIdSipUdpServer.DoOnResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Sender, Response);
end;

procedure TIdSipUdpServer.SendBadRequestResponse(PeerInfo: TPeerInfo;
                                           const Reason: String;
                                                 Parser: TIdSipParser);
var
  Res: TIdSipResponse;
begin
  Res := TIdSipResponse.Create;
  try
    // We really can't do much more than this. The message was unparseable,
    // so what else can we do?
    Res.StatusCode := SIPBadRequest;
    Res.StatusText := Reason;
    Res.SipVersion := SipVersion;

    Self.Send(PeerInfo.PeerIP, PeerInfo.PeerPort, Res.AsString);
  finally
    Res.Free;
  end;
end;

end.
