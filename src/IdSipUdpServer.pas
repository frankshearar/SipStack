unit IdSipUdpServer;

interface

uses
  Classes, IdSipMessage, IdSipParser, IdSocketHandle, IdUDPServer, IdSipTcpServer;

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
    fOnResponse: TIdSipResponseEvent;
    Parser:      TIdSipParser;
    procedure DoOnRequest(const Request: TIdSipRequest);
    procedure DoOnResponse(const Response: TIdSipResponse);

    procedure SendBadRequestResponse(PeerInfo: TPeerInfo;
                                     const Reason: String;
                                     Parser: TIdSipParser);
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

  // what happens if the message is malformed?!
  try
    Msg := Self.Parser.ParseAndMakeMessage;
    try
      RemainingBytes := AData.Size - AData.Position;
      // Yes, typecasting an Integer to a Cardinal is not clever. However, the
      // Abs() ensures that Length(Msg.Body) >= 0 and so we can rest in peace
      // knowing we shan't cause an overflow. And we just the compiler warning
      // about comparing signed and unsigned types.
      if Msg.HasHeader(ContentLengthHeaderFull) and
        (RemainingBytes <> Msg.ContentLength) then
        raise EBadRequest.Create(Format(UnexpectedMessageLength, [RemainingBytes, Msg.ContentLength]));

      Msg.ReadBody(Self.Parser.Source);

      if (Msg is TIdSipRequest) then begin
        if TIdSipParser.IsFQDN(Msg.Path.LastHop.SentBy)
          or (Msg.Path.LastHop.SentBy <> ABinding.IP) then
          Msg.Path.LastHop.Received := ABinding.IP;

        Self.DoOnRequest(Msg as TIdSipRequest);
      end
      else
        Self.DoOnResponse(Msg as TIdSipResponse);
    finally
      Msg.Free;
    end;
  except
    on E: EBadRequest do begin
      Self.SendBadRequestResponse(PeerInfo, E.Message, Parser);
    end;
    on E: EBadResponse do;
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

procedure TIdSipUdpServer.SendBadRequestResponse(PeerInfo: TPeerInfo;
                                                 const Reason: String;
                                                 Parser: TIdSipParser);
var
  Msg: TIdSipMessage;
begin
  Msg := Parser.MakeBadRequestResponse(Reason);
  try
    Self.Send(PeerInfo.PeerIP, PeerInfo.PeerPort, Msg.AsString);
  finally
    Msg.Free;
  end;
end;

end.
