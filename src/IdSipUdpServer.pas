unit IdSipUdpServer;

interface

uses
  Classes, IdSipConsts, IdSipMessage, IdSocketHandle, IdUDPServer;

type
  TIdSipIPTarget = record
    IP:   String;
    Port: Integer;
  end;

  TIdSipRequestEvent  = procedure(Sender: TObject; const Request: TIdSipRequest) of object;
  TIdSipResponseEvent = procedure(Sender: TObject; const Response: TIdSipResponse) of object;

  TIdSipUdpServer = class(TIdUDPServer, IIdSipMessageVisitor)
  private
    fOnRequest:  TIdSipRequestEvent;
    fOnResponse: TIdSipResponseEvent;
    Parser:      TIdSipParser;
    procedure DoOnRequest(const Request: TIdSipRequest);
    procedure DoOnResponse(const Response: TIdSipResponse);

    procedure ReturnBadRequest(Binding: TIdSocketHandle;
                         const Reason:  String;
                               Parser:  TIdSipParser);
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure VisitRequest(const Request: TIdSipRequest);
    procedure VisitResponse(const Response: TIdSipResponse);
  published
    property OnRequest:  TIdSipRequestEvent  read fOnRequest write fOnRequest;
    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
  end;

implementation

uses
  IdSipHeaders, IdUDPBase, SysUtils;

//*******************************************************************************
//* TIdSipUdpServer                                                             *
//*******************************************************************************
//* TIdSipUdpServer Public methods **********************************************

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

procedure TIdSipUdpServer.VisitRequest(const Request: TIdSipRequest);
begin
  Self.DoOnRequest(Request);
end;

procedure TIdSipUdpServer.VisitResponse(const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Response);
end;

//* TIdSipUdpServer Protected methods ******************************************

procedure TIdSipUdpServer.DoUDPRead(AData: TStream; ABinding: TIdSocketHandle);
var
  RemainingBytes: Cardinal;
  Msg:            TIdSipMessage;
begin
  inherited DoUDPRead(AData, ABinding);

  Self.Parser.Source := AData;

  try
    Msg := Self.Parser.ParseAndMakeMessage;
    try
      RemainingBytes := AData.Size - AData.Position;
      if Msg.HasHeader(ContentLengthHeaderFull) and
        (RemainingBytes <> Msg.ContentLength) then
        raise EBadRequest.Create(Format(UnexpectedMessageLength, [RemainingBytes, Msg.ContentLength]));

      Msg.ReadBody(Self.Parser.Source);

      if TIdSipParser.IsFQDN(Msg.LastHop.SentBy)
        or (Msg.LastHop.SentBy <> ABinding.IP) then
        Msg.LastHop.Received := ABinding.IP;

      Msg.Accept(Self);
    finally
      Msg.Free;
    end;
  except
    on E: EBadRequest do begin
      Self.ReturnBadRequest(ABinding, E.Message, Parser);
    end;
    on E: EBadResponse do begin
      // drop it on the floor
    end;
  end;
end;

//* TIdSipUdpServer Private methods ********************************************

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

procedure TIdSipUdpServer.ReturnBadRequest(Binding: TIdSocketHandle;
                                     const Reason:  String;
                                           Parser:  TIdSipParser);
var
  OwnVia: TIdSipViaHeader;
  Res:    TIdSipResponse;
begin
  Res := TIdSipResponse.Create;
  try
    // We really can't do much more than this. The message was unparseable,
    // so what else can we do?
    Res.StatusCode := SIPBadRequest;
    Res.StatusText := Reason;
    Res.SipVersion := SipVersion;

    OwnVia := Res.Headers.Add(ViaHeaderFull) as TIdSipViaHeader;
    OwnVia.Port       := Binding.Port;
    OwnVia.SentBy     := Binding.IP;
    OwnVia.SipVersion := 'SIP/2.0';
    OwnVia.Transport  := sttUDP;

    Self.Send(Binding.PeerIP, Binding.PeerPort, Res.AsString);
  finally
    Res.Free;
  end;
end;

end.
