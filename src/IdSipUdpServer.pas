unit IdSipUdpServer;

interface

uses
  Classes, IdSipConsts, IdSipMessage, IdSipTcpClient, IdSipTcpServer,
  IdSocketHandle, IdUDPServer, SyncObjs;

type
  TIdSipUdpServer = class(TIdUDPServer)
  private
    ListenerLock: TCriticalSection;
    Listeners:    TList;

    procedure DoOnParserError(const RawMessage, Reason: String);
    procedure NotifyListeners(const Request: TIdSipRequest;
                              const ReceivedFrom: TIdSipConnectionBindings); overload;
    procedure NotifyListenersOfMalformedMessage(const Msg: String;
                                                const Reason: String);
    procedure ReturnBadRequest(Binding: TIdSocketHandle;
                               const Reason: String;
                               Parser: TIdSipParser);
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
    procedure NotifyListeners(const Response: TIdSipResponse;
                              const ReceivedFrom: TIdSipConnectionBindings); overload; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddMessageListener(const Listener: IIdSipMessageListener);
    procedure RemoveMessageListener(const Listener: IIdSipMessageListener);
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
  Self.ListenerLock  := TCriticalSection.Create;
  Self.Listeners     := TList.Create;
  Self.ThreadedEvent := true;
end;

destructor TIdSipUdpServer.Destroy;
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Free;
  finally
    Self.ListenerLock.Release;
  end;
  Self.ListenerLock.Free;

  inherited Destroy;
end;

procedure TIdSipUdpServer.AddMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Add(Pointer(Listener));
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdSipUdpServer.RemoveMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.ListenerLock.Acquire;
  try
    Self.Listeners.Remove(Pointer(Listener));
  finally
    Self.ListenerLock.Release;
  end;
end;

//* TIdSipUdpServer Protected methods ******************************************

procedure TIdSipUdpServer.DoUDPRead(AData: TStream; ABinding: TIdSocketHandle);
var
  RemainingBytes: Cardinal;
  Msg:            TIdSipMessage;
  Parser:         TIdSipParser;
  RawMsg:         String;
  Reason:         String;
  ReceivedFrom:   TIdSipConnectionBindings;
begin
  inherited DoUDPRead(AData, ABinding);

  ReceivedFrom.LocalIP   := ABinding.IP;
  ReceivedFrom.LocalPort := ABinding.Port;
  ReceivedFrom.PeerIP    := ABinding.PeerIP;
  ReceivedFrom.PeerPort  := ABinding.PeerPort;

  Parser := TIdSipParser.Create;
  try
    Parser.Source := AData;
    Parser.OnParserError := Self.DoOnParserError;

    try
      Msg := Parser.ParseAndMakeMessage;
      try
        RemainingBytes := AData.Size - AData.Position;
        if Msg.HasHeader(ContentLengthHeaderFull) and
          (RemainingBytes <> Msg.ContentLength) then begin

          Reason := Format(UnexpectedMessageLength,
                           [RemainingBytes, Msg.ContentLength]);
          RawMsg := StreamToStr(AData);
          Self.NotifyListenersOfMalformedMessage(RawMsg, Reason);
          raise EBadRequest.Create(Reason, RawMsg);
        end;

        Msg.ReadBody(Parser.Source);

        if Msg.IsRequest then
          Self.NotifyListeners(Msg as TIdSipRequest, ReceivedFrom)
        else
          Self.NotifyListeners(Msg as TIdSipResponse, ReceivedFrom);
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
  finally
    Parser.Free;
  end;
end;

procedure TIdSipUdpServer.NotifyListeners(const Response: TIdSipResponse;
                                          const ReceivedFrom: TIdSipConnectionBindings);
var
  I: Integer;
begin
  Self.ListenerLock.Acquire;
  try
    for I := 0 to Self.Listeners.Count - 1 do
      IIdSipMessageListener(Self.Listeners[I]).OnReceiveResponse(Response,
                                                                 ReceivedFrom);
  finally
    Self.ListenerLock.Release;
  end;
end;

//* TIdSipUdpServer Private methods ********************************************

procedure TIdSipUdpServer.DoOnParserError(const RawMessage, Reason: String);
begin
  Self.NotifyListenersOfMalformedMessage(RawMessage, Reason);
end;

procedure TIdSipUdpServer.NotifyListeners(const Request: TIdSipRequest;
                                          const ReceivedFrom: TIdSipConnectionBindings);
var
  I: Integer;
begin
  Self.ListenerLock.Acquire;
  try
    for I := 0 to Self.Listeners.Count - 1 do
      IIdSipMessageListener(Self.Listeners[I]).OnReceiveRequest(Request,
                                                                ReceivedFrom);
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdSipUdpServer.NotifyListenersOfMalformedMessage(const Msg: String;
                                                            const Reason: String);
var
  I: Integer;
begin
  Self.ListenerLock.Acquire;
  try
    for I := 0 to Self.Listeners.Count - 1 do
      IIdSipMessageListener(Self.Listeners[I]).OnMalformedMessage(Msg, Reason);
  finally
    Self.ListenerLock.Release;
  end;
end;

procedure TIdSipUdpServer.ReturnBadRequest(Binding: TIdSocketHandle;
                                           const Reason: String;
                                           Parser: TIdSipParser);
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
    OwnVia.SipVersion := SIPVersion;
    OwnVia.Transport  := sttUDP;

    Self.Send(Binding.PeerIP, Binding.PeerPort, Res.AsString);
  finally
    Res.Free;
  end;
end;

end.
