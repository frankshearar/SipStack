{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipUdpServer;

interface

uses
  Classes, IdSipConsts, IdSipMessage, IdNotification, IdSipTcpClient,
  IdSipTcpServer, IdSocketHandle, IdUDPServer, SysUtils;

type
  TIdSipUdpServer = class(TIdUDPServer)
  private
    Listeners: TIdNotificationList;

    procedure DoOnParserError(const RawMessage, Reason: String);
    procedure NotifyListeners(const Request: TIdSipRequest;
                              const ReceivedFrom: TIdSipConnectionBindings); overload;
    procedure NotifyListenersOfException(E: Exception);
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
  IdUDPBase;

//*******************************************************************************
//* TIdSipUdpServer                                                             *
//*******************************************************************************
//* TIdSipUdpServer Public methods **********************************************

constructor TIdSipUdpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.DefaultPort   := IdPORT_SIP;
  Self.Listeners     := TIdNotificationList.Create;
  Self.ThreadedEvent := true;
end;

destructor TIdSipUdpServer.Destroy;
begin
  Self.Listeners.Free;

  inherited Destroy;
end;

procedure TIdSipUdpServer.AddMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Listeners.AddListener(Listener);
end;

procedure TIdSipUdpServer.RemoveMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Listeners.RemoveListener(Listener);
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
          (RemainingBytes < Msg.ContentLength) then begin

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
      on E: Exception do begin
        Self.NotifyListenersOfException(E);
      end;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TIdSipUdpServer.NotifyListeners(const Response: TIdSipResponse;
                                          const ReceivedFrom: TIdSipConnectionBindings);
var
  Notification: TIdSipTcpServerReceiveResponseMethod;
begin
  Notification := TIdSipTcpServerReceiveResponseMethod.Create;
  try
    Notification.ReceivedFrom := ReceivedFrom;
    Notification.Response     := Response;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
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
  Notification: TIdSipTcpServerReceiveRequestMethod;
begin
  Notification := TIdSipTcpServerReceiveRequestMethod.Create;
  try
    Notification.ReceivedFrom := ReceivedFrom;
    Notification.Request     := Request;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipUdpServer.NotifyListenersOfException(E: Exception);
var
  Notification: TIdSipTcpServerExceptionMethod;
begin
  Notification := TIdSipTcpServerExceptionMethod.Create;
  try
    Notification.Exception := E;
    Notification.Reason := 'UDP server: ' + E.Message;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipUdpServer.NotifyListenersOfMalformedMessage(const Msg: String;
                                                            const Reason: String);
var
  Notification: TIdSipTcpServerMalformedMessageMethod;
begin
  Notification := TIdSipTcpServerMalformedMessageMethod.Create;
  try
    Notification.Msg := Msg;
    Notification.Reason := Reason;

    Self.Listeners.Notify(Notification);
  finally
    Notification.Free;
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
