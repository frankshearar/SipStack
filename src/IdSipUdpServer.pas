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
  Classes, IdNotification, IdSipConsts, IdSipMessage, IdSipServerNotifier,
  IdSocketHandle, IdUDPServer, SysUtils;

type
  TIdSipUdpServer = class(TIdUDPServer)
  private
    Notifier: TIdSipServerNotifier;

  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
    procedure NotifyListenersOfResponse(Response: TIdSipResponse;
                                        ReceivedFrom: TIdSipConnectionBindings); overload; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddMessageListener(const Listener: IIdSipMessageListener);
    procedure RemoveMessageListener(const Listener: IIdSipMessageListener);
  end;

implementation

uses
  IdUDPBase;

//******************************************************************************
//* TIdSipUdpServer                                                            *
//******************************************************************************
//* TIdSipUdpServer Public methods *********************************************

constructor TIdSipUdpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.DefaultPort   := IdPORT_SIP;
  Self.Notifier      := TIdSipServerNotifier.Create;
  Self.ThreadedEvent := true;
end;

destructor TIdSipUdpServer.Destroy;
begin
  Self.Notifier.Free;

  inherited Destroy;
end;

procedure TIdSipUdpServer.AddMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Notifier.AddMessageListener(Listener);
end;

procedure TIdSipUdpServer.RemoveMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Notifier.RemoveMessageListener(Listener);
end;

//* TIdSipUdpServer Protected methods ******************************************

procedure TIdSipUdpServer.DoUDPRead(AData: TStream; ABinding: TIdSocketHandle);
var
  Msg:          TIdSipMessage;
  ReceivedFrom: TIdSipConnectionBindings;
begin
  // Note that if AData contains a fragment of a message we don't care to
  // reassemble the packet. RFC 3261 section 18.3 tells us:

  //   If the transport packet ends before the end of the
  //   message body, this is considered an error.  If the message is a
  //   response, it MUST be discarded.  If the message is a request, the
  //   element SHOULD generate a 400 (Bad Request) response.  If the message
  //   has no Content-Length header field, the message body is assumed to
  //   end at the end of the transport packet.

  inherited DoUDPRead(AData, ABinding);

  ReceivedFrom.LocalIP   := ABinding.IP;
  ReceivedFrom.LocalPort := ABinding.Port;
  ReceivedFrom.PeerIP    := ABinding.PeerIP;
  ReceivedFrom.PeerPort  := ABinding.PeerPort;

  try
    Msg := TIdSipMessage.ReadMessageFrom(AData);
    try
      Msg.ReadBody(AData);

      if Msg.IsRequest then
        Self.Notifier.NotifyListenersOfRequest(Msg as TIdSipRequest,
                                               ReceivedFrom)
      else
        Self.Notifier.NotifyListenersOfResponse(Msg as TIdSipResponse,
                                                ReceivedFrom);
    finally
      Msg.Free;
    end;
  except
    on E: Exception do begin
      Self.Notifier.NotifyListenersOfException(E, E.Message);
    end;
  end;
end;

procedure TIdSipUdpServer.NotifyListenersOfResponse(Response: TIdSipResponse;
                                                    ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.Notifier.NotifyListenersOfResponse(Response, ReceivedFrom);
end;

end.
