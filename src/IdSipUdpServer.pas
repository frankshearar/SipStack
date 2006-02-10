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
  IdSocketHandle, IdUDPServer, IdTimerQueue, SysUtils;

type
  TIdSipReceiveUDPMessageWait = class;
  TIdSipUdpServer = class(TIdUDPServer)
  private
    fTimer:   TIdTimerQueue;
    Notifier: TIdSipServerNotifier;

    procedure DoOnException(Sender: TObject);
    procedure DoOnReceiveMessage(Sender: TObject);
  protected
    procedure DoUDPRead(AData: TStream; ABinding: TIdSocketHandle); override;
    procedure NotifyListenersOfResponse(Response: TIdSipResponse;
                                        ReceivedFrom: TIdSipConnectionBindings); overload; virtual;
    procedure NotifyOfException(E: Exception);
    procedure ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                           Binding: TIdSocketHandle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddMessageListener(const Listener: IIdSipMessageListener);
    procedure RemoveMessageListener(const Listener: IIdSipMessageListener);

    property Timer: TIdTimerQueue read fTimer write fTimer;
  end;

  // I represent a (possibly) deferred receipt of a message.
  // Give me a COPY of a binding in ReceivedFrom, and I'll free it.
  TIdSipReceiveUDPMessageWait = class(TIdSipMessageNotifyEventWait)
  private
    fReceivedFrom: TIdSipConnectionBindings;
  public
    destructor Destroy; override;

    property ReceivedFrom: TIdSipConnectionBindings read fReceivedFrom write fReceivedFrom;
  end;

implementation

uses
  IdUDPBase, IdSipTransport;

//******************************************************************************
//* TIdSipUdpServer                                                            *
//******************************************************************************
//* TIdSipUdpServer Public methods *********************************************

constructor TIdSipUdpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.DefaultPort   := TIdSipTransportRegistry.DefaultPortFor(UdpTransport);
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
  Msg: TIdSipMessage;
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

  try
    Msg := TIdSipMessage.ReadMessageFrom(AData);
    try
      Msg.ReadBody(AData);

      Self.ReceiveMessageInTimerContext(Msg, ABinding);
    finally
      Msg.Free;
    end;
  except
    on E: Exception do begin
      Self.NotifyOfException(E);
    end;
  end;
end;

procedure TIdSipUdpServer.NotifyListenersOfResponse(Response: TIdSipResponse;
                                                    ReceivedFrom: TIdSipConnectionBindings);
begin
  Self.Notifier.NotifyListenersOfResponse(Response, ReceivedFrom);
end;

procedure TIdSipUdpServer.NotifyOfException(E: Exception);
var
  Ex: TIdSipExceptionWait;
begin
  Ex := TIdSipExceptionWait.Create;
  Ex.Event := Self.DoOnException;
  Ex.ExceptionType := ExceptClass(E.ClassType);
  Ex.Reason := E.Message;
  Self.Timer.AddEvent(TriggerImmediately, Ex);
end;

procedure TIdSipUdpServer.ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                                       Binding: TIdSocketHandle);
var
  Wait: TIdSipReceiveUDPMessageWait;
begin
  Wait := TIdSipReceiveUDPMessageWait.Create;
  Wait.Event        := Self.DoOnReceiveMessage;
  Wait.Message      := Msg.Copy;

  Wait.ReceivedFrom := TIdSipConnectionBindings.Create;
  Wait.ReceivedFrom.LocalIP   := Binding.IP;
  Wait.ReceivedFrom.LocalPort := Binding.Port;
  Wait.ReceivedFrom.PeerIP    := Binding.PeerIP;
  Wait.ReceivedFrom.PeerPort  := Binding.PeerPort;

  Self.Timer.AddEvent(TriggerImmediately, Wait);
end;

//* TIdSipUdpServer Private methods ********************************************

procedure TIdSipUdpServer.DoOnException(Sender: TObject);
var
  FakeException: Exception;
  Wait:          TIdSipExceptionWait;
begin
  Wait := Sender as TIdSipExceptionWait;

  FakeException := Wait.ExceptionType.Create(Wait.ExceptionMsg);
  try
    Self.Notifier.NotifyListenersOfException(FakeException,
                                             Wait.Reason);
  finally
    FakeException.Free;
  end;
end;

procedure TIdSipUdpServer.DoOnReceiveMessage(Sender: TObject);
var
  Wait: TIdSipReceiveUDPMessageWait;
begin
  Wait := Sender as TIdSipReceiveUDPMessageWait;

  if Wait.Message.IsRequest then
    Self.Notifier.NotifyListenersOfRequest(Wait.Message as TIdSipRequest,
                                           Wait.ReceivedFrom)
  else
    Self.Notifier.NotifyListenersOfResponse(Wait.Message as TIdSipResponse,
                                            Wait.ReceivedFrom);
end;

//******************************************************************************
//* TIdSipReceiveUDPMessageWait                                                *
//******************************************************************************
//* TIdSipReceiveUDPMessageWait Public methods *********************************

destructor TIdSipReceiveUDPMessageWait.Destroy;
begin
  Self.ReceivedFrom.Free;

  inherited Destroy;
end;

end.
