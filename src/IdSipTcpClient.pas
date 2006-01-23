{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipTcpClient;

interface

uses
  Classes, IdSipMessage, IdSipServerNotifier, IdTCPClient, IdTCPConnection,
  IdTimerQueue, SysUtils;

type
  TIdSipAddConnectionEvent = procedure(Connection: TIdTCPConnection;
                                       Request: TIdSipRequest) of object;
  TIdSipRemoveConnectionEvent = procedure(Connection: TIdTCPConnection) of object;

  // Given a TCP connection, I read messages off the connection and dispatch
  // them to a TimerQueue until I die or something severs the connection.
  TIdSipTcpMessageReader = class(TObject)
  private
    fNotifier:           TIdSipServerNotifier;
    fOnAddConnection:    TIdSipAddConnectionEvent;
    fOnRemoveConnection: TIdSipRemoveConnectionEvent;
    fReadTimeout:        Integer;
    fTimer:              TIdTimerQueue;

    procedure AddConnection(Connection: TIdTCPConnection;
                            Request: TIdSipRequest);
    procedure ReadBodyInto(Connection: TIdTCPConnection;
                           Msg: TIdSipMessage;
                           Dest: TStringStream);
    procedure ReadMessage(Connection: TIdTCPConnection;
                          Dest: TStringStream);
    procedure ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                           Binding: TIdSipConnectionBindings);
    procedure ReturnInternalServerError(Connection: TIdTCPConnection;
                                        const Reason: String);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure NotifyOfException(ExceptionType: ExceptClass;
                                const Reason: String);
    procedure ReadMessages(Connection: TIdTCPConnection);

    property Notifier:           TIdSipServerNotifier        read fNotifier;
    property OnAddConnection:    TIdSipAddConnectionEvent    read fOnAddConnection write fOnAddConnection;
    property OnRemoveConnection: TIdSipRemoveConnectionEvent read fOnRemoveConnection write fOnRemoveConnection;
    property ReadTimeout:        Integer                     read fReadTimeout write fReadTimeout;
    property Timer:              TIdTimerQueue               read fTimer write fTimer;
  end;

  // Note that the Timeout property determines the maximum length of time to
  // wait for the next line of data to arrive.
  // Through my MessageReader, I read messages from the network and feed them to
  // a TimerQueue. That, and I send messages to the network.
  TIdSipTcpClient = class(TIdTCPClient,
                          IIdSipMessageListener)
  private
    fOnRequest:    TIdSipRequestEvent;
    fOnResponse:   TIdSipResponseEvent;
    fTerminated:   Boolean;
    MessageReader: TIdSipTcpMessageReader;

    function  GetTimer: TIdTimerQueue;
    procedure MarkAsTerminated(Sender: TObject);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnMalformedMessage(const Msg: String;
                                 const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               ReceivedFrom: TIdSipConnectionBindings);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                ReceivedFrom: TIdSipConnectionBindings);
    procedure SetTerminated(Value: Boolean);
    procedure SetTimer(Value: TIdTimerQueue);
  protected
    function DefaultTimeout: Cardinal; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddMessageListener(Listener: IIdSipMessageListener);
    procedure ReceiveMessages;
    procedure RemoveMessageListener(Listener: IIdSipMessageListener);
    procedure Send(Msg: TIdSipMessage);

    property OnRequest:  TIdSipRequestEvent  read fOnRequest write fOnRequest;
    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
    property Terminated: Boolean             read fTerminated write SetTerminated;
    property Timer:      TIdTimerQueue       read GetTimer write SetTimer;
  end;

  TIdSipTcpClientClass = class of TIdSipTcpClient;

  TIdSipTlsClient = class(TIdSipTcpClient)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

  // I represent the (possibly) deferred handling of an exception raised in the
  // process of sending or receiving a message.
  TIdSipMessageExceptionWait = class(TIdWait)
  private
    fExceptionMessage: String;
    fExceptionType:    ExceptClass;
    fListeners:        TIdSipServerNotifier;
    fReason:           String;
  public
    procedure Trigger; override;

    property ExceptionType:    ExceptClass          read fExceptionType write fExceptionType;
    property ExceptionMessage: String               read fExceptionMessage write fExceptionMessage;
    property Listeners:        TIdSipServerNotifier read fListeners write fListeners;
    property Reason:           String               read fReason write fReason;
  end;

  // I represent the (possibly) deferred handling of an inbound message.
  TIdSipReceiveMessageWait = class(TIdSipMessageWait)
  private
    fListeners:    TIdSipServerNotifier;
    fReceivedFrom: TIdSipConnectionBindings;
  public
    destructor Destroy; override;

    procedure Trigger; override;

    property Listeners:    TIdSipServerNotifier     read fListeners write fListeners;
    property ReceivedFrom: TIdSipConnectionBindings read fReceivedFrom write fReceivedFrom;
  end;

implementation

uses
  IdException, IdSipTcpServer, IdSSLOpenSSL;

//******************************************************************************
//* TIdSipTcpMessageReader                                                     *
//******************************************************************************
//* TIdSipTcpMessageReader Public methods **************************************

constructor TIdSipTcpMessageReader.Create;
begin
  inherited Create;

  Self.fNotifier := TIdSipServerNotifier.Create;
end;

destructor TIdSipTcpMessageReader.Destroy;
begin
  Self.fNotifier.Free;

  inherited Destroy;
end;

procedure TIdSipTcpMessageReader.NotifyOfException(ExceptionType: ExceptClass;
                                                   const Reason: String);
var
  Wait: TIdSipMessageExceptionWait;
begin
  Wait := TIdSipMessageExceptionWait.Create;
  Wait.ExceptionType := ExceptionType;
  Wait.ExceptionMessage := Reason;
  Wait.Reason           := Reason;

  Self.Timer.AddEvent(TriggerImmediately, Wait);
end;

procedure TIdSipTcpMessageReader.ReadMessages(Connection: TIdTCPConnection);
var
  ConnClosedOrTimedOut: Boolean;
  Msg:                  TIdSipMessage;
  ReceivedFrom:         TIdSipConnectionBindings;
  S:                    TStringStream;
begin
  ConnClosedOrTimedOut := false;

  Connection.ReadTimeout := Self.ReadTimeout;
  while Connection.Connected and not ConnClosedOrTimedOut do begin
    ReceivedFrom := TIdSipConnectionBindings.Create;
    try
      ReceivedFrom.LocalIP   := Connection.Socket.Binding.IP;
      ReceivedFrom.LocalPort := Connection.Socket.Binding.Port;
      ReceivedFrom.PeerIP    := Connection.Socket.Binding.PeerIP;
      ReceivedFrom.PeerPort  := Connection.Socket.Binding.PeerPort;

      S := TStringStream.Create('');
      try
        try
          Self.ReadMessage(Connection, S);
        except
          on EIdReadTimeout do
            ConnClosedOrTimedOut := true;
          on EIdConnClosedGracefully do
            ConnClosedOrTimedOut := true;
          on EIdClosedSocket do
            ConnClosedOrTimedOut := true;
        end;

        if not ConnClosedOrTimedOut then begin
          Msg := TIdSipMessage.ReadMessageFrom(S);
          try
            try
              try
                Self.ReadBodyInto(Connection, Msg, S);
                Msg.ReadBody(S);
              except
                on EIdReadTimeout do
                  ConnClosedOrTimedOut := true;
                on EIdConnClosedGracefully do
                  ConnClosedOrTimedOut := true;
                on EIdClosedSocket do
                  ConnClosedOrTimedOut := true;
              end;

              // If Self.ReadBody closes the connection, we don't want to AddConnection!
              if Msg.IsRequest and not ConnClosedOrTimedOut then
                Self.AddConnection(Connection, Msg as TIdSipRequest);

              Self.ReceiveMessageInTimerContext(Msg, ReceivedFrom);
            except
              on E: Exception do begin
                // This results in returning a 500 Internal Server Error to a response!
                if Connection.Connected then begin
                  Self.ReturnInternalServerError(Connection, E.Message);
                  Connection.DisconnectSocket;
                end;

                Self.NotifyOfException(ExceptClass(E.ClassType),
                                                   E.Message);
              end;
            end;
          finally
            Msg.Free;
          end;
        end;
      finally
        S.Free;
      end;
    finally
      ReceivedFrom.Free;
    end;
  end;
end;

//* TIdSipTcpMessageReader Private methods *************************************

procedure TIdSipTcpMessageReader.AddConnection(Connection: TIdTCPConnection;
                                               Request: TIdSipRequest);
begin
  if Assigned(Self.OnAddConnection) then
    Self.OnAddConnection(Connection, Request);
end;

procedure TIdSipTcpMessageReader.ReadBodyInto(Connection: TIdTCPConnection;
                                              Msg: TIdSipMessage;
                                              Dest: TStringStream);
begin
  Connection.ReadStream(Dest, Msg.ContentLength);

  // Roll back the stream to just before the message body!
  Dest.Seek(-Msg.ContentLength, soFromCurrent);
end;

procedure TIdSipTcpMessageReader.ReadMessage(Connection: TIdTCPConnection;
                                             Dest: TStringStream);
const
  CrLf = #$D#$A;
begin
  // We skip any leading CRLFs, and read up to (and including) the first blank
  // line.
  while (Dest.DataString = '') do
    Connection.Capture(Dest, '');

  // Capture() returns up to the blank line, but eats it: we add it back in
  // manually.
  Dest.Write(CrLf, Length(CrLf));
  Dest.Seek(0, soFromBeginning);
end;

procedure TIdSipTcpMessageReader.ReceiveMessageInTimerContext(Msg: TIdSipMessage;
                                                              Binding: TIdSipConnectionBindings);
var
  RecvWait: TIdSipReceiveMessageWait;
begin
  RecvWait := TIdSipReceiveMessageWait.Create;
  RecvWait.Message      := Msg.Copy;
  RecvWait.Listeners    := Self.Notifier;
  RecvWait.ReceivedFrom := Binding.Copy;

  Self.Timer.AddEvent(TriggerImmediately, RecvWait);
end;

procedure TIdSipTcpMessageReader.ReturnInternalServerError(Connection: TIdTCPConnection;
                                                           const Reason: String);
var
  Res: TIdSipResponse;
begin
  Res := TIdSipResponse.Create;
  try
    // We really can't do much more than this.
    Res.StatusCode := SIPInternalServerError;
    Res.StatusText := Reason;
    Res.SipVersion := SipVersion;

    Connection.Write(Res.AsString);
  finally
    Res.Free;
  end;
end;

//******************************************************************************
//* TIdSipTcpClient                                                            *
//******************************************************************************
//* TIdSipTcpClient Public methods *********************************************

constructor TIdSipTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.OnDisconnected := Self.MarkAsTerminated;
  Self.ReadTimeout    := Self.DefaultTimeout;

  Self.MessageReader := TIdSipTcpMessageReader.Create;
  Self.MessageReader.Notifier.AddMessageListener(Self);

  Self.Terminated  := false;
end;

destructor TIdSipTcpClient.Destroy;
begin
  Self.MessageReader.Free;

  inherited Destroy;
end;

procedure TIdSipTcpClient.AddMessageListener(Listener: IIdSipMessageListener);
begin
  Self.MessageReader.Notifier.AddMessageListener(Listener);
end;

procedure TIdSipTcpClient.ReceiveMessages;
begin
  Self.MessageReader.ReadTimeout := Self.ReadTimeout;
  Self.MessageReader.ReadMessages(Self);
end;

procedure TIdSipTcpClient.RemoveMessageListener(Listener: IIdSipMessageListener);
begin
  Self.MessageReader.Notifier.RemoveMessageListener(Listener);
end;

procedure TIdSipTcpClient.Send(Msg: TIdSipMessage);
begin
  Self.Write(Msg.AsString);
end;

//* TIdSipTcpClient Protected methods ******************************************

function TIdSipTcpClient.DefaultTimeout: Cardinal;
begin
  Result := 5000;
end;

//* TIdSipTcpClient Private methods ********************************************

function TIdSipTcpClient.GetTimer: TIdTimerQueue;
begin
  Result := Self.MessageReader.Timer;
end;

procedure TIdSipTcpClient.MarkAsTerminated(Sender: TObject);
begin
  Self.Terminated := true;
end;

procedure TIdSipTcpClient.OnException(E: Exception;
                                      const Reason: String);
begin
  // Do nothing.
end;

procedure TIdSipTcpClient.OnMalformedMessage(const Msg: String;
                                             const Reason: String);
begin
  // Do nothing.
end;

procedure TIdSipTcpClient.OnReceiveRequest(Request: TIdSipRequest;
                                           ReceivedFrom: TIdSipConnectionBindings);
begin
  // This executes in the Timer context
  if Assigned(Self.OnRequest) then
    Self.OnRequest(Self, Request, ReceivedFrom);
end;

procedure TIdSipTcpClient.OnReceiveResponse(Response: TIdSipResponse;
                                            ReceivedFrom: TIdSipConnectionBindings);
begin
  // This executes in the Timer context
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, Response, ReceivedFrom);
end;

procedure TIdSipTcpClient.SetTerminated(Value: Boolean);
begin
  Self.fTerminated := Value;

  if Self.fTerminated and Self.Connected then
    Self.Disconnect;
end;

procedure TIdSipTcpClient.SetTimer(Value: TIdTimerQueue);
begin
  Self.MessageReader.Timer := Value;
end;

//******************************************************************************
//* TIdSipTlsClient                                                            *
//******************************************************************************
//* TIdSipTlsClient Public methods *********************************************

constructor TIdSipTlsClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.IOHandler := TIdSSLIOHandlerSocket.Create(nil);
end;

destructor TIdSipTlsClient.Destroy;
begin
  Self.IOHandler.Free;

  inherited Destroy;
end;

//******************************************************************************
//* TIdSipMessageExceptionWait                                                 *
//******************************************************************************
//* TIdSipMessageExceptionWait Public methods **********************************

procedure TIdSipMessageExceptionWait.Trigger;
var
  FakeException: Exception;
begin
  FakeException := Self.ExceptionType.Create(Self.ExceptionMessage);
  try
    Self.Listeners.NotifyListenersOfException(FakeException,
                                              Self.Reason);
  finally
    FakeException.Free;
  end;
end;

//******************************************************************************
//* TIdSipReceiveMessageWait                                                   *
//******************************************************************************
//* TIdSipReceiveMessageWait Public methods ************************************

destructor TIdSipReceiveMessageWait.Destroy;
begin
  Self.fReceivedFrom.Free;

  inherited Destroy;
end;

procedure TIdSipReceiveMessageWait.Trigger;
begin
  if Self.Message.IsRequest then
    Self.Listeners.NotifyListenersOfRequest(Self.Message as TIdSipRequest,
                                            Self.ReceivedFrom)
  else
    Self.Listeners.NotifyListenersOfResponse(Self.Message as TIdSipResponse,
                                             Self.ReceivedFrom);
end;

end.
