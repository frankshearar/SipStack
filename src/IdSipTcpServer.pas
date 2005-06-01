{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipTcpServer;

interface

uses
  Classes, Contnrs, IdNotification, IdSipConsts, IdSipLocator, IdSipMessage,
  IdSipServerNotifier, IdSipTcpClient, IdTCPConnection, IdTCPServer,
  IdTimerQueue, SyncObjs, SysUtils;

type
  TIdSipAddConnectionEvent = procedure(Connection: TIdTCPConnection;
                                       Request: TIdSipRequest) of object;
  TIdSipRemoveConnectionEvent = procedure(Connection: TIdTCPConnection) of object;

  // ReadTimeout = -1 implies that we never timeout the body wait. We do not
  // recommend this. ReadTimeout = n implies we wait n milliseconds for
  // the body to be received. If we haven't read Content-Length bytes by the
  // time the timeout occurs, we sever the connection.
  TIdSipTcpServer = class(TIdTCPServer)
  private
    fConnectionTimeout:  Integer;
    fOnAddConnection:    TIdSipAddConnectionEvent;
    fOnRemoveConnection: TIdSipRemoveConnectionEvent;
    fReadTimeout:        Integer;
    fTimer:              TIdTimerQueue;
    Notifier:            TIdSipServerNotifier;

    procedure AddConnection(Connection: TIdTCPConnection;
                            Request: TIdSipRequest);
    procedure DoOnException(Sender: TObject); overload;
    procedure DoOnException(Thread: TIdPeerThread;
                            Exception: Exception); overload;
    procedure ReadBodyInto(Connection: TIdTCPConnection;
                           Msg: TIdSipMessage;
                           Dest: TStringStream);
    procedure ReadMessage(Connection: TIdTCPConnection;
                          Dest: TStringStream);
    procedure ReturnInternalServerError(Connection: TIdTCPConnection;
                                        const Reason: String);
    procedure ScheduleExceptionNotification(ExceptionType: ExceptClass;
                                            const Reason: String);
    procedure ScheduleReceivedMessage(Msg: TIdSipMessage;
                                      ReceivedFrom: TIdSipConnectionBindings);
    procedure WriteMessage(Connection: TIdTCPConnection;
                           Msg: TIdSipMessage);
  protected
    procedure DoDisconnect(Thread: TIdPeerThread); override;
    procedure DoOnExecute(Thread: TIdPeerThread);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddMessageListener(const Listener: IIdSipMessageListener);
    function  CreateClient: TIdSipTcpClient; virtual;
    function  DefaultTimeout: Cardinal; virtual;
    procedure DestroyClient(Client: TIdSipTcpClient); virtual;
    procedure RemoveMessageListener(const Listener: IIdSipMessageListener);
  published
    property ConnectionTimeout:  Integer                     read fConnectionTimeout write fConnectionTimeout;
    property OnAddConnection:    TIdSipAddConnectionEvent    read fOnAddConnection write fOnAddConnection;
    property OnRemoveConnection: TIdSipRemoveConnectionEvent read fOnRemoveConnection write fOnRemoveConnection;
    property ReadTimeout:        Integer                     read fReadTimeout write fReadTimeout;
    property Timer:              TIdTimerQueue               read fTimer write fTimer;
  end;

  TIdSipTcpServerClass = class of TIdSipTcpServer;

implementation

uses
  IdException, IdSipTransport;

//******************************************************************************
//* TIdSipTcpServer                                                            *
//******************************************************************************
//* TIdSipTcpServer Public methods *********************************************

constructor TIdSipTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.ConnectionTimeout := Self.DefaultTimeout;
  Self.DefaultPort       := TIdSipTransportRegistry.DefaultPortFor(TcpTransport);
  Self.Notifier          := TIdSipServerNotifier.Create;
  Self.ReadTimeout       := Self.DefaultTimeout;
  Self.OnExecute         := Self.DoOnExecute;
  Self.OnException       := Self.DoOnException;
end;

destructor TIdSipTcpServer.Destroy;
begin
  Self.Active := false;
  Self.Notifier.Free;

  inherited Destroy;
end;

procedure TIdSipTcpServer.AddMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Notifier.AddMessageListener(Listener);
end;

function TIdSipTcpServer.CreateClient: TIdSipTcpClient;
begin
  Result := TIdSipTcpClient.Create(nil);
end;

function TIdSipTcpServer.DefaultTimeout: Cardinal;
begin
  Result := 60000; // One minute
end;

procedure TIdSipTcpServer.DestroyClient(Client: TIdSipTcpClient);
begin
  Client.Free;
end;

procedure TIdSipTcpServer.RemoveMessageListener(const Listener: IIdSipMessageListener);
begin
  Self.Notifier.RemoveMessageListener(Listener);
end;

//* TIdSipTcpServer Protected methods ******************************************

procedure TIdSipTcpServer.DoDisconnect(Thread: TIdPeerThread);
begin
  if Assigned(Self.fOnRemoveConnection) then
    Self.fOnRemoveConnection(Thread.Connection);

  inherited DoDisconnect(Thread);
end;

procedure TIdSipTcpServer.DoOnExecute(Thread: TIdPeerThread);
var
  ConnClosed:   Boolean;
  Msg:          TIdSipMessage;
  ReceivedFrom: TIdSipConnectionBindings;
  S:            TStringStream;
begin
  ConnClosed := false;

  ReceivedFrom.PeerIP   := Thread.Connection.Socket.Binding.PeerIP;
  ReceivedFrom.PeerPort := Thread.Connection.Socket.Binding.PeerPort;


  Thread.Connection.ReadTimeout := Self.ReadTimeout;
  while Thread.Connection.Connected do begin
    S := TStringStream.Create('');
    try
      try
        Self.ReadMessage(Thread.Connection, S);
      except
        on EIdClosedSocket do
          ConnClosed := true;
      end;

      if not ConnClosed then begin
        Msg := TIdSipMessage.ReadMessageFrom(S);
        try
          try
            try
              Self.ReadBodyInto(Thread.Connection, Msg, S);
              Msg.ReadBody(S);
            except
              on EIdReadTimeout do
                ConnClosed := true;
              on EIdConnClosedGracefully do
                ConnClosed := true;
              on EIdClosedSocket do
                ConnClosed := true;
            end;

            // If Self.ReadBody closes the connection, we don't want to AddConnection!
            if Msg.IsRequest and not ConnClosed then
              Self.AddConnection(Thread.Connection, Msg as TIdSipRequest);

            Self.ScheduleReceivedMessage(Msg, ReceivedFrom);
          except
            on E: Exception do begin
              // This results in returning a 500 Internal Server Error to a response!
              if Thread.Connection.Connected then begin
                Self.ReturnInternalServerError(Thread.Connection, E.Message);
                Thread.Connection.DisconnectSocket;
              end;

              Self.ScheduleExceptionNotification(ExceptClass(E.ClassType),
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
  end;
end;

//* TIdSipTcpServer Private methods ********************************************

procedure TIdSipTcpServer.AddConnection(Connection: TIdTCPConnection;
                                        Request: TIdSipRequest);
begin
  if Assigned(Self.fOnAddConnection) then
    Self.fOnAddConnection(Connection, Request);
end;

procedure TIdSipTcpServer.DoOnException(Sender: TObject);
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

procedure TIdSipTcpServer.DoOnException(Thread: TIdPeerThread;
                                        Exception: Exception);
begin
  Self.ScheduleExceptionNotification(ExceptClass(Exception.ClassType),
                                     Exception.Message);
end;

procedure TIdSipTcpServer.ReadBodyInto(Connection: TIdTCPConnection;
                                       Msg: TIdSipMessage;
                                       Dest: TStringStream);
begin
  Connection.ReadStream(Dest, Msg.ContentLength);

  // Roll back the stream to just before the message body!
  Dest.Seek(-Msg.ContentLength, soFromCurrent);
end;

procedure TIdSipTcpServer.ReadMessage(Connection: TIdTCPConnection;
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

procedure TIdSipTcpServer.ReturnInternalServerError(Connection: TIdTCPConnection;
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

    Self.WriteMessage(Connection, Res);
  finally
    Res.Free;
  end;
end;

procedure TIdSipTcpServer.ScheduleExceptionNotification(ExceptionType: ExceptClass;
                                                        const Reason: String);
var
  Ex: TIdSipExceptionWait;
begin
  Ex := TIdSipExceptionWait.Create;
  Ex.Event         := Self.DoOnException;
  Ex.ExceptionType := ExceptionType;
  Ex.Reason        := Reason;
  Self.Timer.AddEvent(TriggerImmediately, Ex);
end;

procedure TIdSipTcpServer.ScheduleReceivedMessage(Msg: TIdSipMessage;
                                                  ReceivedFrom: TIdSipConnectionBindings);
var
  RecvWait: TIdSipReceiveTCPMessageWait;
begin
  RecvWait := TIdSipReceiveTCPMessageWait.Create;
  RecvWait.Message      := Msg.Copy;
  RecvWait.Listeners    := Self.Notifier;
  RecvWait.ReceivedFrom := ReceivedFrom;

  Self.Timer.AddEvent(TriggerImmediately, RecvWait);
end;

procedure TIdSipTcpServer.WriteMessage(Connection: TIdTCPConnection;
                                       Msg: TIdSipMessage);
begin
  Connection.Write(Msg.AsString);
end;

end.
