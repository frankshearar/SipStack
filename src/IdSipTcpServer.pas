unit IdSipTcpServer;

interface

uses
  Classes, IdSipMessage, IdSipParser, IdSipTimer, IdTCPConnection, IdTCPServer;

type
  TIdSipMethodEvent = procedure(AThread: TIdPeerThread;
                                AMessage: TIdSipMessage) of object;

  TIdSipTcpConnectionCutter = class(TIdSipTimer)
  private
    fConnection: TIdTCPConnection;
  public
    property Connection: TIdTCPConnection read fConnection write fConnection;
  end;

  // ReadBodyTimeout = 0 implies that we never timeout the body wait. This is
  // not recommended. ReadBodyTimeout = n implies we wait n milliseconds for
  // the body to be received. If we haven't read Content-Length bytes by the
  // time the timeout occurs, we sever the connection.
  TIdSipTcpServer = class(TIdTCPServer)
  private
    fOnMethod:        TIdSipMethodEvent;
    fReadBodyTimeout: Cardinal;

    procedure DoOnMethod(AThread: TIdPeerThread;
                         AMessage: TIdSipMessage);
    procedure OnReadBodyTimeout(Sender: TObject);
    function  ReadBody(Connection: TIdTCPConnection; Message: TIdSipMessage): String;
    function  ReadMessage(Connection: TIdTCPConnection): TStream;
    procedure ReturnBadRequest(Connection: TIdTCPConnection; Reason: String; Parser: TIdSipParser);
    procedure WriteMessage(Connection: TIdTCPConnection; AMessage: TIdSipMessage);
  protected
    function DoExecute(AThread: TIdPeerThread): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DefaultPort default IdPORT_SIP;
    property OnMethod:        TIdSipMethodEvent read fOnMethod write fOnMethod;
    property ReadBodyTimeout: Cardinal          read fReadBodyTimeout write fReadBodyTimeout;
  end;

implementation

uses
  SysUtils;

//******************************************************************************
//* TIdSipTcpServer                                                            *
//******************************************************************************
//* TIdSipTcpServer Public methods *********************************************

constructor TIdSipTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.DefaultPort := IdPORT_SIP;
end;

//* TIdSipTcpServer Protected methods ******************************************

function TIdSipTcpServer.DoExecute(AThread: TIdPeerThread): Boolean;
var
  Msg:    TIdSipMessage;
  Parser: TIdSipParser;
  S:      TStream;
begin
  Result := true;

  while AThread.Connection.Connected do begin
    S := Self.ReadMessage(AThread.Connection);
    try
      Parser := TIdSipParser.Create;
      try
        Parser.Source := S;

        try
          Msg := Parser.ParseAndMakeMessage;
          try
            if not Msg.Headers.HasHeader(ContentLengthHeaderFull) then
              Msg.ContentLength := 0;
            Msg.Body := Self.ReadBody(AThread.Connection, Msg);

            if TIdSipParser.IsFQDN(Msg.Path.LastHop.Host)
              or (Msg.Path.LastHop.Host <> AThread.Connection.Socket.Binding.IP) then
              Msg.Path.LastHop.Received := AThread.Connection.Socket.Binding.IP;

            Self.DoOnMethod(AThread, Msg)
          finally
            Msg.Free;
          end;
        except
          on E: EBadRequest do begin
            Self.ReturnBadRequest(AThread.Connection, E.Message, Parser);
            AThread.Connection.DisconnectSocket;
          end;
          on EBadResponse do begin
            // tear down the connection?
          end;
        end;
      finally
        Parser.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

//* TIdSipTcpServer Private methods ********************************************

procedure TIdSipTcpServer.DoOnMethod(AThread: TIdPeerThread;
                                     AMessage: TIdSipMessage);
begin
  if Assigned(Self.OnMethod) then
    Self.OnMethod(AThread, AMessage);
end;

procedure TIdSipTcpServer.OnReadBodyTimeout(Sender: TObject);
begin
  (Sender as TIdSipTcpConnectionCutter).Connection.DisconnectSocket;
end;

function TIdSipTcpServer.ReadBody(Connection: TIdTCPConnection; Message: TIdSipMessage): String;
var
  Timer: TIdSipTcpConnectionCutter;
begin
  if (Self.ReadBodyTimeout > 0) then begin
    Timer := TIdSipTcpConnectionCutter.Create(true);
    try
      Timer.FreeOnTerminate := true;
      Timer.OnTimer := Self.OnReadBodyTimeout;
      Timer.Start;
    except
      Timer.Free;
    end;
  end;

  Result := Connection.ReadString(Message.ContentLength);
end;

function TIdSipTcpServer.ReadMessage(Connection: TIdTCPConnection): TStream;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('');

  // we skip any leading CRLFs
  while (Str.DataString = '') do
    Connection.Capture(Str, '');
  Str.Seek(0, soFromBeginning);

  Result := Str;
end;

procedure TIdSipTcpServer.ReturnBadRequest(Connection: TIdTCPConnection; Reason: String; Parser: TIdSipParser);
var
  Msg: TIdSipMessage;
begin
  Msg := Parser.MakeBadRequestResponse(Reason);
  try
    Self.WriteMessage(Connection, Msg);
  finally
    Msg.Free;
  end;
end;

procedure TIdSipTcpServer.WriteMessage(Connection: TIdTCPConnection; AMessage: TIdSipMessage);
begin
  Connection.Write(AMessage.AsString);
end;

end.
