unit IdSipTcpServer;

interface

uses
  Classes, IdSipConsts, IdSipMessage, IdSipTimer, IdTCPConnection,
  IdTCPServer;

type
  TIdSipTcpConnectionCutter = class(TIdSipTimer)
  private
    fConnection: TIdTCPConnection;
  public
    property Connection: TIdTCPConnection read fConnection write fConnection;
  end;

  TIdSipTcpRequestEvent = procedure(Sender: TObject; const Request: TIdSipRequest) of object;
  TIdSipTcpResponseEvent = procedure(Sender: TObject; const Response: TIdSipResponse) of object;

  // ReadBodyTimeout = 0 implies that we never timeout the body wait. This is
  // not recommended. ReadBodyTimeout = n implies we wait n milliseconds for
  // the body to be received. If we haven't read Content-Length bytes by the
  // time the timeout occurs, we sever the connection.
  TIdSipTcpServer = class(TIdTCPServer, IIdSipMessageVisitor)
  private
    fOnRequest:       TIdSipTcpRequestEvent;
    fOnResponse:      TIdSipTcpResponseEvent;
    fReadBodyTimeout: Cardinal;

    procedure DoOnRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure DoOnResponse(Sender: TObject; const Response: TIdSipResponse);
    procedure OnReadBodyTimeout(Sender: TObject);
    function  ReadBody(Connection: TIdTCPConnection; Message: TIdSipMessage): String;
    function  ReadMessage(Connection: TIdTCPConnection): TStream;
    procedure ReturnBadRequest(Connection: TIdTCPConnection; Reason: String; Parser: TIdSipParser);
    procedure WriteMessage(Connection: TIdTCPConnection; AMessage: TIdSipMessage);
  protected
    function DoExecute(AThread: TIdPeerThread): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure VisitRequest(const Request: TIdSipRequest);
    procedure VisitResponse(const Response: TIdSipResponse);
  published
    property DefaultPort default IdPORT_SIP;
    property OnRequest:       TIdSipTcpRequestEvent  read fOnRequest write fOnRequest;
    property OnResponse:      TIdSipTcpResponseEvent read fOnResponse write fOnResponse;
    property ReadBodyTimeout: Cardinal               read fReadBodyTimeout write fReadBodyTimeout;
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

procedure TIdSipTcpServer.VisitRequest(const Request: TIdSipRequest);
begin
  Self.DoOnRequest(Self, Request);
end;

procedure TIdSipTcpServer.VisitResponse(const Response: TIdSipResponse);
begin
  Self.DoOnResponse(Self, Response);
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

            if TIdSipParser.IsFQDN(Msg.LastHop.SentBy)
              or (Msg.LastHop.SentBy <> AThread.Connection.Socket.Binding.IP) then
              Msg.LastHop.Received := AThread.Connection.Socket.Binding.IP;

            Msg.Accept(Self);
          finally
            Msg.Free;
          end;
        except
          on E: EBadRequest do begin
            Self.ReturnBadRequest(AThread.Connection, E.Message, Parser);
            AThread.Connection.DisconnectSocket;
          end;
          on E: EBadResponse do begin
            // drop it on the floor
            AThread.Connection.DisconnectSocket;
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

procedure TIdSipTcpServer.DoOnRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  if Assigned(Self.OnRequest) then
    Self.OnRequest(Self, Request);
end;

procedure TIdSipTcpServer.DoOnResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, Response);
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
  Result := TStringStream.Create('');
  try
    Str := Result as TStringStream;

    // we skip any leading CRLFs
    while (Str.DataString = '') do
      Connection.Capture(Str, '');
    Str.Seek(0, soFromBeginning);
  except
    Result.Free;

    raise;
  end;
end;

procedure TIdSipTcpServer.ReturnBadRequest(Connection: TIdTCPConnection; Reason: String; Parser: TIdSipParser);
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
    
    Self.WriteMessage(Connection, Res);
  finally
    Res.Free;
  end;
end;

procedure TIdSipTcpServer.WriteMessage(Connection: TIdTCPConnection; AMessage: TIdSipMessage);
begin
  Connection.Write(AMessage.AsString);
end;

end.
