unit IdSipTcpServer;

interface

uses
  Classes, IdSipParser, IdTCPConnection, IdTCPServer;

type
  TIdSipMethodEvent = procedure(AThread: TIdPeerThread;
                                AMessage: TIdSipMessage) of object;

  TIdSipTcpServer = class(TIdTCPServer)
  private
    fOnMethod: TIdSipMethodEvent;

    procedure  DoOnMethod(AThread: TIdPeerThread;
                          AMessage: TIdSipMessage);
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
    property OnMethod: TIdSipMethodEvent read fOnMethod write fOnMethod;
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
  Parser:  TIdSipParser;
  Msg: TIdSipMessage;
  S:       TStream;
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

            Self.DoOnMethod(AThread, Msg)
          finally
            Msg.Free;
          end;
        except
          on E: EBadRequest do begin
            Self.ReturnBadRequest(AThread.Connection, E.Message, Parser);
            AThread.Connection.Disconnect;
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

function TIdSipTcpServer.ReadBody(Connection: TIdTCPConnection; Message: TIdSipMessage): String;
begin
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
