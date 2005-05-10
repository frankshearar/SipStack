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
  Classes, IdSipMessage, IdTCPClient, IdTCPConnection, IdTimerQueue;

type
  // Note that the Timeout property determines the maximum length of time to
  // wait for the next line of data to arrive.
  TIdSipTcpClient = class(TIdTCPClient)
  private
    fOnRequest:  TIdSipRequestEvent;
    fOnResponse: TIdSipResponseEvent;
    fTerminated: Boolean;

    procedure DoOnMessage(Msg: TIdSipMessage;
                          ReceivedFrom: TIdSipConnectionBindings);
    procedure ReadBodyInto(Msg: TIdSipMessage;
                           Dest: TStringStream);
    function  ReadMessage(Dest: TStringStream): String;
    procedure ReadMessages;
  protected
    function  DefaultTimeout: Cardinal; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Send(Request: TIdSipRequest); overload;
    procedure Send(Response: TIdSipResponse); overload;

    property OnRequest:  TIdSipRequestEvent  read fOnRequest write fOnRequest;
    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
    property Terminated: Boolean             read fTerminated write fTerminated;
  end;

  TIdSipTcpClientClass = class of TIdSipTcpClient;

  TIdSipTlsClient = class(TIdSipTcpClient)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

implementation

uses
  IdException, IdSipTcpServer, IdSSLOpenSSL, SysUtils;

//******************************************************************************
//* TIdSipTcpClient                                                            *
//******************************************************************************
//* TIdSipTcpClient Public methods *********************************************

constructor TIdSipTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.ReadTimeout := Self.DefaultTimeout;
  Self.Terminated  := false;  
end;

procedure TIdSipTcpClient.Send(Request: TIdSipRequest);
begin
  Self.Write(Request.AsString);
  Self.ReadMessages;
end;

procedure TIdSipTcpClient.Send(Response: TIdSipResponse);
begin
  Self.Write(Response.AsString);
end;

//* TIdSipTcpClient Protected methods ******************************************

function TIdSipTcpClient.DefaultTimeout: Cardinal;
begin
  Result := 5000;
end;

//* TIdSipTcpClient Private methods ********************************************

procedure TIdSipTcpClient.DoOnMessage(Msg: TIdSipMessage;
                                      ReceivedFrom: TIdSipConnectionBindings);
begin
  if Msg.IsRequest then begin
    if Assigned(Self.OnRequest) then
      Self.OnRequest(Self, Msg as TIdSipRequest, ReceivedFrom)
  end
  else begin
    if Assigned(Self.OnResponse) then
      Self.OnResponse(Self, Msg as TIdSipResponse, ReceivedFrom);
  end;
end;

procedure TIdSipTcpClient.ReadBodyInto(Msg: TIdSipMessage;
                                       Dest: TStringStream);
begin
  Self.ReadStream(Dest, Msg.ContentLength);

  // Roll back the stream to just before the message body!
  Dest.Seek(-Msg.ContentLength, soFromCurrent);
end;

function TIdSipTcpClient.ReadMessage(Dest: TStringStream): String;
const
  CrLf = #$D#$A;
begin
  // We skip any leading CRLFs, and read up to (and including) the first blank
  // line.
  while (Dest.DataString = '') do
    Self.Capture(Dest, '');

  // Capture() returns up to the blank line, but eats it: we add it back in
  // manually.
  Dest.Write(CrLf, Length(CrLf));
  Dest.Seek(0, soFromBeginning);
end;

procedure TIdSipTcpClient.ReadMessages;
var
  S:            TStringStream;
  Msg:          TIdSipMessage;
  ReceivedFrom: TIdSipConnectionBindings;
begin
  ReceivedFrom.LocalIP   := Self.Socket.Binding.IP;
  ReceivedFrom.LocalPort := Self.Socket.Binding.Port;
  ReceivedFrom.PeerIP    := Self.Socket.Binding.PeerIP;
  ReceivedFrom.PeerPort  := Self.Socket.Binding.PeerPort;

  while (not Self.Terminated and Self.Connected) do begin
    S := TStringStream.Create('');
    try
      try
        Self.ReadMessage(S);
        Msg := TIdSipMessage.ReadMessageFrom(S);
        try
          try
            Self.ReadBodyInto(Msg, S);
            Msg.ReadBody(S);

            Self.DoOnMessage(Msg, ReceivedFrom);
          except
            // Exceptions reading the body of a message from the socket.
            on E: Exception do begin
              if not Msg.IsRequest then begin
                //Self.ReturnInternalServerError(E.Message);
                Self.Disconnect;
                Self.Terminated := true;
              end;
            end;
          end;
        finally
          Msg.Free;
        end;
      except
        // Exceptions reading a message from the socket.
        on EIdReadTimeout do
          Self.Terminated := true;
        on EIdConnClosedGracefully do
          Self.Terminated := true;
        on EIdClosedSocket do
          Self.Terminated := true;
      end;
    finally
      S.Free;
    end;
  end;
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

end.
