unit IdSipTcpClient;

interface

uses
  Classes, IdSipMessage, IdTCPClient;

type
  TIdSipConnectionBindings = record
    LocalIP:   String;
    LocalPort: Integer;
    PeerIP:    String;
    PeerPort:  Integer;
  end;

  TIdSipTcpClient = class;
  TIdSipClientEvent = procedure(Sender: TObject) of object;
  TIdSipResponseEvent = procedure(Sender: TObject;
                                  const R: TIdSipResponse;
                                  const ReceivedFrom: TIdSipConnectionBindings) of object;

  // Note that the Timeout property determines the maximum length of time to
  // wait for the next line of data to arrive.
  TIdSipTcpClient = class(TIdTCPClient)
  private
    fOnFinished: TIdSipClientEvent;
    fOnResponse: TIdSipResponseEvent;
    fTimeout:    Cardinal;

    procedure DoOnFinished;
    procedure DoOnResponse(const R: TIdSipResponse;
                           const ReceivedFrom: TIdSipConnectionBindings);
    function  ReadResponse(var TimedOut: Boolean): String;
    procedure ReadResponses;
  protected
    procedure DoOnDisconnected; override;
  public
    constructor Create(AOwner: TComponent); override;

    function  DefaultTimeout: Cardinal; virtual;
    procedure Send(const Request: TIdSipRequest); overload;
    procedure Send(const Response: TIdSipResponse); overload;

    property OnFinished:    TIdSipClientEvent   read fOnFinished write fOnFinished;
    property OnResponse:    TIdSipResponseEvent read fOnResponse write fOnResponse;
    property Timeout:       Cardinal            read fTimeout write fTimeout;
  end;

  TIdSipTcpClientClass = class of TIdSipTcpClient;

implementation

uses
  IdException, IdTCPConnection;

//******************************************************************************
//* TIdSipTcpClient                                                            *
//******************************************************************************
//* TIdSipTcpClient Public methods *********************************************

constructor TIdSipTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Timeout := Self.DefaultTimeout;
end;

function TIdSipTcpClient.DefaultTimeout: Cardinal;
begin
  Result := 5000;
end;

procedure TIdSipTcpClient.Send(const Request: TIdSipRequest);
begin
  Self.Write(Request.AsString);
  Self.ReadResponses;
end;

procedure TIdSipTcpClient.Send(const Response: TIdSipResponse);
begin
  Self.Write(Response.AsString);
end;

//* TIdSipTcpClient Protected methods ******************************************

procedure TIdSipTcpClient.DoOnDisconnected;
begin
  inherited DoOnDisconnected;
  Self.DoOnFinished;
end;

//* TIdSipTcpClient Private methods ********************************************

procedure TIdSipTcpClient.DoOnFinished;
begin
  if Assigned(Self.OnFinished) then
    Self.OnFinished(Self);
end;

procedure TIdSipTcpClient.DoOnResponse(const R: TIdSipResponse;
                                       const ReceivedFrom: TIdSipConnectionBindings);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, R, ReceivedFrom);
end;

function TIdSipTcpClient.ReadResponse(var TimedOut: Boolean): String;
var
  Line: String;
begin
  Result := '';

  Line := Self.ReadLn(#$A, Self.Timeout);

  while (Line <> '') do begin
    Result := Result + Line + #13#10;
    Line := Self.ReadLn(#$A, Self.Timeout);
  end;

  TimedOut := Self.ReadLnTimedOut;
end;

procedure TIdSipTcpClient.ReadResponses;
var
  Finished:     Boolean;
  S:            String;
  P:            TIdSipParser;
  R:            TIdSipResponse;
  ReceivedFrom: TIdSipConnectionBindings;
begin
  Finished := false;

  ReceivedFrom.LocalIP   := Self.Socket.Binding.IP;
  ReceivedFrom.LocalPort := Self.Socket.Binding.Port;
  ReceivedFrom.PeerIP    := Self.Socket.Binding.PeerIP;
  ReceivedFrom.PeerPort  := Self.Socket.Binding.PeerPort;

  try
    P := TIdSipParser.Create;
    try
      while (not Finished) do begin
        S := Self.ReadResponse(Finished);

        if (S <> '') then begin
          R := P.ParseAndMakeResponse(S);
          try
            R.Body := Self.ReadString(R.ContentLength);
            Self.DoOnResponse(R, ReceivedFrom);

            Finished := R.IsFinal;
          finally
            R.Free;
          end;
        end;
      end;
    finally
      P.Free;
    end;
  except
    on EIdConnClosedGracefully do;
  end;
  
  Self.DoOnFinished;
end;

end.
