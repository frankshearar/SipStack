unit IdSipTcpClient;

interface

uses
  Classes, IdSipMessage, IdTCPClient{, IdSipTcpServer};

type
  TIdSipTcpRequestEvent = procedure(Sender: TObject; const Request: TIdSipRequest) of object;
  TIdSipTcpResponseEvent = procedure(Sender: TObject; const Response: TIdSipResponse) of object;

  // todo:
  // * resend requests on the same connection
  // * keep the connection alive for "long enough"
  TIdSipTcpClient = class(TIdTCPClient)
  private
    fOnFinished: TNotifyEvent;
    fOnResponse: TIdSipTcpResponseEvent;
    fTimeout:    Cardinal;

    procedure DoOnFinished;
    procedure DoOnResponse(const R: TIdSipResponse);
    function  ReadResponse(var TimedOut: Boolean): String;
    procedure ReadResponses;
  protected
    procedure DoOnDisconnected; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Send(const Request: TIdSipRequest); overload;
    procedure Send(const Response: TIdSipResponse); overload;

    property OnFinished: TNotifyEvent           read fOnFinished write fOnFinished;
    property OnResponse: TIdSipTcpResponseEvent read fOnResponse write fOnResponse;
    property Timeout:    Cardinal               read fTimeout write fTimeout;
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

  Self.Timeout := 5000;
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

procedure TIdSipTcpClient.DoOnResponse(const R: TIdSipResponse);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, R);
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
  Finished: Boolean;
  S:        String;
  P:        TIdSipParser;
  R:        TIdSipResponse;
begin
  Finished := false;

  try
    P := TIdSipParser.Create;
    try
      while (not Finished) do begin
        S := Self.ReadResponse(Finished);

        if (S <> '') then begin
          R := P.ParseAndMakeResponse(S);
          try
            R.Body := Self.ReadString(R.ContentLength);
            Self.DoOnResponse(R);

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
