unit IdSipTcpClient;

interface

uses
  Classes, IdSipMessage, IdTCPClient, IdSipTcpServer;

type
  // todo:
  // * resend requests on the same connection
  // * keep the connection alive for "long enough"
  TIdSipTcpClient = class(TIdTCPClient)
  private
    fOnResponse: TIdSipTcpResponseEvent;
    fTimeout:    Cardinal;

    procedure DoOnResponse(const R: TIdSipResponse);
    procedure ReadResponses;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Send(const Request: TIdSipRequest);

    property OnResponse: TIdSipTcpResponseEvent read fOnResponse write fOnResponse;
    property Timeout:    Cardinal               read fTimeout write fTimeout;
  end;

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

//* TIdSipTcpClient Private methods ********************************************

procedure TIdSipTcpClient.DoOnResponse(const R: TIdSipResponse);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, R);
end;

procedure TIdSipTcpClient.ReadResponses;
var
  Finished:              Boolean;
  Line:                  String;
  S:                     TStringStream;
  P:                     TIdSipParser;
  R:                     TIdSipResponse;
begin
  Finished := false;

  try
    while (not Finished) do begin
      S := TStringStream.Create('');
      try
        Line := Self.ReadLn(#$A, Timeout);
        while (Line <> '') do begin
          S.WriteString(Line);
          Line := Self.ReadLn(#$A, Timeout);
        end;
        S.Seek(0, soFromBeginning);
        Finished := Self.ReadLnTimedOut;

        if (S.DataString <> '') then begin
          P := TIdSipParser.Create;
          try
            P.Source := S;
            R := P.ParseAndMakeResponse;
            try
              R.Body := Self.ReadString(R.ContentLength);
              Self.DoOnResponse(R);

              Finished := R.IsFinal;
            finally
              R.Free;
            end;
          finally
            P.Free;
          end;
        end;
      finally
        S.Free;
      end;
    end;
  except
    on EIdConnClosedGracefully do;
  end;
end;

end.
