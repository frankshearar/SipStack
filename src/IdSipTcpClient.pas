unit IdSipTcpClient;

interface

uses
  IdSipMessage, IdTCPClient, IdSipUdpServer;

type
  // todo:
  // * resend requests on the same connection
  // * keep the connection alive for "long enough"
  TIdSipTcpClient = class(TIdTCPClient)
  private
    fOnResponse: TIdSipResponseEvent;
    procedure DoOnResponse(const R: TIdSipResponse);
    procedure ReadResponses;
  public
    procedure Send(const Request: TIdSipRequest);

    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
  end;

implementation

uses
  Classes, IdSipParser;

//******************************************************************************
//* TIdSipTcpClient                                                            *
//******************************************************************************
//* TIdSipTcpClient Public methods *********************************************

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
  S:                     TStringStream;
  P:                     TIdSipParser;
  R:                     TIdSipResponse;
  ReceivedFinalResponse: Boolean;
begin
  ReceivedFinalResponse := false;

  while (not ReceivedFinalResponse) do begin
    S := TStringStream.Create('');
    try
      while (S.DataString = '') do
        Self.Capture(S, '');
      S.Seek(0, soFromBeginning);

      P := TIdSipParser.Create;
      try
        P.Source := S;
        R := P.ParseAndMakeMessage as TIdSipResponse;
        try
          R.Body := Self.ReadString(R.ContentLength);
          Self.DoOnResponse(R);

          ReceivedFinalResponse := R.IsFinal;
        finally
          R.Free;
        end;
      finally
        P.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

end.
