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
  Classes, IdSipMessage, IdTCPClient;

type
  TIdSipTcpClient = class;
  TIdSipResponseEvent = procedure(Sender: TObject;
                                  R: TIdSipResponse;
                                  ReceivedFrom: TIdSipConnectionBindings) of object;

  TIdSipTcpClient = class(TIdTCPClient)
  private
    fOnFinished: TNotifyEvent;
    fOnResponse: TIdSipResponseEvent;
    fTimeout:    Cardinal;

    procedure DoOnFinished;
    procedure DoOnResponse(R: TIdSipResponse;
                           ReceivedFrom: TIdSipConnectionBindings);
    function  ReadResponse(var TimedOut: Boolean): String;
    procedure ReadResponses;
  protected
    function  DefaultTimeout: Cardinal; virtual;
    procedure DoOnDisconnected; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Send(Request: TIdSipRequest); overload;
    procedure Send(Response: TIdSipResponse); overload;

    property OnFinished:    TNotifyEvent        read fOnFinished write fOnFinished;
    property OnResponse:    TIdSipResponseEvent read fOnResponse write fOnResponse;
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

  Self.ReadTimeout := Self.DefaultTimeout;
end;

procedure TIdSipTcpClient.Send(Request: TIdSipRequest);
begin
  Self.Write(Request.AsString);
  Self.ReadResponses;
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

procedure TIdSipTcpClient.DoOnResponse(R: TIdSipResponse;
                                       ReceivedFrom: TIdSipConnectionBindings);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, R, ReceivedFrom);
end;

function TIdSipTcpClient.ReadResponse(var TimedOut: Boolean): String;
var
  Line: String;
begin
  Result := '';

  Line := Self.ReadLn(#$A, Self.ReadTimeout);

  while (Line <> '') do begin
    Result := Result + Line + #13#10;
    Line := Self.ReadLn(#$A, Self.ReadTimeout);
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
