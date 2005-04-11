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
  Classes, IdSipMessage, IdTCPClient, IdTimerQueue;

type
  // Note that the Timeout property determines the maximum length of time to
  // wait for the next line of data to arrive.
  TIdSipTcpClient = class(TIdTCPClient)
  private
    fIsFinished: Boolean;
    fOnResponse: TIdSipResponseEvent;
    fTimer:      TIdTimerQueue;

    procedure DoOnReceiveMessage(Sender: TObject);
    procedure DoOnResponse(R: TIdSipResponse;
                           ReceivedFrom: TIdSipConnectionBindings);
    procedure MarkFinished;
    procedure MarkUnfinished;
    function  ReadResponse(var TimedOut: Boolean): String;
    procedure ReadResponses;
  protected
    function  DefaultTimeout: Cardinal; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Send(Request: TIdSipRequest); overload;
    procedure Send(Response: TIdSipResponse); overload;

    property IsFinished: Boolean             read fIsFinished;
    property OnResponse: TIdSipResponseEvent read fOnResponse write fOnResponse;
    property Timer:      TIdTimerQueue       read fTimer write fTimer;
  end;

  TIdSipTcpClientClass = class of TIdSipTcpClient;

implementation

uses
  IdException, IdSipTcpServer, IdTCPConnection;

//******************************************************************************
//* TIdSipTcpClient                                                            *
//******************************************************************************
//* TIdSipTcpClient Public methods *********************************************

constructor TIdSipTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.MarkUnfinished;
  
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

//* TIdSipTcpClient Private methods ********************************************

procedure TIdSipTcpClient.DoOnReceiveMessage(Sender: TObject);
var
  Wait: TIdSipReceiveTCPMessageWait;
begin
  Wait := Sender as TIdSipReceiveTCPMessageWait;

  if not Wait.Message.IsRequest then
    Self.DoOnResponse(Wait.Message as TIdSipResponse,
                      Wait.ReceivedFrom);
end;

procedure TIdSipTcpClient.DoOnResponse(R: TIdSipResponse;
                                       ReceivedFrom: TIdSipConnectionBindings);
begin
  if Assigned(Self.OnResponse) then
    Self.OnResponse(Self, R, ReceivedFrom);
end;

procedure TIdSipTcpClient.MarkFinished;
begin
  Self.fIsFinished := true;
end;

procedure TIdSipTcpClient.MarkUnfinished;
begin
  Self.fIsFinished := false;
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
  R:            TIdSipResponse;
  ReceivedFrom: TIdSipConnectionBindings;
  RecvWait:     TIdSipReceiveTCPMessageWait;
begin
  Finished := false;

  ReceivedFrom.LocalIP   := Self.Socket.Binding.IP;
  ReceivedFrom.LocalPort := Self.Socket.Binding.Port;
  ReceivedFrom.PeerIP    := Self.Socket.Binding.PeerIP;
  ReceivedFrom.PeerPort  := Self.Socket.Binding.PeerPort;

  try
    while (not Finished) do begin
      S := Self.ReadResponse(Finished);

      if (S <> '') then begin
        R := TIdSipMessage.ReadResponseFrom(S);
        try
          try
            R.Body := Self.ReadString(R.ContentLength);
          except
            on EIdConnClosedGracefully do;
          end;

          RecvWait := TIdSipReceiveTCPMessageWait.Create;
          RecvWait.Event        := Self.DoOnReceiveMessage;
          RecvWait.Message      := R.Copy;
          RecvWait.ReceivedFrom := ReceivedFrom;
          Self.Timer.AddEvent(TriggerImmediately, RecvWait);

          Finished := R.IsFinal;
        finally
          R.Free;
        end;
      end;
    end;
  except
    on EIdConnClosedGracefully do;
  end;

  Self.MarkFinished;
end;

end.
