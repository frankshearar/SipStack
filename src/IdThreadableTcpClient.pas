{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdThreadableTcpClient;

interface

uses
  Classes, IdBaseThread, IdTCPClient, IdTimerQueue, SysUtils;

type
  // I provide a way of receiving data and notifying a TimerQueue. My subclasses
  // schedule TIdWaits in the TimerQueue.
  TIdThreadableTcpClient = class(TIdTCPClient)
  private
    fConserveConnections: Boolean;
    fTerminated:          Boolean;

    procedure MarkAsTerminated(Sender: TObject);
    procedure SetConserveConnections(Value: Boolean);
    procedure SetTerminated(Value: Boolean);
  protected
    function  DefaultTimeout: Cardinal; virtual;
    function  GetTimer: TIdTimerQueue; virtual;
    procedure SetTimer(Value: TIdTimerQueue); virtual;
  public
   constructor Create(AOwner: TComponent); override;

    procedure Connect(const Timeout: Integer); override;
    procedure ReceiveMessages; virtual;

    property ConserveConnections: Boolean       read fConserveConnections write SetConserveConnections;
    property Terminated:          Boolean       read fTerminated write SetTerminated;
    property Timer:               TIdTimerQueue read GetTimer write SetTimer;
  end;

  // I allow a TIdThreadableTcpClient to run in the context of its own thread.
  TIdThreadedTcpClient = class(TIdBaseThread)
  private
    fClient: TIdThreadableTcpClient;
  protected
    function  GetTimer: TIdTimerQueue; virtual;
    procedure NotifyOfException(E: Exception); virtual;
    procedure SetTimer(Value: TIdTimerQueue); virtual;
  public
    constructor Create(Connection: TIdThreadableTcpClient); reintroduce;

    procedure Terminate; override;

    property Client: TIdThreadableTcpClient read fClient;
    property Timer:  TIdTimerQueue          read GetTimer write SetTimer;
  end;

const
  FiveSeconds = 5000; // milliseconds

implementation

uses
  IdIndyUtils, RuntimeSafety;

//******************************************************************************
//* TIdThreadableTcpClient                                                     *
//******************************************************************************
//* TIdThreadableTcpClient Public methods **************************************

constructor TIdThreadableTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.ConserveConnections := true;
  Self.OnDisconnected      := Self.MarkAsTerminated;
  Self.ReadTimeout         := Self.DefaultTimeout;
  Self.Terminated          := false;
end;

procedure TIdThreadableTcpClient.Connect(const Timeout: Integer);
begin
  inherited Connect(Timeout);

  KeepAliveSocket(Self, Self.ConserveConnections);
end;

procedure TIdThreadableTcpClient.ReceiveMessages;
begin
  RaiseAbstractError(Self.ClassName, 'ReceiveMessages');
end;

//* TIdThreadableTcpClient Protected methods ***********************************

function TIdThreadableTcpClient.DefaultTimeout: Cardinal;
begin
  Result := FiveSeconds;
end;

function TIdThreadableTcpClient.GetTimer: TIdTimerQueue;
begin
  Result := nil;
  RaiseAbstractError(Self.ClassName, 'GetTimer');
end;

procedure TIdThreadableTcpClient.SetTimer(Value: TIdTimerQueue);
begin
  RaiseAbstractError(Self.ClassName, 'SetTimer');
end;

//* TIdThreadableTcpClient Private methods *************************************

procedure TIdThreadableTcpClient.MarkAsTerminated(Sender: TObject);
begin
  Self.Terminated := true;
end;

procedure TIdThreadableTcpClient.SetConserveConnections(Value: Boolean);
begin
  Self.fConserveConnections := Value;

  KeepAliveSocket(Self, Value);
end;

procedure TIdThreadableTcpClient.SetTerminated(Value: Boolean);
begin
  Self.fTerminated := Value;

  if Self.fTerminated and Self.Connected then
    Self.Disconnect;
end;

//******************************************************************************
//* TIdThreadedTcpClient                                                       *
//******************************************************************************
//* TIdThreadedTcpClient Public methods ****************************************

constructor TIdThreadedTcpClient.Create(Connection: TIdThreadableTcpClient);
begin
  Self.FreeOnTerminate := true;
  Self.fClient := Connection;

  inherited Create(false);
end;

procedure TIdThreadedTcpClient.Terminate;
begin
  Self.Client.Terminated := true;

  inherited Terminate;
end;

//* TIdThreadedTcpClient Protected methods *************************************

function TIdThreadedTcpClient.GetTimer: TIdTimerQueue;
begin
  Result := nil;
  RaiseAbstractError(Self.ClassName, 'GetTimer');
end;

procedure TIdThreadedTcpClient.NotifyOfException(E: Exception);
begin
  RaiseAbstractError(Self.ClassName, 'NotifyOfException');
end;

procedure TIdThreadedTcpClient.SetTimer(Value: TIdTimerQueue);
begin
  RaiseAbstractError(Self.ClassName, 'SetTimer');
end;

end.
