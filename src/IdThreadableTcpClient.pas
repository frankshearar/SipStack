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
  Classes, IdBaseThread, IdConnectionBindings, IdRegisteredObject, IdTCPClient,
  IdTCPConnection, IdTimerQueue, SysUtils;

type
  TIdThreadableTcpClient = class;

  TReceiveMessageProc = procedure(Sender: TObject; Msg: String; ReceivedOn: TIdConnectionBindings) of object;
  TIdSdpTcpClientProc = procedure(C: TIdThreadableTcpClient) of object;

  // I provide a way of receiving data and notifying a TimerQueue. My subclasses
  // schedule TIdWaits in the TimerQueue.
  TIdThreadableTcpClient = class(TIdTCPClient)
  private
    fCachedBindings:      TIdConnectionBindings;
    fConserveConnections: Boolean;
    fID:                  TRegisteredObjectID;
    fOnIdle:              TIdSdpTcpClientProc;
    fOnReceiveMessage:    TReceiveMessageProc;
    fTerminated:          Boolean;

    procedure MarkAsTerminated(Sender: TObject);
    procedure SetConserveConnections(Value: Boolean);
    procedure SetTerminated(Value: Boolean);
  protected
    function  DefaultTimeout: Cardinal; virtual;
    procedure DoOnIdle(C: TIdThreadableTcpClient);
    function  GetTimer: TIdTimerQueue; virtual;
    procedure ReceiveMessage(Msg: String; ReceivedOn: TIdConnectionBindings); overload; virtual;
    procedure ReceiveMessage(Msg: TStream; ReceivedOn: TIdConnectionBindings); overload; virtual;
    procedure SetTimer(Value: TIdTimerQueue); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Connect(const Timeout: Integer); override;
    function  Protocol: String; virtual;
    procedure ReceiveMessages; virtual;

    property CachedBindings:      TIdConnectionBindings read fCachedBindings write fCachedBindings;
    property ConserveConnections: Boolean               read fConserveConnections write SetConserveConnections;
    property ID:                  TRegisteredObjectID   read fID;
    property OnIdle:              TIdSdpTcpClientProc   read fOnIdle write fOnIdle;
    property OnReceiveMessage:    TReceiveMessageProc   read fOnReceiveMessage write fOnReceiveMessage;
    property Terminated:          Boolean               read fTerminated write SetTerminated;
    property Timer:               TIdTimerQueue         read GetTimer write SetTimer;
  end;

  // I allow a TIdThreadableTcpClient to run in the context of its own thread.
  TIdThreadedTcpClient = class(TIdBaseThread)
  private
    fClient: TIdThreadableTcpClient;
  protected
    function  GetTimer: TIdTimerQueue; virtual;
    procedure SetTimer(Value: TIdTimerQueue); virtual;
  public
    constructor Create(Connection: TIdThreadableTcpClient); reintroduce; virtual;

    procedure Terminate; override;

    property Client: TIdThreadableTcpClient read fClient;
    property Timer:  TIdTimerQueue          read GetTimer write SetTimer;
  end;

const
  FiveSeconds = 5000; // milliseconds

procedure StoreBindings(C: TIdTCPConnection; TransportType: String; Dest: TIdConnectionBindings);

implementation

uses
  IdIndyUtils, RuntimeSafety;

//******************************************************************************
//* Unit Public functions/procedures                                           *
//******************************************************************************

procedure StoreBindings(C: TIdTCPConnection; TransportType: String; Dest: TIdConnectionBindings);
begin
  if C.Connected then begin
    Dest.LocalIP   := C.Socket.Binding.IP;
    Dest.LocalPort := IntToPortNum(C.Socket.Binding.Port);
    Dest.PeerIP    := C.Socket.Binding.PeerIP;
    Dest.PeerPort  := IntToPortNum(C.Socket.Binding.PeerPort);
    Dest.Transport := TransportType;
  end;
end;

//******************************************************************************
//* TIdThreadableTcpClient                                                     *
//******************************************************************************
//* TIdThreadableTcpClient Public methods **************************************

constructor TIdThreadableTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.ConserveConnections := true;
  Self.fCachedBindings     := TIdConnectionBindings.Create;
  Self.OnDisconnected      := Self.MarkAsTerminated;
  Self.ReadTimeout         := Self.DefaultTimeout;
  Self.Terminated          := false;
end;

destructor TIdThreadableTcpClient.Destroy;
begin
  Self.fCachedBindings.Free;

  inherited Destroy;
end;

procedure TIdThreadableTcpClient.AfterConstruction;
begin
  inherited AfterConstruction;

  Self.fID := TIdObjectRegistry.Singleton.RegisterObject(Self);
end;

procedure TIdThreadableTcpClient.BeforeDestruction;
begin
  TIdObjectRegistry.Singleton.UnregisterObject(Self.ID);

  inherited BeforeDestruction;
end;

procedure TIdThreadableTcpClient.Connect(const Timeout: Integer);
begin
  inherited Connect(Timeout);

  KeepAliveSocket(Self, Self.ConserveConnections);
  StoreBindings(Self, Self.Protocol, Self.CachedBindings);
end;

function TIdThreadableTcpClient.Protocol: String;
begin
  // What's the transport protocol represented this instance/class?

  Result := TcpTransport;
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

procedure TIdThreadableTcpClient.DoOnIdle(C: TIdThreadableTcpClient);
begin
  if Assigned(Self.fOnIdle) then
    Self.fOnIdle(C);
end;

function TIdThreadableTcpClient.GetTimer: TIdTimerQueue;
begin
  Result := nil;
  RaiseAbstractError(Self.ClassName, 'GetTimer');
end;

procedure TIdThreadableTcpClient.ReceiveMessage(Msg: String; ReceivedOn: TIdConnectionBindings);
begin
  if Assigned(Self.fOnReceiveMessage) then
    Self.fOnReceiveMessage(Self, Msg, ReceivedOn);
end;

procedure TIdThreadableTcpClient.ReceiveMessage(Msg: TStream; ReceivedOn: TIdConnectionBindings);
var
  S: TStringStream;
begin
  if not Assigned(Self.fOnReceiveMessage) then Exit;

  S := TStringStream.Create('');
  try
    S.CopyFrom(Msg, 0);
    Self.fOnReceiveMessage(Self, S.DataString, ReceivedOn);
  finally
    S.Free;
  end;
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

procedure TIdThreadedTcpClient.SetTimer(Value: TIdTimerQueue);
begin
  RaiseAbstractError(Self.ClassName, 'SetTimer');
end;

end.
