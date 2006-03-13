{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipMockTransactionDispatcher;

interface

uses
  Classes, IdSipLocator, IdSipMessage, IdSipMockLocator, IdSipMockTransport,
  IdSipTransaction, IdSipTransport, IdTimerQueue;

type
  TIdSipMockTransactionDispatcher = class(TIdSipTransactionDispatcher)
  private
    fTransportType: String;

    function GetDebugTimer: TIdDebugTimerQueue;
    function GetMockLocator: TIdSipMockLocator;
    function GetTransport: TIdSipMockTransport;
  public
    constructor Create; reintroduce;
    destructor  Destroy; override;

    procedure AddTransportSendingListener(Listener: IIdSipTransportSendingListener);
    procedure SendToTransport(Msg: TIdSipMessage;
                              Dest: TIdSipLocation); override;
    procedure SendToTransport(Response: TIdSipResponse;
                              Dests: TIdSipLocations); override;

    property DebugTimer:    TIdDebugTimerQueue  read GetDebugTimer;
    property MockLocator:   TIdSipMockLocator   read GetMockLocator;
    property Transport:     TIdSipMockTransport read GetTransport;
    property TransportType: String              read fTransportType write fTransportType;
  end;

implementation

uses
  IdSipConsts, SysUtils;

const
  NoDestinationsForResponse = 'Couldn''t send the response because there are '
                            + 'no destinations for ''%s'': check your A/AAAA/etc '
                            + 'setup in your mock locator';

//******************************************************************************
//* TIdSipMockTransactionDispatcher                                            *
//******************************************************************************
//* TIdSipMockTransactionDispatcher Public methods *****************************

constructor TIdSipMockTransactionDispatcher.Create;
var
  I:              Integer;
  SupportedTrans: TStrings;
begin
  inherited Create(TIdDebugTimerQueue.Create(false), TIdSipMockLocator.Create);

  Self.DebugTimer.TriggerImmediateEvents := true;

  TIdSipTransportRegistry.RegisterTransportType(TcpTransport, TIdSipMockTcpTransport);
  TIdSipTransportRegistry.RegisterTransportType(TlsTransport, TIdSipMockTlsTransport);
  TIdSipTransportRegistry.RegisterTransportType(UdpTransport, TIdSipMockUdpTransport);

  Self.TransportType := UdpTransport;

  SupportedTrans := TStringList.Create;
  try
    TIdSipTransportRegistry.SecureTransports(SupportedTrans);
    TIdSipTransportRegistry.InSecureTransports(SupportedTrans);

    for I := 0 to SupportedTrans.Count - 1 do begin
      Self.AddTransportBinding(SupportedTrans[I],
                               '127.0.0.1',
                               TIdSipTransportRegistry.TransportTypeFor(SupportedTrans[I]).DefaultPort);
    end;
  finally
    SupportedTrans.Free;
  end;
end;

destructor TIdSipMockTransactionDispatcher.Destroy;
begin
  Self.Timer.Terminate;
  Self.Locator.Free;

  TIdSipTransportRegistry.UnregisterTransportType(TcpTransport);
  TIdSipTransportRegistry.UnregisterTransportType(TlsTransport);
  TIdSipTransportRegistry.UnregisterTransportType(UdpTransport);

  inherited Destroy;
end;

procedure TIdSipMockTransactionDispatcher.AddTransportSendingListener(Listener: IIdSipTransportSendingListener);
var
  I: Integer;
begin
  for I := 0 to Self.Transports.Count - 1 do
    Self.Transports[I].AddTransportSendingListener(Listener);
end;

procedure TIdSipMockTransactionDispatcher.SendToTransport(Msg: TIdSipMessage;
                                                          Dest: TIdSipLocation);
begin
  Self.Transport.Send(Msg, Dest);
end;

procedure TIdSipMockTransactionDispatcher.SendToTransport(Response: TIdSipResponse;
                                                          Dests: TIdSipLocations);
begin
  if Dests.IsEmpty then
    raise Exception.Create(Format(NoDestinationsForResponse, [Response.LastHop.SentBy]));

  Self.Transport.Send(Response, Dests[0]);
end;

//* TIdSipMockTransactionDispatcher Private methods ****************************

function TIdSipMockTransactionDispatcher.GetDebugTimer: TIdDebugTimerQueue;
begin
  Result := Self.Timer as TIdDebugTimerQueue;
end;

function TIdSipMockTransactionDispatcher.GetMockLocator: TIdSipMockLocator;
begin
  Result := Self.Locator as TIdSipMockLocator;
end;

function TIdSipMockTransactionDispatcher.GetTransport: TIdSipMockTransport;
var
  Index: Integer;
begin
  Index := 0;
  while (Index < Self.TransportCount)
    and (Self.Transports[Index].GetTransportType <> Self.TransportType) do
    Inc(Index);

  if (Index < Self.TransportCount) then
     Result := Self.Transports[Index] as TIdSipMockTransport
  else
    raise Exception.Create('TIdSipMockTransactionDispatcher doesn''t know '
                         + 'about transport ''' + Self.TransportType + '''');
end;

end.
