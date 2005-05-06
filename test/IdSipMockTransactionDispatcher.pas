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
  Classes, IdSipDialog, IdSipLocator, IdSipMessage, IdSipMockLocator,
  IdSipMockTransport, IdSipTransaction, IdSipTransport, IdTimerQueue;

type
  TIdSipMockTransactionDispatcher = class(TIdSipTransactionDispatcher)
  private
    fTransportType: String;
    MockTransports: TStrings;

    function GetDebugTimer: TIdDebugTimerQueue;
    function GetMockLocator: TIdSipMockLocator;
    function GetTransport: TIdSipMockTransport;
    function TransportAt(Index: Integer): TIdSipMockTransport;
  public
    constructor Create; reintroduce;
    destructor  Destroy; override;

    procedure AddTransportSendingListener(Listener: IIdSipTransportSendingListener);
    procedure SendToTransport(Request: TIdSipRequest;
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
  Tran:           TIdSipTransport;
begin
  inherited Create(TIdDebugTimerQueue.Create(false), TIdSipMockLocator.Create);

  Self.DebugTimer.FireImmediateEvents := true;

  TIdSipTransportRegistry.RegisterTransport(TcpTransport, TIdSipMockTcpTransport);
  TIdSipTransportRegistry.RegisterTransport(TlsTransport, TIdSipMockTlsTransport);
  TIdSipTransportRegistry.RegisterTransport(UdpTransport, TIdSipMockUdpTransport);

  Self.MockTransports := TStringList.Create;
  Self.TransportType := UdpTransport;

  SupportedTrans := TStringList.Create;
  try
    TIdSipTransportRegistry.SecureTransports(SupportedTrans);
    TIdSipTransportRegistry.InSecureTransports(SupportedTrans);

    for I := 0 to SupportedTrans.Count - 1 do begin
      Tran := TIdSipTransportRegistry.TransportFor(SupportedTrans[I]).Create;
      Self.AddTransport(Tran);
      Self.MockTransports.AddObject(SupportedTrans[I],
                                Tran);
    end;
  finally
    SupportedTrans.Free;
  end;
end;

destructor TIdSipMockTransactionDispatcher.Destroy;
begin
  Self.MockTransports.Free;
  Self.Timer.Terminate;
  Self.Locator.Free;

  TIdSipTransportRegistry.UnregisterTransport(TcpTransport);
  TIdSipTransportRegistry.UnregisterTransport(TlsTransport);
  TIdSipTransportRegistry.UnregisterTransport(UdpTransport);

  inherited Destroy;
end;

procedure TIdSipMockTransactionDispatcher.AddTransportSendingListener(Listener: IIdSipTransportSendingListener);
var
  I: Integer;
begin
  for I := 0 to Self.MockTransports.Count - 1 do
    Self.TransportAt(I).AddTransportSendingListener(Listener);
end;

procedure TIdSipMockTransactionDispatcher.SendToTransport(Request: TIdSipRequest;
                                                          Dest: TIdSipLocation);
begin
  Self.Transport.Send(Request, Dest);
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
  Index := Self.MockTransports.IndexOf(Self.TransportType);

  if (Index <> -1) then
     Result := Self.TransportAt(Index)
  else
    raise Exception.Create('TIdSipMockTransactionDispatcher doesn''t know '
                         + 'about transport ''' + Self.TransportType + '''');
end;

function TIdSipMockTransactionDispatcher.TransportAt(Index: Integer): TIdSipMockTransport;
begin
  Result := Self.MockTransports.Objects[Index] as TIdSipMockTransport
end;

end.
