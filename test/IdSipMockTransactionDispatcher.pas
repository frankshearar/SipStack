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
  IdSipMockTransport, IdSipTransaction, IdSipTransport;

type
  TIdSipMockTransactionDispatcher = class(TIdSipTransactionDispatcher)
  private
    fTransportType: String;
    Transports:     TStrings;

    function GetTransport: TIdSipMockTransport;
    function TransportAt(Index: Integer): TIdSipMockTransport;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddTransportSendingListener(Listener: IIdSipTransportSendingListener);
    procedure SendToTransport(Request: TIdSipRequest;
                              Dest: TIdSipLocation); override;
    procedure SendToTransport(Response: TIdSipResponse;
                              Dests: TIdSipLocations); override;

    property Transport:     TIdSipMockTransport read GetTransport;
    property TransportType: String              read fTransportType write fTransportType;
  end;

implementation

uses
  IdSipConsts, SysUtils;

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
  inherited Create;

  TIdSipTransportRegistry.RegisterTransport(TcpTransport, TIdSipMockTcpTransport);
  TIdSipTransportRegistry.RegisterTransport(TlsTransport, TIdSipMockTlsTransport);
  TIdSipTransportRegistry.RegisterTransport(UdpTransport, TIdSipMockUdpTransport);

  Self.Locator := TIdSipMockLocator.Create;
  Self.Transports := TStringList.Create;
  Self.TransportType := UdpTransport;

  SupportedTrans := TStringList.Create;
  try
    TIdSipTransportRegistry.SecureTransports(SupportedTrans);
    TIdSipTransportRegistry.InSecureTransports(SupportedTrans);

    for I := 0 to SupportedTrans.Count - 1 do begin
      Tran := TIdSipTransportRegistry.TransportFor(SupportedTrans[I]).Create;
      Self.AddTransport(Tran);
      Self.Transports.AddObject(SupportedTrans[I],
                                Tran);
    end;
  finally
    SupportedTrans.Free;
  end;
end;

destructor TIdSipMockTransactionDispatcher.Destroy;
var
  I: Integer;
begin
  for I := 0 to Self.Transports.Count - 1 do
    Self.Transports.Objects[I].Free;

  Self.Transports.Free;
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
  for I := 0 to Self.Transports.Count - 1 do
    Self.TransportAt(I).AddTransportSendingListener(Listener);
end;

procedure TIdSipMockTransactionDispatcher.SendToTransport(Request: TIdSipRequest;
                                                          Dest: TIdSipLocation);
begin
  Self.Transport.Send(Request);
end;

procedure TIdSipMockTransactionDispatcher.SendToTransport(Response: TIdSipResponse;
                                                          Dests: TIdSipLocations);
begin
  Self.Transport.Send(Response);
end;

//* TIdSipMockTransactionDispatcher Private methods ****************************

function TIdSipMockTransactionDispatcher.GetTransport: TIdSipMockTransport;
var
  Index: Integer;
begin
  Index := Self.Transports.IndexOf(Self.TransportType);

  if (Index <> -1) then
     Result := Self.TransportAt(Index)
  else
    raise Exception.Create('TIdSipMockTransactionDispatcher doesn''t know '
                         + 'about transport ''' + Self.TransportType + '''');
end;

function TIdSipMockTransactionDispatcher.TransportAt(Index: Integer): TIdSipMockTransport;
begin
  Result := Self.Transports.Objects[Index] as TIdSipMockTransport
end;

end.
