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
  IdSipDialog, IdSipLocator, IdSipMessage, IdSipMockTransport,
  IdSipTransaction;

type
  TIdSipMockTransactionDispatcher = class(TIdSipTransactionDispatcher)
  private
    fTransport: TIdSipMockTransport;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure SendToTransport(Request: TIdSipRequest;
                              Dest: TIdSipLocation); override;
    procedure SendToTransport(Response: TIdSipResponse); override;

    property Transport: TIdSipMockTransport read fTransport;
  end;

implementation

uses
  IdSipConsts;

//******************************************************************************
//* TIdSipMockTransactionDispatcher                                            *
//******************************************************************************
//* TIdSipMockTransactionDispatcher Public methods *****************************

constructor TIdSipMockTransactionDispatcher.Create;
begin
  inherited Create;

  Self.fTransport := TIdSipMockTransport.Create;
  Self.fTransport.Address  := '127.0.0.1';
  Self.fTransport.HostName := 'mocktransport';
  Self.fTransport.Port     := IdPORT_SIP;

  Self.AddTransport(Self.fTransport);
end;

destructor TIdSipMockTransactionDispatcher.Destroy;
begin
  Self.Transport.Free;

  inherited Destroy;
end;

procedure TIdSipMockTransactionDispatcher.SendToTransport(Request: TIdSipRequest;
                                                          Dest: TIdSipLocation);
begin
  Self.Transport.Send(Request);
end;

procedure TIdSipMockTransactionDispatcher.SendToTransport(Response: TIdSipResponse);
begin
  Self.Transport.Send(Response);
end;

end.
