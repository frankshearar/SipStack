unit IdSipMockTransactionDispatcher;

interface

uses
  IdSipDialog, IdSipMessage, IdSipMockTransport, IdSipTransaction;

type
  TIdSipMockTransactionDispatcher = class(TIdSipTransactionDispatcher)
  private
    fTransport: TIdSipMockTransport;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Send(Msg: TIdSipMessage); override;

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

  Self.fTransport := TIdSipMockTransport.Create(IdPORT_SIP);
  Self.fTransport.HostName := 'mocktransport';

  Self.AddTransport(Self.fTransport);
end;

destructor TIdSipMockTransactionDispatcher.Destroy;
begin
  Self.Transport.Free;

  inherited Destroy;
end;

procedure TIdSipMockTransactionDispatcher.Send(Msg: TIdSipMessage);
begin
  Self.Transport.Send(Msg);
end;

end.
