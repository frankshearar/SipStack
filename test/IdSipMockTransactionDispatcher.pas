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

    procedure FireOnTransactionFail(const Reason: String);
    procedure Send(const Msg: TIdSipMessage); override;

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

  Self.AddTransport(Self.fTransport);
end;

destructor TIdSipMockTransactionDispatcher.Destroy;
begin
  Self.Transport.Free;

  inherited Destroy;
end;

procedure TIdSipMockTransactionDispatcher.FireOnTransactionFail(const Reason: String);
begin
  Self.DoOnTransactionFail(Self, Reason);
end;

procedure TIdSipMockTransactionDispatcher.Send(const Msg: TIdSipMessage);
begin
  Self.Transport.Send(Msg);
end;

end.
