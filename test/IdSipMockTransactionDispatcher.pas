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

    procedure FireOnNewDialog(const Dialog: TIdSipDialog);
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

procedure TIdSipMockTransactionDispatcher.FireOnNewDialog(const Dialog: TIdSipDialog);
begin
  Self.DoOnNewDialog(Dialog);
end;

procedure TIdSipMockTransactionDispatcher.Send(const Msg: TIdSipMessage);
begin
  Self.Transport.Send(Msg);
//  Self.Transport.FireOnRequest(R); // ??
end;

end.
