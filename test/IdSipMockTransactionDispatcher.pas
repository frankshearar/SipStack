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
