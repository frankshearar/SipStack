unit Spike;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdSipCore, IdSipMessage, IdSipTransaction,
  IdSipTransport, StdCtrls;

type
  TrnidSpike = class(TForm)
    Log: TMemo;
  private
    Dispatch:  TIdSipTransactionDispatcher;
    Transport: TIdSipTcpTransport;
    UA:        TIdSipUserAgentCore;

    procedure OnInvite(Sender: TObject; const Request: TIdSipRequest);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  rnidSpike: TrnidSpike;

implementation

{$R *.dfm}

uses
  IdSipConsts;

//******************************************************************************
//* TrnidSpike                                                                 *
//******************************************************************************
//* TrnidSpike Public methods **************************************************

constructor TrnidSpike.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Transport := TIdSipTcpTransport.Create(IdPORT_SIP);
  Self.Dispatch := TIdSipTransactionDispatcher.Create;
  Self.Dispatch.AddTransport(Self.Transport);

  Self.UA := TIdSipUserAgentCore.Create;
  Self.UA.Dispatcher := Self.Dispatch;

  Self.UA.OnInvite := Self.OnInvite;

  Self.Transport.Start;
end;

destructor TrnidSpike.Destroy;
begin
  Self.Transport.Stop;

  Self.UA.Free;
  Self.Dispatch.Free;
  Self.Transport.Free;

  inherited Destroy;
end;

//* TrnidSpike Private methods *************************************************

procedure TrnidSpike.OnInvite(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.Log.Text := Self.Log.Text + Request.AsString;

  Self.UA.AcceptCall(Request);
end;

end.
