unit Spike;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdSipCore, IdSipMessage, IdSipTransaction,
  IdSipTransport, StdCtrls;

type
  TrnidSpike = class(TForm)
    Log: TMemo;
    Panel1: TPanel;
    InviteSelf: TButton;
    procedure InviteSelfClick(Sender: TObject);
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
  IdSipConsts, IdSipHeaders;

//******************************************************************************
//* TrnidSpike                                                                 *
//******************************************************************************
//* TrnidSpike Public methods **************************************************

constructor TrnidSpike.Create(AOwner: TComponent);
var
  Contact: TIdSipContactHeader;
  From:    TIdSipFromHeader;
begin
  inherited Create(AOwner);

  Self.Transport := TIdSipTcpTransport.Create(IdPORT_SIP);
  Self.Transport.HostName := 'wsfrank';
  Self.Dispatch := TIdSipTransactionDispatcher.Create;
  Self.Dispatch.AddTransport(Self.Transport);

  Self.UA := TIdSipUserAgentCore.Create;
  Self.UA.Dispatcher := Self.Dispatch;

  Contact := TIdSipContactHeader.Create;
  try
    Contact.Value := 'sip:franks@wsfrank';
    Self.UA.Contact := Contact;
  finally
    Contact.Free;
  end;

  From := TIdSipFromHeader.Create;
  try
    From.Value := 'sip:franks@wsfrank';
    Self.UA.From := From;
  finally
    From.Free;
  end;

//  Self.UA.OnInvite := Self.OnInvite;

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

//  Self.UA.AcceptCall(Request, Self.Transport);
end;

procedure TrnidSpike.InviteSelfClick(Sender: TObject);
var
  Local: TIdSipToHeader;
begin
  Local := TIdSipToHeader.Create;
  try
    Local.Value := 'Frank <sip:franks@wsfrank>';
    Self.UA.Call(Local);
  finally
    Local.Free;
  end;
end;

end.
