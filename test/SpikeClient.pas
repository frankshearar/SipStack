{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit SpikeClient;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdSipCore, IdSipTransaction,
  IdSipTransport, StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Invite: TButton;
    Bye: TButton;
    Target: TEdit;
  private
    Dispatch: TIdSipTransactionDispatcher;
    Tran:     TIdSipTransport;
    UA:       TIdSipUserAgentCore;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  IdSipConsts;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Tran := TIdSipUDPTransport.Create(IdPORT_SIP);

  Self.Dispatch := TIdSipTransactionDispatcher.Create;
  Self.Dispatch.AddTransport(Self.Tran);

  Self.UA := TIdSipUserAgentCore.Create;
  Self.UA.Dispatcher := Self.Dispatch;

  Self.Tran.Start;
end;

destructor TForm1.Destroy;
begin
  Self.Tran.Stop;

  Self.UA.Free;
  Self.Dispatch.Free;
  Self.Tran.Free;

  inherited Destroy;
end;

end.
