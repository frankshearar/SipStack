unit SpikeClient;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdSipCore, IdSipTransaction,
  IdSipTransport, StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Invite: TButton;
    Log: TMemo;
    procedure InviteClick(Sender: TObject);
  private
    UA:   TIdSipUserAgentCore;
    Tran: TIdSipAbstractTransport;

    procedure OnNewSession(Sender: TObject; const Session: TIdSipSession);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  IdSipConsts, IdSipHeaders, IdSipMessage, IdTcpClient, IdURI;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  UA := TIdSipUserAgentCore.Create;

  UA.From.Address.URI    := 'sip:franks@127.0.0.1';
  UA.From.Tag            := '1';
  UA.Contact.Address.URI := 'sip:franks@127.0.0.1';
  UA.HostName := '127.0.0.1';

  UA.Dispatcher := TIdSipTransactionDispatcher.Create;

  Tran := TIdSipTCPTransport.Create(IdPORT_SIP + 10000);
  UA.Dispatcher.AddTransport(Tran);
  Tran.Start;
end;

destructor TForm1.Destroy;
begin
  Tran.Free;
  UA.Dispatcher.Free;
  UA.Free;

  inherited Destroy;
end;

procedure TForm1.InviteClick(Sender: TObject);
var
  Client:   TIdTcpClient;
  Line:     String;
  Request:  TIdSipRequest;
  ToHeader: TIdSipToHeader;
begin
  ToHeader := TIdSipToHeader.Create;
  try
    ToHeader.Value := 'sip:franks@127.0.0.1';

    UA.Call(ToHeader);
  finally
    ToHeader.Free;
  end;
end;

procedure TForm1.OnNewSession(Sender: TObject; const Session: TIdSipSession);
begin
  ShowMessage('New session');
end;

end.
