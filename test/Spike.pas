unit Spike;

interface

uses
  Classes, Controls, Forms, IdSipParser, IdSipTcpServer, IdSipUdpServer,
  IdTCPServer, StdCtrls, ExtCtrls;

type
  TrnidSpike = class(TForm)
    Log: TMemo;
    ServerType: TRadioGroup;
    procedure ServerTypeClick(Sender: TObject);
  private
    TcpServer: TIdSipTcpServer;
    UdpServer: TIdSipUdpServer;

    procedure OnMethod(AThread: TIdPeerThread;
                       AMessage: TIdSipMessage);
    procedure OnRequest(Sender: TObject; const Request: TIdSipRequest);
    procedure OnResponse(Sender: TObject; const Response: TIdSipResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  rnidSpike: TrnidSpike;

implementation

{$R *.dfm}

//******************************************************************************
//* TrnidSpike                                                                 *
//******************************************************************************
//* TrnidSpike Public methods **************************************************

constructor TrnidSpike.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.UdpServer := TIdSipUdpServer.Create(nil);

  Self.UdpServer.OnRequest  := Self.OnRequest;
  Self.UdpServer.OnResponse := Self.OnResponse;
  Self.UdpServer.Active     := true;

  Self.TcpServer := TIdSipTcpServer.Create(nil);
  Self.TcpServer.OnMethod := Self.OnMethod;
end;

destructor TrnidSpike.Destroy;
begin
  TcpServer.Free;
  UdpServer.Free;

  inherited Destroy;
end;

//* TrnidSpike Private methods *************************************************

procedure TrnidSpike.OnMethod(AThread: TIdPeerThread;
                              AMessage: TIdSipMessage);
begin
  Self.Log.Lines.Add('=== TCP message ===');
  Self.Log.Lines.Add(AMessage.AsString);
end;

procedure TrnidSpike.OnRequest(Sender: TObject; const Request: TIdSipRequest);
begin
  Self.Log.Lines.Add('=== UDP Request ===');
  Self.Log.Lines.Add(Request.AsString);
end;

procedure TrnidSpike.OnResponse(Sender: TObject; const Response: TIdSipResponse);
begin
  Self.Log.Lines.Add('=== UDP Response ===');
  Self.Log.Lines.Add(Response.AsString);
end;

procedure TrnidSpike.ServerTypeClick(Sender: TObject);
begin
  case ServerType.ItemIndex of
    0: begin
         Self.UdpServer.Active := false;
         Self.TcpServer.Active := true;
       end;
    1: begin
         Self.UdpServer.Active := true;
         Self.TcpServer.Active := false;
       end;
  end;
end;

end.
