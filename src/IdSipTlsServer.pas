unit IdSipTlsServer;

interface

uses
  Classes, IdSSLOpenSSL, IdSipTcpClient, IdSipTcpServer, SysUtils;

type
  TIdSipTlsServer = class(TIdSipTcpServer)
  public
    function  CreateClient: TIdSipTcpClient; override;
    procedure DestroyClient(Client: TIdSipTcpClient); override;
  end;

implementation

uses
  IdSipConsts;

//******************************************************************************
//* TIdSipTlsServer                                                            *
//******************************************************************************
//* TIdSipTlsServer Public methods *********************************************

function TIdSipTlsServer.CreateClient: TIdSipTcpClient;
begin
  Result := inherited CreateClient;

  Result.IOHandler := TIdSSLIOHandlerSocket.Create(nil);
end;

procedure TIdSipTlsServer.DestroyClient(Client: TIdSipTcpClient);
begin
  Client.IOHandler.Free;

  inherited DestroyClient(Client);
end;

end.
