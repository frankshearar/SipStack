unit IdSipUdpClient;

interface

uses
  IdSipMessage, IdSipTcpClient, IdSipUdpServer;

type
  TIdSipUdpClient = class(TIdSipUdpServer)
  private
    fOnFinished: TIdSipClientEvent;

    procedure DoOnFinished;
  protected
    procedure NotifyListeners(const Response: TIdSipResponse;
                              const ReceivedOn: TIdSipIPTarget); overload; override;
  public
    property OnFinished: TIdSipClientEvent read fOnFinished write fOnFinished;
  end;

implementation

//******************************************************************************
//* TIdSipUdpClient                                                            *
//******************************************************************************
//* TIdSipUdpClient Protected methods ******************************************

procedure TIdSipUdpClient.NotifyListeners(const Response: TIdSipResponse;
                                          const ReceivedOn: TIdSipIPTarget);
begin
  inherited NotifyListeners(Response, ReceivedOn);

  if Response.IsFinal then Self.DoOnFinished;
end;

//* TIdSipUdpClient Private methods ********************************************

procedure TIdSipUdpClient.DoOnFinished;
begin
  if Assigned(Self.OnFinished)
    then Self.OnFinished(Self);
end;

end.
