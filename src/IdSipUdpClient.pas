{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
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
                              const ReceivedFrom: TIdSipConnectionBindings); overload; override;
  public
    property OnFinished: TIdSipClientEvent read fOnFinished write fOnFinished;
  end;

implementation

//******************************************************************************
//* TIdSipUdpClient                                                            *
//******************************************************************************
//* TIdSipUdpClient Protected methods ******************************************

procedure TIdSipUdpClient.NotifyListeners(const Response: TIdSipResponse;
                                          const ReceivedFrom: TIdSipConnectionBindings);
begin
  inherited NotifyListeners(Response, ReceivedFrom);

  if Response.IsFinal then Self.DoOnFinished;
end;

//* TIdSipUdpClient Private methods ********************************************

procedure TIdSipUdpClient.DoOnFinished;
begin
  if Assigned(Self.OnFinished)
    then Self.OnFinished(Self);
end;

end.
