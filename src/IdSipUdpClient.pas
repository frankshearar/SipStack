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
  Classes, IdSipMessage, IdSipUdpServer;

type
  TIdSipUdpClient = class(TIdSipUdpServer)
  private
    fOnFinished: TNotifyEvent;

    procedure DoOnFinished;
  protected
    procedure NotifyListenersOfResponse(Response: TIdSipResponse;
                                        ReceivedFrom: TIdSipConnectionBindings); overload; override;
  public
    property OnFinished: TNotifyEvent read fOnFinished write fOnFinished;
  end;

implementation

//******************************************************************************
//* TIdSipUdpClient                                                            *
//******************************************************************************
//* TIdSipUdpClient Protected methods ******************************************

procedure TIdSipUdpClient.NotifyListenersOfResponse(Response: TIdSipResponse;
                                                    ReceivedFrom: TIdSipConnectionBindings);
begin
  inherited NotifyListenersOfResponse(Response, ReceivedFrom);

  if Response.IsFinal then Self.DoOnFinished;
end;

//* TIdSipUdpClient Private methods ********************************************

procedure TIdSipUdpClient.DoOnFinished;
begin
  if Assigned(Self.OnFinished)
    then Self.OnFinished(Self);
end;

end.
