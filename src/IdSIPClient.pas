unit IdSIPClient;

interface

uses
  IdURI, IdSipParser;

type
  TIdSipTransport = class(TObject)
  public
    procedure SendRequest(Request: TIdSipRequest; Response: TIdSipResponse); virtual; abstract;
  end;

  TIdSipTCPTransport = class(TIdSipTransport);

  TIdSipUDPTransport = class(TIdSipTransport);

  TIdSipMockTransport = class(TIdSipTransport)
  public
    procedure SendRequest(Request: TIdSipRequest; Response: TIdSipResponse); override;
  end;

  TIdSipClient = class(TObject)
  public
    procedure Invite(const Target: TIdURI);
  end;

implementation

//******************************************************************************
//* TIdSipMockTransport                                                        *
//******************************************************************************
//* TIdSipMockTransport Public methods *****************************************

procedure TIdSipMockTransport.SendRequest(Request: TIdSipRequest; Response: TIdSipResponse);
begin
  Response.StatusCode := SIPOK;
  Response.StatusText := RSSIPOK;
  Response.SIPVersion := SIPVersion;
end;

//* TIdSipMockTransport Protected methods **************************************
//* TIdSipMockTransport Private methods ****************************************
//* TIdSipMockTransport Published methods **************************************

//******************************************************************************
//* TIdSipClient                                                               *
//******************************************************************************
//* TIdSipClient Public methods ************************************************

procedure TIdSipClient.Invite(const Target: TIdURI);
begin
end;

//* TIdSipClient Protected methods *********************************************
//* TIdSipClient Private methods ***********************************************
//* TIdSipClient Published methods *********************************************

end.
