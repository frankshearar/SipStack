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
    function  GetTransport(const Target: TIdURI): TIdSipTransportType;
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

function TIdSipClient.GetTransport(const Target: TIdURI): TIdSipTransportType;
var
  Scheme: String;
begin
  Scheme := Target.Protocol;
  Result := sttSCTP;
end;

procedure TIdSipClient.Invite(const Target: TIdURI);
var
  Msg: TIdSipRequest;
begin
  Msg := TIdSipRequest.Create;
  try
    Msg.Method     := MethodInvite;
    Msg.SIPVersion := SIPVersion;
    Msg.Request    := Target.GetFullURI;
    Msg.Headers.Add(ToHeaderFull).Value := Msg.Request;
    Msg.Headers.Add(FromHeaderFull).Value := 'sip:i@dont.know';
    // how do we get what actual iface through which the packet was sent?
    Msg.Headers.Add(ViaHeaderFull).Value := 'SIP/2.0/UDP ' + '';
  finally
  end;
end;

//* TIdSipClient Protected methods *********************************************
//* TIdSipClient Private methods ***********************************************
//* TIdSipClient Published methods *********************************************

end.
