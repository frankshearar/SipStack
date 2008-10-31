{
  (c) 2008 Technology Directorate, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestFrameworkSdp;

interface

uses
  IdSdp, TestFrameworkRtp;

type
  TMockMediaStreamFactory = class(TIdSdpMediaStreamFactory)
  public
    constructor Create; override;
  end;

implementation

//******************************************************************************
//* TMockMediaStreamFactory                                                    *
//******************************************************************************
//* TMockMediaStreamFactory Public methods *************************************

constructor TMockMediaStreamFactory.Create;
begin
  inherited Create;

  Self.fRtpServerType := TIdMockRTPPeer;
  Self.fTcpServerType := TIdSdpMockTcpConnection;
end;

end.
