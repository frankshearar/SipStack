{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipProxy;

interface

uses
  IdSipAuthentication, IdSipCore, IdSipDialog, IdSipMessage;

type
  TIdSipProxy = class(TIdSipAbstractCore)
  protected
    procedure SetAuthenticator(Value: TIdSipAbstractAuthenticator); override;
  end;

implementation

//******************************************************************************
//* TIdSipProxy                                                                *
//******************************************************************************
//* TIdSipProxy Protected methods **********************************************

procedure TIdSipProxy.SetAuthenticator(Value: TIdSipAbstractAuthenticator);
begin
  inherited SetAuthenticator(Value);

  Self.Authenticator.IsProxy := true;
end;

end.
