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
  IdSipCore, IdSipDialog, IdSipMessage;

type
  TIdSipProxy = class(TIdSipAbstractCore)
  protected
    function  AuthenticationHeader: String; override;
    function  AuthenticationStatusCode: Cardinal; override;
    function  HasAuthorization(Request: TIdSipRequest): Boolean; override;
  public
    function  CreateRequest(Dest: TIdSipAddressHeader): TIdSipRequest; overload; override;
    function  CreateRequest(Dialog: TIdSipDialog): TIdSipRequest; overload; override;
  end;

implementation

uses
  IdSipConsts, SysUtils;

//******************************************************************************
//* TIdSipProxy                                                                *
//******************************************************************************
//* TIdSipProxy Public methods *************************************************

function TIdSipProxy.CreateRequest(Dest: TIdSipAddressHeader): TIdSipRequest;
begin
  raise Exception.Create('This stack cannot proxy requests as yet');
end;

function TIdSipProxy.CreateRequest(Dialog: TIdSipDialog): TIdSipRequest;
begin
  raise Exception.Create('This stack cannot proxy requests as yet');
end;

//* TIdSipProxy Protected methods **********************************************

function TIdSipProxy.AuthenticationHeader: String;
begin
  Result := ProxyAuthenticateHeader;
end;

function TIdSipProxy.AuthenticationStatusCode: Cardinal;
begin
  Result := SIPProxyAuthenticationRequired;
end;

function TIdSipProxy.HasAuthorization(Request: TIdSipRequest): Boolean;
begin
  // Proxies and User Agent Servers use different headers to
  // challenge/authenticate.

  Result := Request.HasProxyAuthorization;
end;

end.
