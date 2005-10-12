{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipMockCore;

interface

uses
  IdSipCore, IdSipInviteModule, IdSipMessage, IdSipTransaction, IdSipTransport,
  IdSipUserAgent;

type
  TIdSipMockCore = class(TIdSipAbstractCore)
  private
    fReceiveRequestCalled: Boolean;
    fReceiveResponseCalled: Boolean;
  protected
    procedure ActOnRequest(Request: TIdSipRequest;
                           Receiver: TIdSipTransport); override;
    procedure ActOnResponse(Response: TIdSipResponse;
                            Receiver: TIdSipTransport); override;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;
  public
    procedure Reset;

    property ReceiveRequestCalled:  Boolean read fReceiveRequestCalled;
    property ReceiveResponseCalled: Boolean read fReceiveResponseCalled;
  end;

  TIdSipMockSession = class(TIdSipInboundSession)
  private
    fIsInbound:  Boolean;
    fResponseResent: Boolean;
  protected
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         UsingSecureTransport: Boolean); override;
  public
    function  IsInbound: Boolean; override;
    procedure SetIsInbound(Value: Boolean);

    property ResponseResent: Boolean read fResponseResent;
  end;

implementation

//******************************************************************************
//* TIdSipMockCore                                                             *
//******************************************************************************
//* TIdSipMockCore Public methods **********************************************

procedure TIdSipMockCore.Reset;
begin
  fReceiveRequestCalled  := true;
  fReceiveResponseCalled := true;
end;

//* TIdSipMockCore Protected methods *******************************************

procedure TIdSipMockCore.ActOnRequest(Request: TIdSipRequest;
                                      Receiver: TIdSipTransport);
begin
  fReceiveRequestCalled := true;
end;

procedure TIdSipMockCore.ActOnResponse(Response: TIdSipResponse;
                                      Receiver: TIdSipTransport);
begin
  fReceiveResponseCalled := true;
end;

function TIdSipMockCore.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := uarAccept;
end;

//******************************************************************************
//* TIdSipMockSession                                                          *
//******************************************************************************
//* TIdSipMockSession Public methods *******************************************

function TIdSipMockSession.IsInbound: Boolean;
begin
  Result := Self.fIsInbound;
end;

procedure TIdSipMockSession.SetIsInbound(Value: Boolean);
begin
  Self.fIsInbound := Value;
end;

//* TIdSipMockSession Protected methods ****************************************

procedure TIdSipMockSession.Initialise(UA: TIdSipAbstractCore;
                                       Request: TIdSipRequest;
                                       UsingSecureTransport: Boolean);
begin
  inherited Initialise(UA, Request, UsingSecureTransport);

  Self.fResponseResent := false;
  Self.SetIsInbound(false);
end;

end.
