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
  IdSipCore, IdSipMessage, IdSipTransaction, IdSipTransport;

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
    procedure RejectRequest(Reaction: TIdSipUserAgentReaction;
                            Request: TIdSipRequest); override;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;
    function  WillAcceptResponse(Response: TIdSipResponse): TIdSipUserAgentReaction; override;
  public
    function  CreateRequest(Dest: TIdSipAddressHeader): TIdSipRequest; override;
    function  CreateResponse(Request: TIdSipRequest;
                             ResponseCode: Cardinal): TIdSipResponse; override;
    procedure Reset;

    property ReceiveRequestCalled:  Boolean read fReceiveRequestCalled;
    property ReceiveResponseCalled: Boolean read fReceiveResponseCalled;
  end;

  TIdSipMockSession = class(TIdSipInboundSession)
  private
    fIsInboundCall:  Boolean;
    fResponseResent: Boolean;
  public
    constructor Create(UA: TIdSipAbstractUserAgent;
                       Invite: TIdSipRequest;
                       UsingSecureTransport: Boolean); override;

    function  IsInboundCall: Boolean; override;
    procedure SetIsInboundCall(Value: Boolean);

    property ResponseResent: Boolean read fResponseResent;
  end;

implementation

//******************************************************************************
//* TIdSipMockCore                                                             *
//******************************************************************************
//* TIdSipMockCore Public methods **********************************************

function TIdSipMockCore.CreateRequest(Dest: TIdSipAddressHeader): TIdSipRequest;
var
  UA: TIdSipAbstractUserAgent;
begin
  UA := TIdSipAbstractUserAgent.Create;
  try
    Result := UA.CreateRequest(Dest);
  finally
    UA.Free;
  end;
end;

function TIdSipMockCore.CreateResponse(Request: TIdSipRequest;
                                       ResponseCode: Cardinal): TIdSipResponse;
begin
  Result := nil;
end;

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

procedure TIdSipMockCore.RejectRequest(Reaction: TIdSipUserAgentReaction;
                                       Request: TIdSipRequest);
begin
end;

function TIdSipMockCore.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := uarAccept;
end;

function TIdSipMockCore.WillAcceptResponse(Response: TIdSipResponse): TIdSipUserAgentReaction;
begin
  Result := uarAccept;
end;

//******************************************************************************
//* TIdSipMockSession                                                          *
//******************************************************************************
//* TIdSipMockSession Public methods *******************************************

constructor TIdSipMockSession.Create(UA: TIdSipAbstractUserAgent;
                                     Invite: TIdSipRequest;
                                     UsingSecureTransport: Boolean);
begin
  inherited Create(UA, Invite, UsingSecureTransport);

  Self.fResponseResent := false;
  Self.SetIsInboundCall(false);
end;

function TIdSipMockSession.IsInboundCall: Boolean;
begin
  Result := Self.fIsInboundCall;
end;

procedure TIdSipMockSession.SetIsInboundCall(Value: Boolean);
begin
  Self.fIsInboundCall := Value;
end;

end.
