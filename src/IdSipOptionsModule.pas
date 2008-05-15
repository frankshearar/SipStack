{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipOptionsModule;

interface

uses
  IdConnectionBindings, IdNotification, IdSipCore, IdSipMessage;

type
  TIdSipOutboundOptions = class;

  IIdSipOptionsListener = interface(IIdSipActionListener)
    ['{3F2ED4DF-4854-4255-B156-F4581AEAEDA3}']
    procedure OnResponse(OptionsAgent: TIdSipOutboundOptions;
                         Response: TIdSipResponse);
  end;

  TIdSipOptionsModule = class(TIdSipMessageModule)
  protected
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;
  public
    constructor Create(UA: TIdSipAbstractCore); override;

    function Accept(Request: TIdSipRequest;
                    Binding: TIdConnectionBindings): TIdSipAction; override;
    function AcceptsMethods: String; override;
    function CreateOptions(From: TIdSipAddressHeader;
                           Dest: TIdSipAddressHeader;
                           MaxForwards: Cardinal): TIdSipRequest;
    function QueryOptions(Server: TIdSipAddressHeader): TIdSipOutboundOptions;
  end;

  TIdSipOptions = class(TIdSipAction)
  protected
    Module: TIdSipOptionsModule;

    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         Binding: TIdConnectionBindings); override;
  public
    function IsOptions: Boolean; override;
    function Method: String; override;
  end;

  TIdSipInboundOptions = class(TIdSipOptions)
  public
    function  IsInbound: Boolean; override;
    procedure ReceiveRequest(Options: TIdSipRequest;
                             Binding: TIdConnectionBindings); override;
  end;

  TIdSipOutboundOptions = class(TIdSipOptions)
  private
    fServer:          TIdSipAddressHeader;
    OptionsListeners: TIdNotificationList;

    procedure NotifyOfResponse(Response: TIdSipResponse);
    procedure SetServer(Value: TIdSipAddressHeader);
  protected
    procedure ActionSucceeded(Response: TIdSipResponse); override;
    function  CreateNewAttempt: TIdSipRequest; override;
    procedure Initialise(UA: TIdSipAbstractCore;
                         Request: TIdSipRequest;
                         Binding: TIdConnectionBindings); override;
    procedure NotifyOfFailure(Response: TIdSipResponse); override;
  public
    destructor Destroy; override;

    procedure AddListener(const Listener: IIdSipOptionsListener);
    procedure RemoveListener(const Listener: IIdSipOptionsListener);

    property Server: TIdSipAddressHeader read fServer write SetServer;
  end;

  TIdSipOptionsResponseMethod = class(TIdNotification)
  private
    fOptions:  TIdSipOutboundOptions;
    fResponse: TIdSipResponse;
  public
    procedure Run(const Subject: IInterface); override;

    property Options:  TIdSipOutboundOptions read fOptions write fOptions;
    property Response: TIdSipResponse        read fResponse write fResponse;
  end;

implementation

uses
  IdSdp, IdSipInviteModule, SysUtils;

//******************************************************************************
//* TIdSipOptionsModule                                                        *
//******************************************************************************
//* TIdSipOptionsModule Public methods *****************************************

constructor TIdSipOptionsModule.Create(UA: TIdSipAbstractCore);
begin
  inherited Create(UA);;

  Self.AcceptsMethodsList.Add(MethodOptions);
  Self.AllowedContentTypeList.Add(SdpMimeType);
end;

function TIdSipOptionsModule.Accept(Request: TIdSipRequest;
                                    Binding: TIdConnectionBindings): TIdSipAction;
begin
  Result := inherited Accept(Request, Binding);

  if not Assigned(Result) then
    Result := TIdSipInboundOptions.CreateInbound(Self.UserAgent,
                                                 Request,
                                                 Binding);
end;

function TIdSipOptionsModule.AcceptsMethods: String;
begin
  Result := MethodOptions;
end;

function TIdSipOptionsModule.CreateOptions(From: TIdSipAddressHeader;
                                           Dest: TIdSipAddressHeader;
                                           MaxForwards: Cardinal): TIdSipRequest;
begin
  Result := Self.UserAgent.CreateRequest(MethodOptions, From, Dest, MaxForwards);
  try
    Result.AddHeader(AcceptHeader).Value := Self.UserAgent.AllowedContentTypes;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipOptionsModule.QueryOptions(Server: TIdSipAddressHeader): TIdSipOutboundOptions;
begin
  Result := Self.UserAgent.AddOutboundAction(TIdSipOutboundOptions) as TIdSipOutboundOptions;
  Result.Server := Server;
end;

//* TIdSipOptionsModule Protected methods **************************************

function TIdSipOptionsModule.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := inherited WillAcceptRequest(Request);
end;

//******************************************************************************
//* TIdSipOptions                                                              *
//******************************************************************************
//* TIdSipOptions Public methods ***********************************************

function TIdSipOptions.IsOptions: Boolean;
begin
  Result := true;
end;

function TIdSipOptions.Method: String;
begin
  Result := MethodOptions;
end;

//* TIdSipOptions Protected methods ********************************************

function TIdSipOptions.CreateNewAttempt: TIdSipRequest;
var
  TempTo: TIdSipToHeader;
begin
  TempTo := TIdSipToHeader.Create;
  try
    TempTo.Address := Self.InitialRequest.RequestUri;

    Result := Self.Module.CreateOptions(Self.LocalParty, TempTo, Self.MaxForwards);
  finally
    TempTo.Free;
  end;
end;

procedure TIdSipOptions.Initialise(UA: TIdSipAbstractCore;
                                   Request: TIdSipRequest;
                                   Binding: TIdConnectionBindings);
begin
  inherited Initialise(UA, Request, Binding);

  Self.Module := Self.UA.ModuleFor(Self.Method) as TIdSipOptionsModule;
end;

//******************************************************************************
//* TIdSipInboundOptions                                                       *
//******************************************************************************
//* TIdSipInboundOptions Public methods ****************************************

function TIdSipInboundOptions.IsInbound: Boolean;
begin
  Result := true;
end;

procedure TIdSipInboundOptions.ReceiveRequest(Options: TIdSipRequest;
                                              Binding: TIdConnectionBindings);
var
  Response: TIdSipResponse;
begin
  Assert(Options.IsOptions, 'TIdSipAction.ReceiveOptions must only receive OPTIONSes');

  Response := Self.UA.CreateResponse(Options,
                                     Self.UA.ResponseForInvite,
                                     Self.LocalGruu);
  try
    Response.Accept.Value := Self.UA.AllowedContentTypes;
    Response.Allow.Value  := Self.UA.KnownMethods;

    if not Response.HasHeader(AcceptEncodingHeader) then
      Response.AddHeader(AcceptEncodingHeader).Value := Self.UA.AllowedEncodings;
    if not Response.HasHeader(AcceptLanguageHeader) then
      Response.AddHeader(AcceptLanguageHeader).Value := Self.UA.AllowedLanguages;

    Response.Supported.Value := Self.UA.AllowedExtensions;

    // For OPTIONS "traceroute"-like functionality. cf RFC 3261, section 11.2
    Response.FirstWarning.Code  := WarningMisc;
    Response.FirstWarning.Agent := Binding.LocalIP;
    // This should contain the IP of the transport that received the OPTIONS.
    Response.FirstWarning.Text  := '';

    Self.SendResponse(Response);
    Self.LocalGruu := Response.FirstContact;
  finally
    Response.Free;
  end;

  Self.Terminate;
end;

//******************************************************************************
//* TIdSipOutboundOptions                                                      *
//******************************************************************************
//* TIdSipOutboundOptions Public methods ***************************************

destructor TIdSipOutboundOptions.Destroy;
begin
  Self.fServer.Free;
  Self.OptionsListeners.Free;

  inherited Destroy;
end;

procedure TIdSipOutboundOptions.AddListener(const Listener: IIdSipOptionsListener);
begin
  Self.OptionsListeners.AddListener(Listener);
end;

procedure TIdSipOutboundOptions.RemoveListener(const Listener: IIdSipOptionsListener);
begin
  Self.OptionsListeners.RemoveListener(Listener);
end;

//* TIdSipOutboundOptions Protected methods ************************************

procedure TIdSipOutboundOptions.ActionSucceeded(Response: TIdSipResponse);
begin
  Self.NotifyOfResponse(Response);
end;

function TIdSipOutboundOptions.CreateNewAttempt: TIdSipRequest;
begin
  Result := Self.Module.CreateOptions(Self.LocalParty, Self.Server, Self.MaxForwards);
end;

procedure TIdSipOutboundOptions.Initialise(UA: TIdSipAbstractCore;
                                           Request: TIdSipRequest;
                                           Binding: TIdConnectionBindings);
begin
  inherited Initialise(UA, Request, Binding);

  Self.OptionsListeners := TIdNotificationList.Create;

  Self.fServer := TIdSipAddressHeader.Create;
end;

procedure TIdSipOutboundOptions.NotifyOfFailure(Response: TIdSipResponse);
begin
  Self.NotifyOfResponse(Response);
end;

//* TIdSipOutboundOptions Private methods **************************************

procedure TIdSipOutboundOptions.NotifyOfResponse(Response: TIdSipResponse);
var
  Notification: TIdSipOptionsResponseMethod;
begin
  Notification := TIdSipOptionsResponseMethod.Create;
  try
    Notification.Options  := Self;
    Notification.Response := Response;

    Self.OptionsListeners.Notify(Notification);
  finally
    Notification.Free;
  end;

  Self.Terminate;
end;

procedure TIdSipOutboundOptions.SetServer(Value: TIdSipAddressHeader);
begin
  Self.fServer.Assign(Value);
end;

//******************************************************************************
//* TIdSipOptionsResponseMethod                                                *
//******************************************************************************
//* TIdSipOptionsResponseMethod Public methods *********************************

procedure TIdSipOptionsResponseMethod.Run(const Subject: IInterface);
begin
  (Subject as IIdSipOptionsListener).OnResponse(Self.Options,
                                               Self.Response);
end;

end.
