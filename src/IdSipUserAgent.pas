{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipUserAgent;

interface

uses
  Contnrs, Classes, IdNotification, IdSipAuthentication, IdSipCore,
  IdSipInviteModule, IdSipMessage, IdSipRegistration, IdSipTransaction,
  IdSipTransport, IdTimerQueue;

type
  TIdSipUserAgent = class;

  IIdSipUserAgentListener = interface(IIdSipTransactionUserListener)
    ['{E365D17F-054B-41AB-BB18-0C339715BFA3}']
  end;

  TIdSipUserAgent = class(TIdSipAbstractCore,
                          IIdSipInviteModuleListener)
  private
    fDoNotDisturb:        Boolean;
    fDoNotDisturbMessage: String;
    fHasProxy:            Boolean;
    fProxy:               TIdSipUri;
    fRegisterModule:      TIdSipOutboundRegisterModule;
    fInviteModule:        TIdSipInviteModule;
    UserAgentListeners:   TIdNotificationList;

    function  GetInitialResendInterval: Cardinal;
    function  GetProgressResendInterval: Cardinal;
    procedure OnInboundCall(UserAgent: TIdSipAbstractCore;
                            Session: TIdSipInboundSession);
    procedure SetInitialResendInterval(Value: Cardinal);
    procedure SetProgressResendInterval(Value: Cardinal);
    procedure SetProxy(Value: TIdSipUri);
  protected
    procedure NotifyOfDroppedMessage(Message: TIdSipMessage;
                                     Receiver: TIdSipTransport); override;
    function  WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddLocalHeaders(OutboundRequest: TIdSipRequest); override;
    procedure AddTransportListener(Listener: IIdSipTransportListener);
    procedure AddUserAgentListener(const Listener: IIdSipUserAgentListener);
    procedure RemoveTransportListener(Listener: IIdSipTransportListener);
    procedure RemoveUserAgentListener(const Listener: IIdSipUserAgentListener);
    function  ResponseForInvite: Cardinal; override;
    function  SessionCount: Integer;

    property DoNotDisturb:           Boolean                      read fDoNotDisturb write fDoNotDisturb;
    property DoNotDisturbMessage:    String                       read fDoNotDisturbMessage write fDoNotDisturbMessage;
    property HasProxy:               Boolean                      read fHasProxy write fHasProxy;
    property InitialResendInterval:  Cardinal                     read GetInitialResendInterval write SetInitialResendInterval;
    property InviteModule:           TIdSipInviteModule           read fInviteModule;
    property ProgressResendInterval: Cardinal                     read GetProgressResendInterval write SetProgressResendInterval;
    property Proxy:                  TIdSipUri                    read fProxy write SetProxy;
    property RegisterModule:         TIdSipOutboundRegisterModule read fRegisterModule;
  end;

  // Given a configuration file, I create a stack.
  // The configuration file consists of lines. Each line is a complete and
  // independent setting consisting of a Directive, at least one space, and the
  // settings for that Directive.
  //
  // Currently we support the following Directives: Listen, NameServer,
  // Register.
  //
  // Here's a summary of the formats for each directive:
  //   NameServer: <domain name or IP>:<port>
  //   NameServer: MOCK
  //   Listen: <transport name><SP><host|IPv4 address|IPv6 reference|AUTO>:<port>
  //   Register: <SIP/S URI>
  //   Proxy: <SIP/S URI>
  //   From: "Count Zero" <sip:countzero@jammer.org>
  //   Contact: sip:wintermute@tessier-ashpool.co.luna
  //   SupportEvent: refer
  TIdSipStackConfigurator = class(TObject)
  private
    procedure AddAuthentication(UserAgent: TIdSipAbstractCore;
                                const AuthenticationLine: String);
    procedure AddAutoContact(UserAgent: TIdSipAbstractCore);
    procedure AddContact(UserAgent: TIdSipAbstractCore;
                      const ContactLine: String);
    procedure AddFrom(UserAgent: TIdSipAbstractCore;
                      const FromLine: String);
    procedure AddLocator(UserAgent: TIdSipAbstractCore;
                         const NameServerLine: String);
    procedure AddProxy(UserAgent: TIdSipUserAgent;
                       const ProxyLine: String);
    procedure AddSupportForEventPackage(UserAgent: TIdSipAbstractCore;
                                        const SupportEventLine: String);
    procedure AddTransport(Dispatcher: TIdSipTransactionDispatcher;
                           const TransportLine: String);
    procedure CheckUri(Uri: TIdSipUri;
                       const FailMsg: String);
    function  CreateLayers(Context: TIdTimerQueue): TIdSipUserAgent;
    procedure EatDirective(var Line: String);
    procedure InstantiateMissingObjectsAsDefaults(UserAgent: TIdSipAbstractCore);
    procedure ParseFile(UserAgent: TIdSipUserAgent;
                        Configuration: TStrings;
                        PendingActions: TObjectList);
    procedure ParseLine(UserAgent: TIdSipUserAgent;
                        const ConfigurationLine: String;
                        PendingActions: TObjectList);
    procedure RegisterUA(UserAgent: TIdSipUserAgent;
                         const RegisterLine: String;
                         PendingActions: TObjectList);
    procedure SendPendingActions(Actions: TObjectList);
  public
    function CreateUserAgent(Configuration: TStrings;
                             Context: TIdTimerQueue): TIdSipUserAgent; overload;
  end;

implementation

uses
  IdSimpleParser, IdSipIndyLocator, IdSipMockLocator,
  IdSipSubscribeModule, IdSystem, IdUnicode, SysUtils;

//******************************************************************************
//* TIdSipUserAgent                                                            *
//******************************************************************************
//* TIdSipUserAgent Public methods *********************************************

constructor TIdSipUserAgent.Create;
begin
  inherited Create;

  Self.UserAgentListeners := TIdNotificationList.Create;
  Self.UserAgentListeners.AddExpectedException(EParserError);

  Self.fInviteModule   := Self.AddModule(TIdSipInviteModule) as TIdSipInviteModule;
  Self.fRegisterModule := Self.AddModule(TIdSipOutboundRegisterModule) as TIdSipOutboundRegisterModule;

  Self.InviteModule.AddListener(Self);

  Self.DoNotDisturb           := false;
  Self.DoNotDisturbMessage    := RSSIPTemporarilyUnavailable;
  Self.fProxy                 := TIdSipUri.Create('');
  Self.HasProxy               := false;
  Self.InitialResendInterval  := DefaultT1;
end;

destructor TIdSipUserAgent.Destroy;
begin
  // Because we create TIdSipUserAgents from a StackConfigurator factory method,
  // we must clean up certain objects to which we have references, viz.,
  // Self.Dispatcher and Self.Authenticator.
  //
  // Thus we destroy these objects AFTER the inherited Destroy, because the base
  // class could well expect these objects to still exit.

  Self.Proxy.Free;
  Self.UserAgentListeners.Free;

  inherited Destroy;

  Self.Dispatcher.Free;
  Self.Authenticator.Free;
end;

procedure TIdSipUserAgent.AddLocalHeaders(OutboundRequest: TIdSipRequest);
begin
  inherited AddLocalHeaders(OutboundRequest);

  if Self.HasProxy then
    OutboundRequest.Route.AddRoute(Self.Proxy);
end;

procedure TIdSipUserAgent.AddTransportListener(Listener: IIdSipTransportListener);
begin
  Self.Dispatcher.AddTransportListener(Listener);
end;

procedure TIdSipUserAgent.AddUserAgentListener(const Listener: IIdSipUserAgentListener);
begin
  Self.UserAgentListeners.AddListener(Listener);
end;

procedure TIdSipUserAgent.NotifyOfDroppedMessage(Message: TIdSipMessage;
                                                 Receiver: TIdSipTransport);
var
  Notification: TIdSipUserAgentDroppedUnmatchedMessageMethod;
begin
  Notification := TIdSipUserAgentDroppedUnmatchedMessageMethod.Create;
  try
    Notification.Message   := Message;
    Notification.Receiver  := Receiver;
    Notification.UserAgent := Self;

    Self.UserAgentListeners.Notify(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TIdSipUserAgent.RemoveTransportListener(Listener: IIdSipTransportListener);
begin
  Self.Dispatcher.RemoveTransportListener(Listener);
end;

procedure TIdSipUserAgent.RemoveUserAgentListener(const Listener: IIdSipUserAgentListener);
begin
  Self.UserAgentListeners.RemoveListener(Listener);
end;

function TIdSipUserAgent.ResponseForInvite: Cardinal;
begin
  // If we receive an INVITE (or an OPTIONS), what response code
  // would we return? If we don't wish to be disturbed, we return
  // SIPTemporarilyUnavailable; if we have no available lines, we
  // return SIPBusyHere, etc.

  if Self.DoNotDisturb then
    Result := SIPTemporarilyUnavailable
  else
    Result := inherited ResponseForInvite;
end;

function TIdSipUserAgent.SessionCount: Integer;
begin
  Result := Self.Actions.SessionCount;
end;

//* TIdSipUserAgent Protected methods ******************************************

function TIdSipUserAgent.WillAcceptRequest(Request: TIdSipRequest): TIdSipUserAgentReaction;
begin
  Result := inherited WillAcceptRequest(Request);

  if (Result = uarAccept) then begin
    if (Request.IsInvite or Request.IsOptions) and Self.DoNotDisturb then
      Result := uarDoNotDisturb;
  end;
end;

//* TIdSipUserAgent Private methods ********************************************

function TIdSipUserAgent.GetInitialResendInterval: Cardinal;
begin
  Result := Self.InviteModule.InitialResendInterval;
end;

function TIdSipUserAgent.GetProgressResendInterval: Cardinal;
begin
  Result := Self.InviteModule.ProgressResendInterval;
end;

procedure TIdSipUserAgent.OnInboundCall(UserAgent: TIdSipAbstractCore;
                                        Session: TIdSipInboundSession);
begin
  // For now, do nothing
end;

procedure TIdSipUserAgent.SetInitialResendInterval(Value: Cardinal);
begin
  Self.InviteModule.InitialResendInterval := Value;
end;

procedure TIdSipUserAgent.SetProgressResendInterval(Value: Cardinal);
begin
  Self.InviteModule.ProgressResendInterval := Value;
end;

procedure TIdSipUserAgent.SetProxy(Value: TIdSipUri);
begin
  Self.Proxy.Uri := Value.Uri;
end;

//******************************************************************************
//* TIdSipStackConfigurator                                                    *
//******************************************************************************
//* TIdSipStackConfigurator Public methods *************************************

function TIdSipStackConfigurator.CreateUserAgent(Configuration: TStrings;
                                                 Context: TIdTimerQueue): TIdSipUserAgent;
var
  PendingActions: TObjectList;
begin
  try
    Result := Self.CreateLayers(Context);

    PendingActions := TObjectList.Create(false);
    try
      Self.ParseFile(Result, Configuration, PendingActions);
      Self.InstantiateMissingObjectsAsDefaults(Result);
      Self.SendPendingActions(PendingActions);
    finally
      PendingActions.Free;
    end;
  except
    FreeAndNil(Result);

    raise;
  end;
end;

//* TIdSipStackConfigurator Private methods ************************************

procedure TIdSipStackConfigurator.AddAuthentication(UserAgent: TIdSipAbstractCore;
                                                    const AuthenticationLine: String);
var
  Line: String;
begin
  Line := AuthenticationLine;
  Self.EatDirective(Line);

  if IsEqual(Trim(Line), MockKeyword) then
    UserAgent.Authenticator := TIdSipMockAuthenticator.Create;
end;

procedure TIdSipStackConfigurator.AddAutoContact(UserAgent: TIdSipAbstractCore);
begin
  UserAgent.Contact.DisplayName      := UTF16LEToUTF8(GetFullUserName);
  UserAgent.Contact.Address.Username := UTF16LEToUTF8(GetUserName);
  UserAgent.Contact.Address.Host     := LocalAddress;
end;

procedure TIdSipStackConfigurator.AddContact(UserAgent: TIdSipAbstractCore;
                                             const ContactLine: String);
var
  Line: String;
begin
  Line := ContactLine;
  Self.EatDirective(Line);

  if (Trim(Line) = AutoKeyword) then
    Self.AddAutoContact(UserAgent)
  else begin
    UserAgent.Contact.Value := Line;

    if UserAgent.Contact.IsMalformed then
      raise EParserError.Create(Format(MalformedConfigurationLine, [ContactLine]));
  end;
end;

procedure TIdSipStackConfigurator.AddFrom(UserAgent: TIdSipAbstractCore;
                                          const FromLine: String);
var
  Line: String;
begin
  Line := FromLine;
  Self.EatDirective(Line);

  UserAgent.From.Value := Line;

  if UserAgent.From.IsMalformed then
    raise EParserError.Create(Format(MalformedConfigurationLine, [FromLine]));
end;

procedure TIdSipStackConfigurator.AddLocator(UserAgent: TIdSipAbstractCore;
                                             const NameServerLine: String);
var
  Host: String;
  Line: String;
  Loc:  TIdSipIndyLocator;
  Port: String;
begin
  // See class comment for the format for this directive.
  Line := NameServerLine;
  Self.EatDirective(Line);

  Host := Fetch(Line, ':');
  Port := Fetch(Line, ' ');

  if IsEqual(Host, MockKeyword) then
    UserAgent.Locator := TIdSipMockLocator.Create
  else begin
    if not TIdSimpleParser.IsNumber(Port) then
      raise EParserError.Create(Format(MalformedConfigurationLine, [NameServerLine]));

    Loc := TIdSipIndyLocator.Create;
    Loc.NameServer := Host;
    Loc.Port       := StrToInt(Port);

    UserAgent.Locator := Loc;
  end;

  UserAgent.Dispatcher.Locator := UserAgent.Locator;
end;

procedure TIdSipStackConfigurator.AddProxy(UserAgent: TIdSipUserAgent;
                                           const ProxyLine: String);
var
  Line: String;
begin
  Line := ProxyLine;
  Self.EatDirective(Line);

  UserAgent.HasProxy := true;

  UserAgent.Proxy.Uri := Trim(Line);

  Self.CheckUri(UserAgent.Proxy, Format(MalformedConfigurationLine, [ProxyLine]));
end;

procedure TIdSipStackConfigurator.AddSupportForEventPackage(UserAgent: TIdSipAbstractCore;
                                                            const SupportEventLine: String);
var
  I:        Integer;
  Line:     String;
  Module:   TIdSipSubscribeModule;
  Packages: TStrings;
begin
  if UserAgent.ModuleFor(MethodSubscribe).IsNull then
    Module := UserAgent.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule
  else
    Module := UserAgent.ModuleFor(MethodSubscribe) as TIdSipSubscribeModule;

  Line := SupportEventLine;
  Self.EatDirective(Line);

  Packages := TStringList.Create;
  try
    Packages.CommaText := Line;

    for I := 0 to Packages.Count - 1 do
      Module.AddPackage(Packages[I]);
  finally
    Packages.Free;
  end;
end;

procedure TIdSipStackConfigurator.AddTransport(Dispatcher: TIdSipTransactionDispatcher;
                                               const TransportLine: String);
var
  HostAndPort:  TIdSipHostAndPort;
  Line:         String;
  NewTransport: TIdSipTransport;
  Transport:    String;
begin
  // See class comment for the format for this directive.
  Line := TransportLine;

  Self.EatDirective(Line);
  Transport := Fetch(Line, ' ');

  NewTransport := TIdSipTransportRegistry.TransportFor(Transport).Create;
  Dispatcher.AddTransport(NewTransport);
  NewTransport.Timer := Dispatcher.Timer;

  HostAndPort := TIdSipHostAndPort.Create;
  try
    HostAndPort.Value := Line;
    
    NewTransport.Port := HostAndPort.Port;

    if (HostAndPort.Host = AutoKeyword) then
      NewTransport.Address := LocalAddress
    else
      NewTransport.Address := HostAndPort.Host;

    NewTransport.HostName := NewTransport.Address;
  finally
    HostAndPort.Free;
  end;
end;

procedure TIdSipStackConfigurator.CheckUri(Uri: TIdSipUri;
                                           const FailMsg: String);
begin
  if not TIdSimpleParser.IsFQDN(Uri.Host)
    and not TIdIPAddressParser.IsIPv4Address(Uri.Host)
    and not TIdIPAddressParser.IsIPv6Reference(Uri.Host) then
    raise EParserError.Create(FailMsg);
end;

function TIdSipStackConfigurator.CreateLayers(Context: TIdTimerQueue): TIdSipUserAgent;
begin
  Result := TIdSipUserAgent.Create;
  Result.Timer := Context;
  Result.Dispatcher := TIdSipTransactionDispatcher.Create(Result.Timer, nil);
end;

procedure TIdSipStackConfigurator.EatDirective(var Line: String);
begin
  Fetch(Line, ':');
  Line := Trim(Line);
end;

procedure TIdSipStackConfigurator.InstantiateMissingObjectsAsDefaults(UserAgent: TIdSipAbstractCore);
begin
  if not Assigned(UserAgent.Authenticator) then
    UserAgent.Authenticator := TIdSipAuthenticator.Create;

  if not Assigned(UserAgent.Locator) then
    UserAgent.Locator := TIdSipIndyLocator.Create;

  if UserAgent.UsingDefaultContact then
    Self.AddAutoContact(UserAgent);
end;

procedure TIdSipStackConfigurator.ParseFile(UserAgent: TIdSipUserAgent;
                                            Configuration: TStrings;
                                            PendingActions: TObjectList);
var
  I: Integer;
begin
  for I := 0 to Configuration.Count - 1 do
    Self.ParseLine(UserAgent, Configuration[I], PendingActions);
end;

procedure TIdSipStackConfigurator.ParseLine(UserAgent: TIdSipUserAgent;
                                            const ConfigurationLine: String;
                                            PendingActions: TObjectList);
var
  FirstToken: String;
  Line:       String;
begin
  Line := ConfigurationLine;
  FirstToken := Trim(Fetch(Line, ':', false));

  if      IsEqual(FirstToken, AuthenticationDirective) then
    Self.AddAuthentication(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ContactDirective) then
    Self.AddContact(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, FromDirective) then
    Self.AddFrom(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ListenDirective) then
    Self.AddTransport(UserAgent.Dispatcher, ConfigurationLine)
  else if IsEqual(FirstToken, NameServerDirective) then
    Self.AddLocator(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ProxyDirective) then
    Self.AddProxy(UserAgent,  ConfigurationLine)
  else if IsEqual(FirstToken, RegisterDirective) then
    Self.RegisterUA(UserAgent, ConfigurationLine, PendingActions)
  else if IsEqual(FirstToken, SupportEventDirective) then
    Self.AddSupportForEventPackage(UserAgent, ConfigurationLine);
end;

procedure TIdSipStackConfigurator.RegisterUA(UserAgent: TIdSipUserAgent;
                                             const RegisterLine: String;
                                             PendingActions: TObjectList);
var
  Line: String;
  Reg:  TIdSipOutboundRegisterModule;
begin
  // See class comment for the format for this directive.
  Line := RegisterLine;
  Self.EatDirective(Line);

  Line := Trim(Line);

  Reg := UserAgent.RegisterModule;

  Reg.AutoReRegister := true;
  Reg.HasRegistrar := true;
  Reg.Registrar.Uri := Line;
  PendingActions.Add(Reg.RegisterWith(Reg.Registrar));
end;

procedure TIdSipStackConfigurator.SendPendingActions(Actions: TObjectList);
var
  I: Integer;
begin
  for I := 0 to Actions.Count - 1 do
    (Actions[I] as TIdSipAction).Send;
end;

end.
