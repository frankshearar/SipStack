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
  IdSipInviteModule, IdSipOptionsModule, IdSipMessage, IdSipRegistration,
  IdSipTransaction, IdSipTransport, IdTimerQueue;

type
  TIdSipUserAgent = class;

  IIdSipUserAgentListener = interface(IIdSipTransactionUserListener)
    ['{E365D17F-054B-41AB-BB18-0C339715BFA3}']
  end;

  TIdSipUserAgent = class(TIdSipAbstractCore,
                          IIdSipInviteModuleListener)
  private
    fDoNotDisturbMessage: String;
    fHasProxy:            Boolean;
    fProxy:               TIdSipUri;
    fRegisterModule:      TIdSipOutboundRegisterModule;
    fInviteModule:        TIdSipInviteModule;
    UserAgentListeners:   TIdNotificationList;

    function  GetDoNotDisturb: Boolean;
    function  GetInitialResendInterval: Cardinal;
    function  GetProgressResendInterval: Cardinal;
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
    procedure SetDoNotDisturb(Value: Boolean);
    procedure SetInitialResendInterval(Value: Cardinal);
    procedure SetProgressResendInterval(Value: Cardinal);
    procedure SetProxy(Value: TIdSipUri);
  protected
    procedure NotifyOfDroppedMessage(Message: TIdSipMessage;
                                     Binding: TIdSipConnectionBindings); override;
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

    property DoNotDisturb:           Boolean                      read GetDoNotDisturb write SetDoNotDisturb;
    property DoNotDisturbMessage:    String                       read fDoNotDisturbMessage write fDoNotDisturbMessage;
    property HasProxy:               Boolean                      read fHasProxy write fHasProxy;
    property InitialResendInterval:  Cardinal                     read GetInitialResendInterval write SetInitialResendInterval;
    property InviteModule:           TIdSipInviteModule           read fInviteModule;
    property ProgressResendInterval: Cardinal                     read GetProgressResendInterval write SetProgressResendInterval;
    property Proxy:                  TIdSipUri                    read fProxy write SetProxy;
    property RegisterModule:         TIdSipOutboundRegisterModule read fRegisterModule;
  end;

  TIdSipPendingLocalResolutionAction = class;

  // Given a configuration file, I create a stack.
  // The configuration file consists of lines. Each line is a complete and
  // independent setting consisting of a Directive, at least one space, and the
  // settings for that Directive.
  //
  // Here's a summary of the formats for each directive:
  //   Contact: sip:wintermute@tessier-ashpool.co.luna
  //   From: "Count Zero" <sip:countzero@jammer.org>
  //   HostName: talkinghead1.tessier-ashpool.co.luna
  //   HostName: 192.168.1.1
  //   Listen: <transport name><SP><host|IPv4 address|IPv6 reference|AUTO>:<port>
  //   NameServer: <domain name or IP>:<port>
  //   NameServer: MOCK
  //   Register: <SIP/S URI>
  //   ResolveNamesLocallyFirst: <true|TRUE|yes|YES|on|ON|1|false|FALSE|no|NO|off|OFF|0>
  //   Proxy: <SIP/S URI>
  //   SupportEvent: refer
  //   InstanceID: urn:uuid:00000000-0000-0000-0000-000000000000
  //
  // We try keep the configuration as order independent as possible. To
  // accomplish this, directives are sometimes marked as pending (by putting
  // objects in the PendingActions list in UpdateConfiguration). Some pending
  // actions involve sending SIP messages (like REGISTERs). Others configure the
  // stack that must only happen after other directives have been processed
  // (like ResolveNamesLocallyFirst, which must happen after processing the
  // NameServer directive). All pending actions that modify the stack
  // configuration are always processed BEFORE message-sending pending actions.
  TIdSipStackConfigurator = class(TObject)
  private
    FalseValues:             TStrings;
    FirstTransportDirective: Boolean;

    procedure AddAddress(UserAgent: TIdSipAbstractCore;
                         AddressHeader: TIdSipAddressHeader;
                         const AddressLine: String);
    procedure AddAuthentication(UserAgent: TIdSipAbstractCore;
                                const AuthenticationLine: String);
    procedure AddAutoAddress(UserAgent: TIdSipAbstractCore;
                             AddressHeader: TIdSipAddressHeader);
    procedure AddContact(UserAgent: TIdSipAbstractCore;
                      const ContactLine: String);
    procedure AddFrom(UserAgent: TIdSipAbstractCore;
                      const FromLine: String);
    procedure AddHostName(UserAgent: TIdSipAbstractCore;
                      const HostNameLine: String);
    procedure AddLocator(UserAgent: TIdSipAbstractCore;
                         const NameServerLine: String);
    procedure AddPendingConfiguration(PendingActions: TObjectList;
                                      Action: TIdSipPendingLocalResolutionAction);
    procedure AddPendingMessageSend(PendingActions: TObjectList;
                                    Action: TIdSipAction);
    procedure AddProxy(UserAgent: TIdSipUserAgent;
                       const ProxyLine: String);
    procedure AddSupportForEventPackage(UserAgent: TIdSipAbstractCore;
                                        const SupportEventLine: String);
    procedure AddTransport(Dispatcher: TIdSipTransactionDispatcher;
                           const TransportLine: String);
    procedure CheckUri(Uri: TIdSipUri;
                       const FailMsg: String);
    function  CreateLayers(Context: TIdTimerQueue): TIdSipUserAgent;
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
    procedure UseLocalResolution(UserAgent: TIdSipAbstractCore;
                                 const ResolveNamesLocallyFirstLine: String;
                                 PendingActions: TObjectList);
    procedure SendPendingActions(Actions: TObjectList);
    procedure SetInstanceID(UserAgent: TIdSipUserAgent;
                            const InstanceIDLine: String);
  public
    constructor Create;
    destructor  Destroy; override;

    function CreateUserAgent(Configuration: TStrings;
                             Context: TIdTimerQueue): TIdSipUserAgent; overload;
    function  StrToBool(B: String): Boolean;
    procedure UpdateConfiguration(UserAgent: TIdSipUserAgent;
                                  Configuration: TStrings);
  end;

  TIdSipPendingConfigurationAction = class(TObject)
  public
    procedure Execute; virtual; abstract;
  end;

  TIdSipPendingLocalResolutionAction = class(TIdSipPendingConfigurationAction)
  private
    fCore:                TIdSipAbstractCore;
    fResolveLocallyFirst: Boolean;
  public
    constructor Create(Core: TIdSipAbstractCore;
                       ResolveLocallyFirst: Boolean);

    procedure Execute; override;

    property Core:                TIdSipAbstractCore read fCore;
    property ResolveLocallyFirst: Boolean            read fResolveLocallyFirst;
  end;

  TIdSipPendingMessageSend = class(TIdSipPendingConfigurationAction)
  private
    fAction: TIdSipAction;
  public
    constructor Create(Action: TIdSipAction);

    procedure Execute; override;

    property Action: TIdSipAction read fAction;
  end;

  TIdSipReconfigureStackWait = class(TIdWait)
  private
    fConfiguration: TStrings;
    fStack:         TIdSipUserAgent;

    procedure SetConfiguration(Value: TStrings);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Trigger; override;

    property Configuration: TStrings        read fConfiguration write SetConfiguration;
    property Stack:         TIdSipUserAgent read fStack write fStack;
  end;

// Configuration file constants
const
  AuthenticationDirective           = 'Authentication';
  AutoKeyword                       = 'AUTO';
  ContactDirective                  = ContactHeaderFull;
  DebugMessageLogDirective          = 'DebugMessageLog';
  FromDirective                     = FromHeaderFull;
  HostNameDirective                 = 'HostName';
  InstanceIDDirective               = 'InstanceID';
  ListenDirective                   = 'Listen';
  MockKeyword                       = 'MOCK';
  NameServerDirective               = 'NameServer';
  ProxyDirective                    = 'Proxy';
  RegisterDirective                 = 'Register';
  ResolveNamesLocallyFirstDirective = 'ResolveNamesLocallyFirst';
  SupportEventDirective             = 'SupportEvent';

procedure EatDirective(var Line: String);

implementation

uses
  IdSimpleParser, IdSipIndyLocator, IdSipMockLocator,
  IdSipSubscribeModule, IdSystem, IdUnicode, SysUtils;

//******************************************************************************
//* Unit Public functions & procedures                                         *
//******************************************************************************

procedure EatDirective(var Line: String);
begin
  Fetch(Line, ':');
  Line := Trim(Line);
end;

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
  // class could well expect these objects to still exist.

  inherited Destroy;

  Self.Proxy.Free;
  Self.UserAgentListeners.Free;
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
                                                 Binding: TIdSipConnectionBindings);
var
  Notification: TIdSipUserAgentDroppedUnmatchedMessageMethod;
begin
  Notification := TIdSipUserAgentDroppedUnmatchedMessageMethod.Create;
  try
    Notification.Binding   := Binding;
    Notification.Message   := Message;
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

//* TIdSipUserAgent Private methods ********************************************

function TIdSipUserAgent.GetDoNotDisturb: Boolean;
begin
  Result := Self.InviteModule.DoNotDisturb;
end;

function TIdSipUserAgent.GetInitialResendInterval: Cardinal;
begin
  Result := Self.InviteModule.InitialResendInterval;
end;

function TIdSipUserAgent.GetProgressResendInterval: Cardinal;
begin
  Result := Self.InviteModule.ProgressResendInterval;
end;

procedure TIdSipUserAgent.OnInboundCall(UserAgent: TIdSipInviteModule;
                                        Session: TIdSipInboundSession);
begin
  // For now, do nothing
end;

procedure TIdSipUserAgent.SetDoNotDisturb(Value: Boolean);
begin
  Self.InviteModule.DoNotDisturb := Value;
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

constructor TIdSipStackConfigurator.Create;
begin
  inherited Create;

  Self.FalseValues := TStringList.Create;
  Self.FalseValues.Add('false');
  Self.FalseValues.Add('FALSE');
  Self.FalseValues.Add('no');
  Self.FalseValues.Add('NO');
  Self.FalseValues.Add('off');
  Self.FalseValues.Add('OFF');
  Self.FalseValues.Add('0');
end;

destructor TIdSipStackConfigurator.Destroy;
begin
  Self.FalseValues.Free;

  inherited Destroy;
end;

function TIdSipStackConfigurator.CreateUserAgent(Configuration: TStrings;
                                                 Context: TIdTimerQueue): TIdSipUserAgent;
begin
  try
    Result := Self.CreateLayers(Context);
    Self.UpdateConfiguration(Result, Configuration);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

function TIdSipStackConfigurator.StrToBool(B: String): Boolean;
begin
  Result := Self.FalseValues.IndexOf(B) = ItemNotFoundIndex;
end;

procedure TIdSipStackConfigurator.UpdateConfiguration(UserAgent: TIdSipUserAgent;
                                                      Configuration: TStrings);
var
  PendingActions: TObjectList;
begin
  // Unregister if necessary (we've got a Registrar, and there's a different one in Configuration)
  // Update any settings in Configuration
  // Register to a new registrar if necessary

  Self.FirstTransportDirective := true;

  if UserAgent.RegisterModule.HasRegistrar and not UserAgent.RegisterModule.Registrar.IsMalformed then
    UserAgent.RegisterModule.UnregisterFrom(UserAgent.RegisterModule.Registrar).Send;

//  UserAgent.Dispatcher.ClearTransports;

  PendingActions := TObjectList.Create(false);
  try
    Self.ParseFile(UserAgent, Configuration, PendingActions);
    Self.InstantiateMissingObjectsAsDefaults(UserAgent);
    Self.SendPendingActions(PendingActions);
  finally
    PendingActions.Free;
  end;
end;

//* TIdSipStackConfigurator Private methods ************************************

procedure TIdSipStackConfigurator.AddAddress(UserAgent: TIdSipAbstractCore;
                                             AddressHeader: TIdSipAddressHeader;
                                             const AddressLine: String);
var
  Line: String;
begin
  Line := AddressLine;
  EatDirective(Line);

  if (Trim(Line) = AutoKeyword) then
    Self.AddAutoAddress(UserAgent, AddressHeader)
  else begin
    AddressHeader.Value := Line;

    if AddressHeader.IsMalformed then
      raise EParserError.Create(Format(MalformedConfigurationLine, [AddressLine]));
  end;
end;

procedure TIdSipStackConfigurator.AddAuthentication(UserAgent: TIdSipAbstractCore;
                                                    const AuthenticationLine: String);
var
  Line: String;
begin
  // See class comment for the format for this directive.
  Line := AuthenticationLine;
  EatDirective(Line);

  if IsEqual(Trim(Line), MockKeyword) then
    UserAgent.Authenticator := TIdSipMockAuthenticator.Create;
end;

procedure TIdSipStackConfigurator.AddAutoAddress(UserAgent: TIdSipAbstractCore;
                                                 AddressHeader: TIdSipAddressHeader);
begin
  AddressHeader.DisplayName      := UTF16LEToUTF8(GetFullUserName);
  AddressHeader.Address.Username := UTF16LEToUTF8(GetUserName);
  AddressHeader.Address.Host     := LocalAddress;
end;

procedure TIdSipStackConfigurator.AddContact(UserAgent: TIdSipAbstractCore;
                                             const ContactLine: String);
begin
  // See class comment for the format for this directive.
  Self.AddAddress(UserAgent, UserAgent.Contact, ContactLine);
end;

procedure TIdSipStackConfigurator.AddFrom(UserAgent: TIdSipAbstractCore;
                                          const FromLine: String);
begin
  // See class comment for the format for this directive.
  Self.AddAddress(UserAgent, UserAgent.From, FromLine);
end;

procedure TIdSipStackConfigurator.AddHostName(UserAgent: TIdSipAbstractCore;
                                              const HostNameLine: String);
var
  Line: String;
begin
  // See class comment for the format for this directive.

  Line := HostNameLine;
  EatDirective(Line);

  UserAgent.HostName := Line;
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
  EatDirective(Line);

  Host := Fetch(Line, ':');
  Port := Fetch(Line, ' ');

  if Assigned(UserAgent.Locator) then begin
    UserAgent.Locator.Free;
    UserAgent.Locator := nil;
    UserAgent.Dispatcher.Locator := nil;
  end;

  if IsEqual(Host, MockKeyword) then
    UserAgent.Locator := TIdSipMockLocator.Create
  else begin
    if not TIdSimpleParser.IsNumber(Port) then
      raise EParserError.Create(Format(MalformedConfigurationLine, [NameServerLine]));

    Loc := TIdSipIndyLocator.Create;
    Loc.NameServer          := Host;
    Loc.Port                := StrToInt(Port);

    UserAgent.Locator := Loc;
  end;

  UserAgent.Dispatcher.Locator := UserAgent.Locator;
end;

procedure TIdSipStackConfigurator.AddPendingConfiguration(PendingActions: TObjectList;
                                                          Action: TIdSipPendingLocalResolutionAction);
begin
  PendingActions.Insert(0, Action);
end;

procedure TIdSipStackConfigurator.AddPendingMessageSend(PendingActions: TObjectList;
                                                        Action: TIdSipAction);
var
  Pending: TIdSipPendingMessageSend;
begin
  Pending := TIdSipPendingMessageSend.Create(Action);
  PendingActions.Add(Pending);
end;

procedure TIdSipStackConfigurator.AddProxy(UserAgent: TIdSipUserAgent;
                                           const ProxyLine: String);
var
  Line: String;
begin
  // See class comment for the format for this directive.
  Line := ProxyLine;
  EatDirective(Line);

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
  // See class comment for the format for this directive.
  if not UserAgent.UsesModule(TIdSipSubscribeModule) then
    Module := UserAgent.AddModule(TIdSipSubscribeModule) as TIdSipSubscribeModule
  else
    Module := UserAgent.ModuleFor(MethodSubscribe) as TIdSipSubscribeModule;

  Line := SupportEventLine;
  EatDirective(Line);

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
  HostAndPort: TIdSipHostAndPort;
  Line:        String;
  Transport:   String;
begin
  // See class comment for the format for this directive.
  Line := TransportLine;

  // If the configuration file contains any Listen directives, make sure we
  // clear all existing transports.
  if Self.FirstTransportDirective then begin
    Dispatcher.StopAllTransports;
    Dispatcher.ClearTransports;
    Self.FirstTransportDirective := false;
  end;

  EatDirective(Line);
  Transport := Fetch(Line, ' ');

  HostAndPort := TIdSipHostAndPort.Create;
  try
    HostAndPort.Value := Line;

    if (HostAndPort.Host = AutoKeyword) then
      HostAndPort.Host := LocalAddress;

    Dispatcher.AddTransportBinding(Transport, HostAndPort.Host, HostAndPort.Port);
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

procedure TIdSipStackConfigurator.InstantiateMissingObjectsAsDefaults(UserAgent: TIdSipAbstractCore);
begin
  if not Assigned(UserAgent.Authenticator) then
    UserAgent.Authenticator := TIdSipAuthenticator.Create;

  if not Assigned(UserAgent.Locator) then
    UserAgent.Locator := TIdSipIndyLocator.Create;

  if UserAgent.UsingDefaultContact then
    Self.AddAutoAddress(UserAgent, UserAgent.Contact);

  if UserAgent.UsingDefaultFrom then
    Self.AddAutoAddress(UserAgent, UserAgent.From);
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
  else if IsEqual(FirstToken, HostNameDirective) then
    Self.AddHostName(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, InstanceIDDirective) then
    Self.SetInstanceID(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ListenDirective) then
    Self.AddTransport(UserAgent.Dispatcher, ConfigurationLine)
  else if IsEqual(FirstToken, NameServerDirective) then
    Self.AddLocator(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ProxyDirective) then
    Self.AddProxy(UserAgent,  ConfigurationLine)
  else if IsEqual(FirstToken, RegisterDirective) then
    Self.RegisterUA(UserAgent, ConfigurationLine, PendingActions)
  else if IsEqual(FirstToken, ResolveNamesLocallyFirstDirective) then
    Self.UseLocalResolution(UserAgent, ConfigurationLine, PendingActions)
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
  EatDirective(Line);

  Line := Trim(Line);

  Reg := UserAgent.RegisterModule;

  Reg.AutoReRegister := true;
  Reg.HasRegistrar := true;
  Reg.Registrar.Uri := Line;
  Self.AddPendingMessageSend(PendingActions, Reg.RegisterWith(Reg.Registrar));
end;

procedure TIdSipStackConfigurator.UseLocalResolution(UserAgent: TIdSipAbstractCore;
                                                     const ResolveNamesLocallyFirstLine: String;
                                                     PendingActions: TObjectList);
var
  Line:    String;
  Pending: TIdSipPendingLocalResolutionAction;
begin
  // See class comment for the format for this directive.
  Line := ResolveNamesLocallyFirstLine;
  EatDirective(Line);

  Line := Trim(Line);

  Pending := TIdSipPendingLocalResolutionAction.Create(UserAgent, Self.StrToBool(Line));
  Self.AddPendingConfiguration(PendingActions, Pending);
end;

procedure TIdSipStackConfigurator.SendPendingActions(Actions: TObjectList);
var
  I: Integer;
begin
  for I := 0 to Actions.Count - 1 do
    (Actions[I] as TIdSipPendingConfigurationAction).Execute;
end;

procedure TIdSipStackConfigurator.SetInstanceID(UserAgent: TIdSipUserAgent;
                                                const InstanceIDLine: String);
var
  Line: String;
begin
  Line := InstanceIDLine;
  EatDirective(Line);

  UserAgent.InstanceID := Line;
end;

//******************************************************************************
//* TIdSipPendingLocalResolutionAction                                         *
//******************************************************************************
//* TIdSipPendingLocalResolutionAction Public methods **************************

constructor TIdSipPendingLocalResolutionAction.Create(Core: TIdSipAbstractCore;
                                                      ResolveLocallyFirst: Boolean);
begin
  inherited Create;

  Self.fCore                := Core;
  Self.fResolveLocallyFirst := ResolveLocallyFirst;
end;

procedure TIdSipPendingLocalResolutionAction.Execute;
var
  IndyLoc: TIdSipIndyLocator;
begin
  if (Self.Core.Locator is TIdSipIndyLocator) then begin
    IndyLoc := Self.Core.Locator as TIdSipIndyLocator;

    IndyLoc.ResolveLocallyFirst := Self.ResolveLocallyFirst;
  end;
end;

//******************************************************************************
//* TIdSipPendingMessageSend                                                   *
//******************************************************************************
//* TIdSipPendingMessageSend Public methods ************************************

constructor TIdSipPendingMessageSend.Create(Action: TIdSipAction);
begin
  inherited Create;

  Self.fAction := Action;
end;

procedure TIdSipPendingMessageSend.Execute;
begin
  Self.Action.Send;
end;

//******************************************************************************
//* TIdSipReconfigureStackWait                                                 *
//******************************************************************************
//* TIdSipReconfigureStackWait Public methods **********************************

constructor TIdSipReconfigureStackWait.Create;
begin
  inherited Create;

  Self.fConfiguration := TStringList.Create;
end;

destructor TIdSipReconfigureStackWait.Destroy;
begin
  Self.fConfiguration.Free;

  inherited Destroy;
end;

procedure TIdSipReconfigureStackWait.Trigger;
var
  Configurator: TIdSipStackConfigurator;
begin
  Configurator := TIdSipStackConfigurator.Create;
  try
    Configurator.UpdateConfiguration(Self.Stack, Self.Configuration);
  finally
    Configurator.Free;
  end;
end;

//* TIdSipReconfigureStackWait Private methods *********************************

procedure TIdSipReconfigureStackWait.SetConfiguration(Value: TStrings);
begin
  Self.fConfiguration.Assign(Value);
end;

end.
