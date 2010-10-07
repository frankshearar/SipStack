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
  Contnrs, Classes, IdConnectionBindings, IdNotification, IdRegisteredObject,
  IdRoutingTable, IdSipAuthentication, IdSipCore, IdSipInviteModule,
  IdSipLocator, IdSipMessage, IdSipOptionsModule, IdSipRegistration,
  IdSipTransaction, IdSipTransport, IdTimerQueue, Windows;

type
  TIdSipUserAgent = class(TIdSipAbstractCore,
                          IIdSipInviteModuleListener,
                          IIdSipRegistrationListener)
  private
    ContactClosestToRegistrar: TIdSipContactHeader;
    Registrar:                 TIdSipUri;
    fDoNotDisturbMessage:      String;
    fFrom:                     TIdSipFromHeader;
    fRegisterModule:           TIdSipOutboundRegisterModule;
    fInviteModule:             TIdSipInviteModule;
    HasRegistered:             Boolean;

    function  DefaultFrom: String;
    function  DefaultPort(SecureTransport: Boolean): TPortNum;
    function  GetDoNotDisturb: Boolean;
    function  GetInitialResendInterval: Cardinal;
    function  GetProgressResendInterval: Cardinal;
    function  GetUsername: String;
    function  FirstResolvedName(FQDN: String): String;
    function  LocalContactNeverSet: Boolean;
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistrationBase;
                        ErrorCode: Cardinal;
                        const Reason: String);
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistrationBase;
                        CurrentBindings: TIdSipContacts);
    procedure SetDoNotDisturb(Value: Boolean);
    procedure SetFrom(Value: TIdSipFromHeader);
    procedure SetInitialResendInterval(Value: Cardinal);
    procedure SetProgressResendInterval(Value: Cardinal);
    procedure SetUsername(Value: String);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure AddLocalHeaders(OutboundRequest: TIdSipRequest; InDialogRequest: Boolean); override;
    function  AddOutboundAction(ActionType: TIdSipActionClass): TIdSipAction; override;
    procedure AddTransportListener(Listener: IIdSipTransportListener);
    function  UsingDefaultFrom: Boolean;
    procedure RemoveTransportListener(Listener: IIdSipTransportListener);
    function  RegisterWith(Registrar: TIdSipUri;
                           From: TIdSipFromHeader): TIdSipOutboundRegistration;
    function  ResponseForInvite: Cardinal; override;
    function  SessionCount: Integer;
    function  UnregisterFrom(Registrar: TIdSipUri): TIdSipOutboundUnregistration;

    property DoNotDisturb:           Boolean                      read GetDoNotDisturb write SetDoNotDisturb;
    property DoNotDisturbMessage:    String                       read fDoNotDisturbMessage write fDoNotDisturbMessage;
    property From:                   TIdSipFromHeader             read fFrom write SetFrom;
    property InitialResendInterval:  Cardinal                     read GetInitialResendInterval write SetInitialResendInterval;
    property InviteModule:           TIdSipInviteModule           read fInviteModule;
    property ProgressResendInterval: Cardinal                     read GetProgressResendInterval write SetProgressResendInterval;
    property RegisterModule:         TIdSipOutboundRegisterModule read fRegisterModule;
    property Username:               String                       read GetUsername write SetUsername;
  end;

  TIdSipPendingLocalResolutionAction = class;

  // Given a configuration file, I create a stack.
  // The configuration file consists of lines. Each line is a complete and
  // independent setting consisting of a Directive, at least one space, and the
  // settings for that Directive.
  //
  // Here's a summary of the formats for each directive:
  //   Authentication: <policy name>[;policy specific param=value pairs]
  //   ConserveConnections: <true|TRUE|yes|YES|on|ON|1|false|FALSE|no|NO|off|OFF|0>
  //   From: "Count Zero" <sip:countzero@jammer.org>
  //   HostName: talkinghead1.tessier-ashpool.co.luna
  //   HostName: 192.168.1.1
  //   InstanceID: urn:uuid:00000000-0000-0000-0000-000000000000
  //   Listen: <transport name><SP><host|IPv4 address|IPv6 reference|AUTO>:<port>
  //   Listen: UDP AUTO
  //   Listen: UDP 127.0.0.1
  //   Listen: TCP [::1]:5060
  //   MappedRoute: <route>/<netmask|number of bits><SP><gateway>[<SP><port>]
  //   MappedRoute: 192.168.0.0/16 192.168.1.1 5060
  //   MappedRoute: 192.168.0.0/255.255.255.0 192.168.1.1
  //   MappedRoute: ::/0 2002:deca:fbad::1 15060
  //   NameServer: <domain name or IP>:<port>
  //   NameServer: MOCK [;ReturnOnlySpecifiedRecords]
  //   Register: <SIP/S URI>
  //   ResolveNamesLocallyFirst: <true|TRUE|yes|YES|on|ON|1|false|FALSE|no|NO|off|OFF|0>
  //   Route: "<"<SIP/S URI>">"<SP>[<route>/<netmask|number of bits>]
  //   Route: <sip:internet_gw;lr>
  //   Route: <sip:localsubnet> 10.0.0.0/8
  //   Route: <sip:specialproxy> example.com
  //   RoutingTable: MOCK
  //   SupportEvent: refer
  //   SuppressLocalResponses: <true|TRUE|yes|YES|on|ON|1|false|FALSE|no|NO|off|OFF|0>
  //   UseGruu: <true|TRUE|yes|YES|on|ON|1|false|FALSE|no|NO|off|OFF|0>
  //   UseInboundConnections: <true|TRUE|yes|YES|on|ON|1|false|FALSE|no|NO|off|OFF|0>
  //   UseTransport: <address space> <transport>
  //
  // Registrar-specific directives are:
  //   ActAsRegistrar: <true|TRUE|yes|YES|on|ON|1|false|FALSE|no|NO|off|OFF|0>
  //   RegistrarDatabase: MOCK|<as yet undefined>[;MatchOnlyUsername]
  //   RegistrarUseGruu: <true|TRUE|yes|YES|on|ON|1|false|FALSE|no|NO|off|OFF|0>
  //
  // Some directives only make sense for mock objects. For instance:
  //   MockRoute: <destination network>/<mask|number of bits><SP><gateway><SP><metric><SP><interface><SP><local address>
  //   MockDns: A <FQDN> <IPv4 address>
  //   MockDns: AAAA <FQDN> <IPv6 address>
  //   MockDns: NAPTR <key (domain name)> <order> <preference> <flags> <service> <regex> <replacement>
  //   MockDns: NAPTR local 0 0 "s" "SIP+D2T" "" _sip._tcp
  //   MockDns: SRV <service> <domain> <priority> <weight> <port> <FQDN>
  //   MockDns: SRV _sips._tcp local 1 2 5061 roke
  //   MockLocalAddress: <IPv4 address|IPv6 address>
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
    FirstRouteDirective:        Boolean;
    FirstTransportDirective:    Boolean;
    FirstUseTransportDirective: Boolean;

    procedure AddAddress(UserAgent: TIdSipAbstractCore;
                         AddressHeader: TIdSipAddressHeader;
                         const AddressLine: String);
    procedure AddAuthentication(UserAgent: TIdSipAbstractCore;
                                const AuthenticationLine: String);
    procedure AddAutoAddress(AddressHeader: TIdSipAddressHeader);
    procedure AddFrom(UserAgent: TIdSipUserAgent;
                      const FromLine: String);
    procedure AddHostName(UserAgent: TIdSipAbstractCore;
                      const HostNameLine: String);
    procedure AddLocator(UserAgent: TIdSipAbstractCore;
                         const NameServerLine: String);
    procedure AddMappedRoute(UserAgent: TIdSipAbstractCore;
                             const MappedRouteLine: String;
                             PendingActions: TObjectList);
    procedure AddMockAAAARecord(UserAgent: TIdSipAbstractCore;
                                const AAAALine: String;
                                PendingActions: TObjectList);
    procedure AddMockARecord(UserAgent: TIdSipAbstractCore;
                             const ALine: String;
                             PendingActions: TObjectList);
    procedure AddMockDnsEntry(UserAgent: TIdSipAbstractCore;
                              const MockDnsLine: String;
                              PendingActions: TObjectList);
    procedure AddMockLocalAddress(UserAgent: TIdSipAbstractCore;
                                  const MockLocalAddressLine: String;
                                  PendingActions: TObjectList);
    procedure AddMockNAPTRRecord(UserAgent: TIdSipAbstractCore;
                                 const NaptrLine: String;
                                 PendingActions: TObjectList);
    procedure AddMockSRVRecord(UserAgent: TIdSipAbstractCore;
                               const SrvLine: String;
                               PendingActions: TObjectList);
    procedure AddMockRoute(UserAgent: TIdSipAbstractCore;
                           const MockRouteLine: String;
                           PendingActions: TObjectList);
    procedure AddPendingConfiguration(PendingActions: TObjectList;
                                      Action: TIdSipPendingLocalResolutionAction);
    procedure AddPendingMessageSend(PendingActions: TObjectList;
                                    Action: TIdSipAction);
    procedure AddPendingUnregister(UserAgent: TIdSipUserAgent;
                                   PendingActions: TObjectList);
    procedure AddRegistrarModule(UserAgent: TIdSipUserAgent;
                                 const RegistrarModuleLine: String);
    procedure AddRouteHeader(UserAgent: TIdSipUserAgent;
                             const RouteHeaderLine: String);
    procedure AddRoutingTable(UserAgent: TIdSipAbstractCore;
                              const RoutingTableLine: String);
    procedure AddSupportForEventPackage(UserAgent: TIdSipAbstractCore;
                                        const SupportEventLine: String);
    procedure AddTransport(Dispatcher: TIdSipTransactionDispatcher;
                           const TransportLine: String);
    procedure AddUseTransport(UserAgent: TIdSipAbstractCore;
                              const UseTransportLine: String);
    procedure CheckAddressSpace(AddressSpace: String;
                                const FailMsg: String);
    procedure CheckUri(Uri: TIdSipUri;
                       const FailMsg: String);
    function  CreateLayers(Context: TIdTimerQueue): TIdSipUserAgent;
    function  CreatePlatformRoutingTable: TIdRoutingTable;
    function  InstantiateInviteModule(UserAgent: TIdSipUserAgent): TIdSipInviteModule;
    procedure InstantiateMissingObjectsAsDefaults(UserAgent: TIdSipUserAgent);
    function  InstantiateRegistrarModule(UserAgent: TIdSipUserAgent): TIdSipRegisterModule;
    procedure ParseFile(UserAgent: TIdSipUserAgent;
                        Configuration: TStrings;
                        PendingActions: TObjectList);
    procedure ParseLine(UserAgent: TIdSipUserAgent;
                        const ConfigurationLine: String;
                        PendingActions: TObjectList);
    procedure PostConfigurationActions(UA: TIdSipUserAgent);
    procedure PreConfigurationActions(UA: TIdSipUserAgent);
    procedure RegisterUA(UserAgent: TIdSipUserAgent;
                         const RegisterLine: String;
                         PendingActions: TObjectList);
    procedure SetConserveConnections(UserAgent: TIdSipUserAgent;
                                     const ConserveConnectionsLine: String;
                                     PendingActions: TObjectList);
    procedure SetRegistrarDatabase(UserAgent: TIdSipUserAgent;
                                   const RegistrarDatabaseLine: String);
    procedure SetRegistrarUseGruu(UserAgent: TIdSipUserAgent;
                                  const UseGruuLine: String);
    procedure SetSuppressLocalResponses(UserAgent: TIdSipUserAgent;
                                        const SuppressLocalResponsesLine: String);
    procedure UseGruu(UserAgent: TIdSipAbstractCore;
                      const UseGruuLine: String);
    procedure UseInboundConnections(UserAgent: TIdSipAbstractCore;
                                    const UseInboundConnectionsLine: String);
    procedure UseLocalResolution(UserAgent: TIdSipAbstractCore;
                                 const ResolveNamesLocallyFirstLine: String;
                                 PendingActions: TObjectList);
    procedure UserAgentName(UserAgent: TIdSipAbstractCore;
                      const UserAgentNameLine: String);
    procedure SendPendingActions(Actions: TObjectList);
    procedure SetInstanceID(UserAgent: TIdSipUserAgent;
                            const InstanceIDLine: String);
  public
    function CreateLocator(Configuration: String): TIdSipAbstractLocator;
    function CreateUserAgent(Configuration: TStrings;
                             Context: TIdTimerQueue): TIdSipUserAgent; overload;
    function CreateUserAgent(Configuration: TStrings;
                             Context: TIdTimerQueue;
                             Listeners: array of IIdSipTransactionUserListener): TIdSipUserAgent; overload;
    procedure UpdateConfiguration(UserAgent: TIdSipUserAgent;
                                  Configuration: TStrings);
  end;

  TIdSipUserAgentFactory = class(TObject)
  private
    fConfiguration: TStrings;
    fListeners:     TIdSipTransactionUserListeners;
    fTimer:         TIdTimerQueue;

    procedure SetConfiguration(Value: TStrings);
    procedure SetListeners(Value: TIdSipTransactionUserListeners);
  public
    constructor Create;
    destructor  Destroy; override;

    function Copy: TIdSipUserAgentFactory;
    function CreateUserAgent: TIdSipUserAgent;

    property Configuration: TStrings                       read fConfiguration write SetConfiguration;
    property Listeners:     TIdSipTransactionUserListeners read fListeners write SetListeners;
    property Timer:         TIdTimerQueue                  read fTimer write fTimer;
  end;

  TIdSipUserAgentFactoryClass = class of TIdSipUserAgentFactory;

  TIdSipPendingConfigurationAction = class(TObject)
  public
    procedure Execute; virtual;
  end;

  TIdSipPendingCoreConfigurationAction = class(TIdSipPendingConfigurationAction)
  private
    fCore: TIdSipAbstractCore;
  public
    constructor Create(Core: TIdSipAbstractCore);

    property Core: TIdSipAbstractCore read fCore;
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

  TIdSipPendingMappedRouteAction = class(TIdSipPendingCoreConfigurationAction)
  private
    fGateway:      String;
    fMappedPort:   TPortNum;
    fMask:         String;
    fNetwork:      String;
  public
    procedure Execute; override;

    property Gateway:    String   read fGateway write fGateway;
    property MappedPort: TPortNum read fMappedPort write fMappedPort;
    property Mask:       String   read fMask write fMask;
    property Network:    String   read fNetwork write fNetwork;
  end;

  TIdSipPendingMockLocalAddressAction = class(TIdSipPendingCoreConfigurationAction)
  private
    fAddress: String;
  public
    procedure Execute; override;

    property Address: String read fAddress write fAddress;
  end;

  TIdSipPendingMockDnsAction = class(TIdSipPendingCoreConfigurationAction)
  end;

  TIdSipPendingMockDnsNameRecordAction = class(TIdSipPendingMockDnsAction)
  private
    fHostName:  String;
    fIPAddress: String;
  public
    property HostName:  String read fHostName write fHostName;
    property IPAddress: String read fIPAddress write fIPAddress;
  end;

  TIdSipPendingMockDnsARecordAction = class(TIdSipPendingMockDnsNameRecordAction)
  public
    procedure Execute; override;
  end;

  TIdSipPendingMockDnsAAAARecordAction = class(TIdSipPendingMockDnsNameRecordAction)
  public
    procedure Execute; override;
  end;

  TIdSipPendingMockDnsNAPTRRecordAction = class(TIdSipPendingMockDnsAction)
  private
    fFlags:       String;
    fKey:         String;
    fOrder:       Cardinal;
    fPreference:  Cardinal;
    fRegEx:       String;
    fReplacement: String;
    fService:     String;
  public
    procedure Execute; override;

    property Flags:       String   read fFlags write fFlags;
    property Key:         String   read fKey write fKey;
    property Order:       Cardinal read fOrder write fOrder;
    property Preference:  Cardinal read fPreference write fPreference;
    property RegEx:       String   read fRegEx write fRegEx;
    property Replacement: String   read fReplacement write fReplacement;
    property Service:     String   read fService write fService;
  end;

  TIdSipPendingMockDnsSRVRecordAction = class(TIdSipPendingMockDnsAction)
  private
    fDomain:   String;
    fPort:     TPortNum;
    fPriority: Cardinal;
    fService:  String;
    fTarget:   String;
    fWeight:   Cardinal;
  public
    procedure Execute; override;

    property Domain:   String   read fDomain write fDomain;
    property Port:     TPortNum read fPort write fPort;
    property Priority: Cardinal read fPriority write fPriority;
    property Service:  String   read fService write fService;
    property Target:   String   read fTarget write fTarget;
    property Weight:   Cardinal read fWeight write fWeight;
  end;

  TIdSipPendingMockRouteAction = class(TIdSipPendingCoreConfigurationAction)
  private
    fDestination:    String;
    fGateway:        String;
    fInterfaceIndex: String;
    fLocalAddress:   String;
    fMask:           String;
    fMetric:         Cardinal;
  public
    procedure Execute; override;

    property Destination:    String             read fDestination write fDestination;
    property Gateway:        String             read fGateway write fGateway;
    property InterfaceIndex: String             read fInterfaceIndex write fInterfaceIndex;
    property LocalAddress:   String             read fLocalAddress write fLocalAddress;
    property Mask:           String             read fMask write fMask;
    property Metric:         Cardinal           read fMetric write fMetric;
  end;

  TIdSipPendingMessageSend = class(TIdSipPendingConfigurationAction)
  private
    fAction: TIdSipAction;
  public
    constructor Create(Action: TIdSipAction);

    procedure Execute; override;

    property Action: TIdSipAction read fAction;
  end;

  TIdSipPendingRegistration = class(TIdSipPendingConfigurationAction)
  private
    fRegistrar: TIdSipUri;
    fUA:        TIdSipUserAgent;
  public
    constructor Create(UA: TIdSipUserAgent; Registrar: TIdSipUri);

    procedure Execute; override;

    property UA:        TIdSipUserAgent read fUA;
    property Registrar: TIdSipUri       read fRegistrar;
  end;

  TIdSipPendingTcpTransportConfiguration = class(TIdSipPendingCoreConfigurationAction)
  private
    fConserveConnections: Boolean;
  public
    procedure Execute; override;

    property ConserveConnections: Boolean read fConserveConnections write fConserveConnections;
  end;

  TIdSipReconfigureStackWait = class(TIdWait)
  private
    fConfiguration: TStrings;
    fUserAgentID:   TRegisteredObjectID;

    procedure ActOnUserAgent(O: TObject);
    procedure SetConfiguration(Value: TStrings);
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Trigger; override;

    property Configuration: TStrings            read fConfiguration write SetConfiguration;
    property UserAgentID:   TRegisteredObjectID read fUserAgentID write fUserAgentID;
  end;

// Configuration file constants
const
  ActAsRegistrarDirective                 = 'ActAsRegistrar';
  AuthenticationDirective                 = 'Authentication';
  AutoKeyword                             = 'AUTO';
  ConserveConnectionsDirective            = 'ConserveConnections';
  DebugMessageLogDirective                = 'DebugMessageLog';
  FromDirective                           = FromHeaderFull;
  HostNameDirective                       = 'HostName';
  InstanceIDDirective                     = 'InstanceID';
  LogFileNameDirective                    = 'LogFileName';
  LogVerbosityLevelDirective              = 'LogVerbosityLevel';
  ListenDirective                         = 'Listen';
  MappedRouteDirective                    = 'MappedRoute';
  MatchOnlyUsernameOption                 = 'MatchOnlyUsername';
  MockKeyword                             = 'MOCK';
  MockDnsDirective                        = 'MockDns';
  MockLocalAddressDirective               = 'MockLocalAddress';
  MockRouteDirective                      = 'MockRoute';
  NameServerDirective                     = 'NameServer';
  NullRouteKeyword                        = 'null';
  RegisterDirective                       = 'Register';
  RegistrarDatabaseDirective              = 'RegistrarDatabase';
  RegistrarUseGruuDirective               = 'RegistrarUseGruu';
  ResolveNamesLocallyFirstDirective       = 'ResolveNamesLocallyFirst';
  ReturnOnlySpecifiedRecordsLocatorOption = 'ReturnOnlySpecifiedRecords';
  RouteHeaderDirective                    = 'Route';
  RoutingTableDirective                   = 'RoutingTable';
  SupportEventDirective                   = 'SupportEvent';
  SuppressLocalResponsesDirective         = 'SuppressLocalResponses';
  UseGruuDirective                        = 'UseGruu';
  UseInboundConnectionsDirective          = 'UseInboundConnections';
  UserAgentNameDirective                  = 'UserAgentName';
  UseTransportDirective                   = 'UseTransport';

const
  DefaultDebugLogFileName = 'debug.log';

procedure EatDirective(var Line: String);

implementation

uses
  ConfigUtils, IdAddressSpace, IdSimpleParser, IdSipDns, IdSipIndyLocator,
  IdSipLocation, IdSipInMemoryBindingDatabase, IdSipMockLocator, IdNetworking,
  IdSipProxyDescription, IdSipSubscribeModule, IdSystem, IdUnicode,
  RuntimeSafety, SysUtils;

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

  Self.ContactClosestToRegistrar := TIdSipContactHeader.Create;
  Self.ContactClosestToRegistrar.IsUnset := true;
  Self.Registrar := TIdSipUri.Create;

  Self.fFrom           := TIdSipFromHeader.Create;
  Self.fInviteModule   := Self.AddModule(TIdSipInviteModule) as TIdSipInviteModule;
  Self.fRegisterModule := Self.AddModule(TIdSipOutboundRegisterModule) as TIdSipOutboundRegisterModule;

  Self.InviteModule.AddListener(Self);

  Self.DoNotDisturb           := false;
  Self.DoNotDisturbMessage    := RSSIPTemporarilyUnavailable;
  Self.From.Value             := Self.DefaultFrom;
  Self.HasRegistered          := false;
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

  if Self.HasRegistered then
    Self.UnregisterFrom(Self.Registrar).Send;

  inherited Destroy;

  Self.Dispatcher.Free;
  Self.Authenticator.Free;

  Self.From.Free;
  Self.Registrar.Free;
  Self.ContactClosestToRegistrar.Free;
end;

procedure TIdSipUserAgent.AddLocalHeaders(OutboundRequest: TIdSipRequest; InDialogRequest: Boolean);
var
  LocalContact: TIdSipContactHeader;
begin
  inherited AddLocalHeaders(OutboundRequest, InDialogRequest);

  // draft-ietf-sip-gruu-10, section 8.1

  LocalContact := TIdSipContactHeader.Create;
  try
    if Self.ContactClosestToRegistrar.IsUnset then begin
      LocalContact.Value := OutboundRequest.From.FullValue;
      LocalContact.RemoveParameter(TagParam);
    end
    else
      LocalContact.Assign(Self.ContactClosestToRegistrar);

    LocalContact.IsGruu  := Self.UseGruu;
    LocalContact.IsUnset := Self.ContactClosestToRegistrar.IsUnset;

    OutboundRequest.AddHeader(LocalContact);
  finally
    LocalContact.Free;
  end;

  if OutboundRequest.HasSipsUri then
    OutboundRequest.FirstContact.Address.Scheme := SipsScheme;

//  if not InDialogRequest then
//    OutboundRequest.AddHeaders(Self.RoutePathFor(OutboundRequest));
end;

function TIdSipUserAgent.AddOutboundAction(ActionType: TIdSipActionClass): TIdSipAction;
begin
  Result := inherited AddOutboundAction(ActionType);

  if not Self.ContactClosestToRegistrar.IsUnset then
    Result.LocalGruu := Self.ContactClosestToRegistrar;
end;

procedure TIdSipUserAgent.AddTransportListener(Listener: IIdSipTransportListener);
begin
  Self.Dispatcher.AddTransportListener(Listener);
end;

function TIdSipUserAgent.UsingDefaultFrom: Boolean;
begin
  Result := Pos(Self.From.Address.Uri, Self.DefaultFrom) > 0;
end;

procedure TIdSipUserAgent.RemoveTransportListener(Listener: IIdSipTransportListener);
begin
  Self.Dispatcher.RemoveTransportListener(Listener);
end;

function TIdSipUserAgent.RegisterWith(Registrar: TIdSipUri;
                                      From: TIdSipFromHeader): TIdSipOutboundRegistration;
var
  LocalAddress:     TIdSipLocation;
  RegistrarAddress: String;
begin
  LocalAddress := TIdSipLocation.Create;
  try
    RegistrarAddress := Self.FirstResolvedName(Registrar.Host);

    Self.RoutingTable.LocalOrMappedAddressFor(RegistrarAddress,
                                              LocalAddress,
                                              Self.DefaultPort(Registrar.IsSecure));

    // We can only regard ContactClosestToRegistrar as "set" when we receive a
    // successful response to the REGISTER.
    Self.ContactClosestToRegistrar.DisplayName      := From.DisplayName;
    Self.ContactClosestToRegistrar.Address.Scheme   := Registrar.Scheme;
    Self.ContactClosestToRegistrar.Address.Username := From.Address.Username;
    Self.ContactClosestToRegistrar.Address.Host     := LocalAddress.IPAddress;
    Self.ContactClosestToRegistrar.Address.Port     := LocalAddress.Port;

    if Self.UseGruu then
      Self.ContactClosestToRegistrar.SipInstance := Self.InstanceID;
    Self.HasRegistered := true;
  finally
    LocalAddress.Free;
  end;

  Self.Registrar.Uri := Registrar.Uri;

  Result := Self.RegisterModule.RegisterWith(Registrar, Self.ContactClosestToRegistrar);
  Result.LocalParty := From;
  Result.AddListener(Self);
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

function TIdSipUserAgent.UnregisterFrom(Registrar: TIdSipUri): TIdSipOutboundUnregistration;
begin
  if not Self.HasRegistered and Self.LocalContactNeverSet then
    Self.ContactClosestToRegistrar.Assign(Self.From);

  Result := Self.RegisterModule.UnregisterFrom(Registrar, Self.ContactClosestToRegistrar);
  Result.AddListener(Self);

  // This is technically incorrect: we should only set this when Result.Send is
  // invoked.
  Self.HasRegistered := false;
end;

//* TIdSipUserAgent Private methods ********************************************

function TIdSipUserAgent.DefaultFrom: String;
begin
  Result := 'unknown <sip:unknown@' + Self.HostName + '>';
end;

function TIdSipUserAgent.DefaultPort(SecureTransport: Boolean): TPortNum;
begin
  if SecureTransport then
    Result := DefaultSipsPort
  else
    Result := DefaultSipPort;
end;

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

function TIdSipUserAgent.GetUsername: String;
begin
  Result := Self.From.DisplayName;
end;

function TIdSipUserAgent.FirstResolvedName(FQDN: String): String;
var
  Names: TIdDomainNameRecords;
begin
  if TIdIPAddressParser.IsIPAddress(Registrar.Host) then
    Result := Registrar.Host
  else begin
    Names := TIdDomainNameRecords.Create;
    try
      Self.Locator.ResolveNameRecords(Registrar.Host, Names);

      if Names.IsEmpty then begin
        // Something's gone very wrong and we're about to try register to a
        // registrar that doesn't really exist!
        //
        // This isn't much of a solution. The only good reasons for doing this
        // are that (a) it doesn't do much harm (?) and (b) it allows us to
        // produce a meaningful Result, even if the result of Result.Send is
        // effectively a no-op.
        Result := '127.0.0.1';
      end
      else
        Result := Names[0].IPAddress;
    finally
      Names.Free;
    end;
  end;
end;

function TIdSipUserAgent.LocalContactNeverSet: Boolean;
begin
  Result := Self.ContactClosestToRegistrar.IsUnset;
end;

procedure TIdSipUserAgent.OnFailure(RegisterAgent: TIdSipOutboundRegistrationBase;
                                    ErrorCode: Cardinal;
                                    const Reason: String);
begin
end;

procedure TIdSipUserAgent.OnInboundCall(UserAgent: TIdSipInviteModule;
                                        Session: TIdSipInboundSession);
begin
  // For now, do nothing.
end;

procedure TIdSipUserAgent.OnSuccess(RegisterAgent: TIdSipOutboundRegistrationBase;
                                    CurrentBindings: TIdSipContacts);
var
  Gruu: String;
begin
  if Self.UseGruu then begin
    Gruu := CurrentBindings.GruuFor(Self.ContactClosestToRegistrar);

    if (Gruu <> '') then begin
      Self.ContactClosestToRegistrar.Address.Uri := Gruu;
      Self.ContactClosestToRegistrar.IsUnset     := false;
    end;
  end;
end;

procedure TIdSipUserAgent.SetDoNotDisturb(Value: Boolean);
begin
  Self.InviteModule.DoNotDisturb := Value;
end;

procedure TIdSipUserAgent.SetFrom(Value: TIdSipFromHeader);
begin
  Self.From.Assign(Value);

  if Self.From.IsMalformed then
    raise EBadHeader.Create(Self.From.Name);

  if not Self.From.Address.IsSipUri then
    raise EBadHeader.Create(Self.From.Name + ': MUST be a SIP/SIPS URI');
end;

procedure TIdSipUserAgent.SetInitialResendInterval(Value: Cardinal);
begin
  Self.InviteModule.InitialResendInterval := Value;
end;

procedure TIdSipUserAgent.SetProgressResendInterval(Value: Cardinal);
begin
  Self.InviteModule.ProgressResendInterval := Value;
end;

procedure TIdSipUserAgent.SetUsername(Value: String);
begin
  Self.From.DisplayName := Value;
end;

//******************************************************************************
//* TIdSipStackConfigurator                                                    *
//******************************************************************************
//* TIdSipStackConfigurator Public methods *************************************

function TIdSipStackConfigurator.CreateLocator(Configuration: String): TIdSipAbstractLocator;
var
  Host: String;
  Line: String;
  Loc:  TIdSipIndyLocator;
  Mock: TIdSipMockLocator;
  Port: String;
begin
  Line := Configuration;
  EatDirective(Line);

  Host := Fetch(Line, [':', ';']);

  if IsEqual(Host, MockKeyword) then begin
    Mock := TIdSipMockLocator.Create;

    if (Line <> '') then begin
      // There are additional configuration options for the mock locator to
      // process:

      Mock.ReturnOnlySpecifiedRecords := IsEqual(Line, ReturnOnlySpecifiedRecordsLocatorOption);

    end;
    Result := Mock;
  end
  else begin
    Port := Fetch(Line, [' ', ';']);
    if {(Port <> '') and} not TIdSimpleParser.IsNumber(Port) then
      raise EParserError.Create(Format(MalformedConfigurationLine, [Configuration]));

    Loc := TIdSipIndyLocator.Create;
    Loc.NameServer          := Host;
    Loc.Port                := StrToInt(Port);

    Result := Loc;
  end;
end;

function TIdSipStackConfigurator.CreateUserAgent(Configuration: TStrings;
                                                 Context: TIdTimerQueue): TIdSipUserAgent;
begin
  // There is a race condition in code like this:
  //   UA := Configurator.CreateUserAgent(Conf, Context);
  //   UA.AddListener(MyApp);
  // because if Conf contains Register directives, UA will create Wait objects
  // that will, in the context of Context, create TIdSipActions. That means that
  // MyApp may or may not have its OnAddAction executed.
  //
  // This method is DANGEROUS. It's here as a convenience method for TESTING
  // purposes.
  Result := Self.CreateUserAgent(Configuration, Context, []);
end;

function TIdSipStackConfigurator.CreateUserAgent(Configuration: TStrings;
                                                 Context: TIdTimerQueue;
                                                 Listeners: array of IIdSipTransactionUserListener): TIdSipUserAgent;
var
  I: Integer;
begin
  try
    Result := Self.CreateLayers(Context);

    for I := Low(Listeners) to High(Listeners) do
      Result.AddListener(Listeners[I]);

    Self.UpdateConfiguration(Result, Configuration);
  except
    FreeAndNil(Result);

    raise;
  end;
end;

procedure TIdSipStackConfigurator.UpdateConfiguration(UserAgent: TIdSipUserAgent;
                                                      Configuration: TStrings);
var
  PendingActions: TObjectList;
begin
  // Unregister if necessary (we've got a Registrar, and there's a different one in Configuration)
  // Update any settings in Configuration
  // Register to a new registrar if necessary

  Self.FirstRouteDirective        := true;
  Self.FirstTransportDirective    := true;
  Self.FirstUseTransportDirective := true;

//  UserAgent.Dispatcher.ClearTransports;

  PendingActions := TObjectList.Create(false);
  try
    Self.AddPendingUnregister(UserAgent, PendingActions);
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
    Self.AddAutoAddress(AddressHeader)
  else begin
    AddressHeader.Value := Line;

    if AddressHeader.IsMalformed then
      raise EParserError.Create(Format(MalformedConfigurationLine, [AddressLine]));
  end;
end;

procedure TIdSipStackConfigurator.AddAuthentication(UserAgent: TIdSipAbstractCore;
                                                    const AuthenticationLine: String);
var
  Policy: String;
  Line:   String;
  Params: TIdSipHeaderParameters;
begin
  // See class comment for the format for this directive.
  Line := AuthenticationLine;
  EatDirective(Line);

  Policy := Trim(Fetch(Line, ';'));

  if IsEqual(Policy, MockKeyword) then
    UserAgent.Authenticator := TIdSipMockAuthenticator.Create
  else
    UserAgent.Authenticator := AuthenticationPolicyFor(Policy).Create;

  if (Line <> '') then begin
    Params := TIdSipHeaderParameters.Create;
    try
      Params.Parse(';' + Line); // Restore the ; eaten by Fetch.
      UserAgent.Authenticator.SetParameters(Params);
    finally
      Params.Free;
    end;
  end;
end;

procedure TIdSipStackConfigurator.AddAutoAddress(AddressHeader: TIdSipAddressHeader);
begin
  AddressHeader.DisplayName      := UTF16LEToUTF8(GetFullUserName);
  AddressHeader.Address.Username := UTF16LEToUTF8(GetUserName);
  AddressHeader.Address.Host     := LocalAddress;
end;

procedure TIdSipStackConfigurator.AddFrom(UserAgent: TIdSipUserAgent;
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
begin
  // See class comment for the format for this directive.
  if Assigned(UserAgent.Locator) then begin
    UserAgent.Locator.Free;
    UserAgent.Locator := nil;
    UserAgent.Dispatcher.Locator := nil;
  end;

  UserAgent.Locator := Self.CreateLocator(NameServerLine);
  UserAgent.Dispatcher.Locator := UserAgent.Locator;
end;

procedure TIdSipStackConfigurator.AddMappedRoute(UserAgent: TIdSipAbstractCore;
                                                 const MappedRouteLine: String;
                                                 PendingActions: TObjectList);
var
  MappedPort:        TPortNum;
  Gateway:           String;
  Line:              String;
  MappedRouteAction: TIdSipPendingMappedRouteAction;
  Mask:              String;
  Network:           String;
  Port:              String;
  Route:             String;
begin
  // See class comment for the format for this directive.
  Line := MappedRouteLine;
  EatDirective(Line);

  Route   := Fetch(Line, ' ');
  Gateway := Fetch(Line, ' ');
  Port    := Line;

  Network := Fetch(Route, '/');
  Mask    := Route;

  // If IsNumber returns true then the route is something like "192.168.0.0/24".
  // Otherwise the route is something like "192.168.0.0/255.255.255.0".
  if TIdSimpleParser.IsNumber(Mask) then begin
    Mask := TIdIPAddressParser.MaskToAddress(StrToInt(Mask), TIdIPAddressParser.IPVersion(Network));
  end;

  // If there's no port, default to the SIP port.
  if (Port = '') then
    MappedPort := TIdSipTransportRegistry.DefaultPortFor(TcpTransport)
  else
    MappedPort := StrToPortNum(Port);

  MappedRouteAction := TIdSipPendingMappedRouteAction.Create(UserAgent);
  MappedRouteAction.Network      := Network;
  MappedRouteAction.Mask         := Mask;
  MappedRouteAction.Gateway      := Gateway;
  MappedRouteAction.MappedPort   := MappedPort;
  PendingActions.Add(MappedRouteAction);
end;

procedure TIdSipStackConfigurator.AddMockAAAARecord(UserAgent: TIdSipAbstractCore;
                                                    const AAAALine: String;
                                                    PendingActions: TObjectList);
var
  FQDN:        String;
  IPv6Address: String;
  Line:        String;
  Pending:     TIdSipPendingMockDnsAAAARecordAction;
begin
  //   MockDns: A <FQDN> <IPv6 address>
  Line := AAAALine;
  EatDirective(Line);

  // Eat the RR token.
  Fetch(Line, ' ');

  IPv6Address := Line;
  FQDN        := Fetch(IPv6Address, ' ');

  Pending := TIdSipPendingMockDnsAAAARecordAction.Create(UserAgent);
  Pending.HostName  := FQDN;
  Pending.IPAddress := IPv6Address;
  PendingActions.Add(Pending);
end;

procedure TIdSipStackConfigurator.AddMockARecord(UserAgent: TIdSipAbstractCore;
                                                 const ALine: String;
                                                 PendingActions: TObjectList);
var
  FQDN:        String;
  IPv4Address: String;
  Line:        String;
  Pending:     TIdSipPendingMockDnsARecordAction;
begin
  //   MockDns: A <FQDN> <IPv4 address>
  Line := ALine;
  EatDirective(Line);

  // Eat the RR token.
  Fetch(Line, ' ');

  IPv4Address := Line;
  FQDN        := Fetch(IPv4Address, ' ');

  if not TIdSimpleParser.IsFQDN(FQDN) then
    raise EParserError.Create(Format(MalformedConfigurationLine, [ALine]));

  if not TIdIPAddressParser.IsIPv4Address(IPv4Address) then
    raise EParserError.Create(Format(MalformedConfigurationLine, [ALine]));

  Pending := TIdSipPendingMockDnsARecordAction.Create(UserAgent);
  Pending.HostName  := FQDN;
  Pending.IPAddress := IPv4Address;
  PendingActions.Add(Pending);
end;

procedure TIdSipStackConfigurator.AddMockDnsEntry(UserAgent: TIdSipAbstractCore;
                                                  const MockDnsLine: String;
                                                  PendingActions: TObjectList);
var
  Line: String;
  RR:   String;
begin
  // See class comment for the format for this directive.
  Line := MockDnsLine;
  EatDirective(Line);

  RR := Fetch(Line, ' ');

  if IsEqual(RR, DnsARecord) then
    Self.AddMockARecord(UserAgent, MockDnsLine, PendingActions)
  else if IsEqual(RR, DnsAAAARecord) then
    Self.AddMockAAAARecord(UserAgent, MockDnsLine, PendingActions)
  else if IsEqual(RR, DnsNAPTRRecord) then
    Self.AddMockNAPTRRecord(UserAgent, MockDnsLine, PendingActions)
  else if IsEqual(RR, DnsSRVRecord) then
    Self.AddMockSRVRecord(UserAgent, MockDnsLine, PendingActions)
end;

procedure TIdSipStackConfigurator.AddMockLocalAddress(UserAgent: TIdSipAbstractCore;
                                                      const MockLocalAddressLine: String;
                                                      PendingActions: TObjectList);
var
  Line: String;
  Pending: TIdSipPendingMockLocalAddressAction;
begin
  // See class comment for the format for this directive.
  Line := MockLocalAddressLine;
  EatDirective(Line);

  if not TIdIPAddressParser.IsIPAddress(Line) then
      raise EParserError.Create(Format(MalformedConfigurationLine, [MockLocalAddressLine]));

  Pending := TIdSipPendingMockLocalAddressAction.Create(UserAgent);
  Pending.Address := Line;

  PendingActions.Add(Pending);
end;

procedure TIdSipStackConfigurator.AddMockNAPTRRecord(UserAgent: TIdSipAbstractCore;
                                                     const NaptrLine: String;
                                                     PendingActions: TObjectList);
  function Dequote(S: String): String;
  var
    FirstChar: Char;
  begin
    Result := S;
    if (S = '') then Exit;

    FirstChar := S[1];
    if (FirstChar in ['''', '"']) and (LastChar(S) = FirstChar) then
      Result := WithoutFirstAndLastChars(S);
  end;
var
  Flags:       String;
  Key:         String;
  Line:        String;
  Order:       String;
  Preference:  String;
  Pending:     TIdSipPendingMockDnsNAPTRRecordAction;
  RegEx:       String;
  Replacement: String;
  Service:     String;
begin
  //   MockDns: NAPTR <key (domain name)> <order> <preference> "<flags>" "<service>" "<regex>" <replacement>

  Line := NaptrLine;
  EatDirective(Line);

  // Eat the RR token.
  Fetch(Line, ' ');

  Key         := Fetch(Line, ' ');
  Order       := Fetch(Line, ' ');
  Preference  := Fetch(Line, ' ');
  Flags       := Fetch(Line, ' ');
  Service     := Fetch(Line, ' ');
  Regex       := Fetch(Line, ' ');
  Replacement := Fetch(Line, ' ');

  if not TIdSimpleParser.IsFQDN(Key) then
      raise EParserError.Create(Format(MalformedConfigurationLine, [NaptrLine]));
  if not TIdSimpleParser.IsNumber(Order) then
      raise EParserError.Create(Format(MalformedConfigurationLine, [NaptrLine]));
  if not TIdSimpleParser.IsNumber(Preference) then
      raise EParserError.Create(Format(MalformedConfigurationLine, [NaptrLine]));
  if (Flags = '') then
    raise EParserError.Create(Format(MalformedConfigurationLine, [NaptrLine]));
  if (Service = '') then
    raise EParserError.Create(Format(MalformedConfigurationLine, [NaptrLine]));
  if (Regex = '') then
    raise EParserError.Create(Format(MalformedConfigurationLine, [NaptrLine]));
  if (Replacement = '') then
    raise EParserError.Create(Format(MalformedConfigurationLine, [NaptrLine]));

  Pending := TIdSipPendingMockDnsNAPTRRecordAction.Create(UserAgent);
  Pending.Flags       := Dequote(Flags);
  Pending.Key         := Key;
  Pending.Order       := StrToInt(Order);
  Pending.Preference  := StrToInt(Preference);
  Pending.Regex       := Dequote(Regex);
  Pending.Replacement := Replacement;
  Pending.Service     := Dequote(Service);

  PendingActions.Add(Pending);
end;

procedure TIdSipStackConfigurator.AddMockSRVRecord(UserAgent: TIdSipAbstractCore;
                                                   const SrvLine: String;
                                                   PendingActions: TObjectList);
var
  Domain:   String;
  Line:     String;
  Pending:  TIdSipPendingMockDnsSRVRecordAction;
  Port:     String;
  Priority: String;
  Service:  String;
  Target:   String;
  Weight:   String;
begin
  //   MockDns: SRV <service> <domain> <priority> <weight> <port> <FQDN>
  Line := SrvLine;
  EatDirective(Line);

  // Eat the RR token.
  Fetch(Line, ' ');

  Service  := Fetch(Line, ' ');
  Domain   := Fetch(Line, ' ');
  Priority := Fetch(Line, ' ');
  Weight   := Fetch(Line, ' ');
  Port     := Fetch(Line, ' ');
  Target   := Fetch(Line, ' ');

  if not TIdSimpleParser.IsNumber(Priority) then
    raise EParserError.Create(Format(MalformedConfigurationLine, [SrvLine]));
  if not TIdSimpleParser.IsNumber(Weight) then
    raise EParserError.Create(Format(MalformedConfigurationLine, [SrvLine]));
  if not TIdSimpleParser.IsNumber(Port) then
    raise EParserError.Create(Format(MalformedConfigurationLine, [SrvLine]));

  Pending := TIdSipPendingMockDnsSRVRecordAction.Create(UserAgent);
  Pending.Domain   := Domain;
  Pending.Port     := StrToPortNum(Port);
  Pending.Priority := StrToInt(Priority);
  Pending.Service  := Service;
  Pending.Target   := Target;
  Pending.Weight   := StrToInt(Weight);
  PendingActions.Add(Pending);
end;

procedure TIdSipStackConfigurator.AddMockRoute(UserAgent: TIdSipAbstractCore;
                                               const MockRouteLine: String;
                                               PendingActions: TObjectList);
var
  Gateway:         String;
  Iface:           String;
  Line:            String;
  LocalAddress:    String;
  MockRouteAction: TIdSipPendingMockRouteAction;
  Mask:            String;
  Metric:          Cardinal;
  Network:         String;
begin
  // See class comment for the format for this directive.
  Line := MockRouteLine;
  EatDirective(Line);

  //   MockRoute: <destination network>/<mask|number of bits><SP><gateway><SP><metric><SP><interface><SP><local address>

  Mask := Fetch(Line, ' ');
  Network := Fetch(Mask, '/');

  Gateway := Fetch(Line, ' ');
  Metric  := StrToInt(Fetch(Line, ' '));
  Iface := Fetch(Line, ' ');
  LocalAddress := Line;

  // If IsNumber returns true then the route is something like "192.168.0.0/24".
  // Otherwise the route is something like "192.168.0.0/255.255.255.0".
  if TIdSimpleParser.IsNumber(Mask) then begin
    Mask := TIdIPAddressParser.MaskToAddress(StrToInt(Mask), TIdIPAddressParser.IPVersion(Network));
  end;

  MockRouteAction := TIdSipPendingMockRouteAction.Create(UserAgent);
  MockRouteAction.Destination    := Network;
  MockRouteAction.Gateway        := Gateway;
  MockRouteAction.InterfaceIndex := Iface;
  MockRouteAction.LocalAddress   := LocalAddress;
  MockRouteAction.Mask           := Mask;
  MockRouteAction.Metric         := Metric;
  PendingActions.Add(MockRouteAction);
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

procedure TIdSipStackConfigurator.AddPendingUnregister(UserAgent: TIdSipUserAgent;
                                                       PendingActions: TObjectList);
var
  Reg: TIdSipOutboundRegisterModule;
begin
  Reg := UserAgent.RegisterModule;

  if Reg.HasRegistrar and not Reg.Registrar.IsMalformed then
    Self.AddPendingMessageSend(PendingActions, UserAgent.UnregisterFrom(Reg.Registrar));
end;

procedure TIdSipStackConfigurator.AddRegistrarModule(UserAgent: TIdSipUserAgent;
                                                     const RegistrarModuleLine: String);
var
  Answer: String;
  Line:   String;
  Params: TIdSipHeaderParameters;
begin
  // See class comment for the format for this directive.
  Line := RegistrarModuleLine;
  EatDirective(Line);

  Answer := Fetch(Line, ';');

  if not UserAgent.UsesModule(TIdSipRegisterModule) then begin
    if StrAsBool(Answer) then
      UserAgent.AddModule(TIdSipRegisterModule)
  end
  else begin
    if not StrAsBool(Answer) then
      UserAgent.RemoveModule(TIdSipRegisterModule);
  end;

  if UserAgent.UsesModule(TIdSipRegisterModule) then begin
    Params := TIdSipHeaderParameters.Create;
    try
      Params.Parse(Line);
      UserAgent.ModuleFor(TIdSipRegisterModule).Configure(Params);
    finally
      Params.Free;
    end;
  end;
end;

procedure TIdSipStackConfigurator.AddRouteHeader(UserAgent: TIdSipUserAgent;
                                                 const RouteHeaderLine: String);
var
  AddressSpace: String;
  Line:         String;
  U:            TIdSipUri;
  Uri:          String;
begin
  // See class comment for the format for this directive.
  Line := RouteHeaderLine;
  EatDirective(Line);

  // If the configuration file contains any Route directives, make sure we
  // clear the existing Route path.
  if Self.FirstRouteDirective then begin
    UserAgent.ClearAllRoutePaths;
    Self.FirstRouteDirective := false;
  end;

  Fetch(Line, '<');
  Uri := Trim(Fetch(Line, '>'));
  AddressSpace := Trim(Line);

  if (AddressSpace <> '') then
    Self.CheckAddressSpace(AddressSpace, Format(MalformedConfigurationLine, [RouteHeaderLine]));

  if (Uri = NullRouteKeyword) then begin
    // Add an empty Route path to AddressSpace. If we've already processed a
    // Route directive for AddressSpace, no "empty Route" header will be added.
    // We want this! If AddressSpace = '' then we don't care: the default Route
    // path always exists and is, by default, the empty Route path.
    if (AddressSpace <> '') then
      UserAgent.AddNullRoutePath(AddressSpace);
  end
  else begin
    U := TIdSipUri.Create(Uri);
    try
      Self.CheckUri(U, Format(MalformedConfigurationLine, [RouteHeaderLine]));

      if (AddressSpace = '') then
        UserAgent.DefaultRoutePath.Add(RouteHeader).Value := '<' + U.AsString + '>'
      else begin
        UserAgent.AddRoute(AddressSpace, U);
      end;
    finally
      U.Free;
    end;
  end;
end;

procedure TIdSipStackConfigurator.AddRoutingTable(UserAgent: TIdSipAbstractCore;
                                                  const RoutingTableLine: String);
var
  Line: String;
begin
  // See class comment for the format for this directive.
  Line := RoutingTableLine;
  EatDirective(Line);

  if IsEqual(Line, MockKeyword) then
    UserAgent.RoutingTable := TIdMockRoutingTable.Create
  else
    UserAgent.RoutingTable := Self.CreatePlatformRoutingTable;

  UserAgent.Dispatcher.RoutingTable := UserAgent.RoutingTable;
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

  Module.RemoveAllPackages;

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

procedure TIdSipStackConfigurator.AddUseTransport(UserAgent: TIdSipAbstractCore;
                                                  const UseTransportLine: String);
var
  AddressSpace: String;
  Line:         String;
  TransportType: String;
begin
  Line := UseTransportLine;
  EatDirective(Line);

  if FirstUseTransportDirective then begin
    FirstUseTransportDirective := false;
    UserAgent.ClearAllPreferredTransportTypes;
  end;

  AddressSpace := Trim(Fetch(Line, ' '));
  Self.CheckAddressSpace(AddressSpace, Format(MalformedConfigurationLine, [UseTransportLine]));

  TransportType := Trim(Line);

  UserAgent.Dispatcher.SetPreferredTransportTypeFor(AddressSpace, TransportType);
end;

procedure TIdSipStackConfigurator.CheckAddressSpace(AddressSpace: String;
                                                    const FailMsg: String);
var
  A: TIdAddressSpace;
begin
  A := TIdAddressSpace.Create;
  try
    if (A.IdentifySpaceType(AddressSpace) = asUnknown) then
      raise EParserError.Create(FailMsg);
  finally
    A.Free;
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
  // We don't give the dispatcher a locator yet, because the configuration file
  // will tell us what kind of locator to create.

  Result := TIdSipUserAgent.Create;
  Result.Timer := Context;
  Result.Dispatcher := TIdSipTransactionDispatcher.Create(Result.Timer, nil);
end;

function TIdSipStackConfigurator.CreatePlatformRoutingTable: TIdRoutingTable;
begin
  Result := TIdRoutingTable.PlatformRoutingTable(OsType).Create;
end;

function TIdSipStackConfigurator.InstantiateInviteModule(UserAgent: TIdSipUserAgent): TIdSipInviteModule;
begin
  if not UserAgent.UsesModule(TIdSipInviteModule) then
    Result := UserAgent.AddModule(TIdSipInviteModule) as TIdSipInviteModule
  else
    Result := UserAgent.ModuleFor(TIdSipInviteModule) as TIdSipInviteModule;
end;

procedure TIdSipStackConfigurator.InstantiateMissingObjectsAsDefaults(UserAgent: TIdSipUserAgent);
begin
  if not Assigned(UserAgent.Authenticator) then
    UserAgent.Authenticator := TIdSipNullAuthenticator.Create;

  if not Assigned(UserAgent.Locator) then
    UserAgent.Locator := TIdSipIndyLocator.Create;

  if not Assigned(UserAgent.RoutingTable) then begin
    UserAgent.RoutingTable := CreatePlatformRoutingTable;
    UserAgent.Dispatcher.RoutingTable := UserAgent.RoutingTable;
  end;

  if UserAgent.UsingDefaultFrom then
    Self.AddAutoAddress(UserAgent.From);
end;

function TIdSipStackConfigurator.InstantiateRegistrarModule(UserAgent: TIdSipUserAgent): TIdSipRegisterModule;
begin
  if not UserAgent.UsesModule(TIdSipRegisterModule) then
    Result := UserAgent.AddModule(TIdSipRegisterModule) as TIdSipRegisterModule
  else
    Result := UserAgent.ModuleFor(TIdSipRegisterModule) as TIdSipRegisterModule;
end;

procedure TIdSipStackConfigurator.ParseFile(UserAgent: TIdSipUserAgent;
                                            Configuration: TStrings;
                                            PendingActions: TObjectList);
var
  I: Integer;
begin
  Self.PreConfigurationActions(UserAgent);
  try
    for I := 0 to Configuration.Count - 1 do
      Self.ParseLine(UserAgent, Configuration[I], PendingActions);
  finally
    Self.PostConfigurationActions(UserAgent);
  end;
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

  if      IsEqual(FirstToken, ActAsRegistrarDirective) then
    Self.AddRegistrarModule(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, AuthenticationDirective) then
    Self.AddAuthentication(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ConserveConnectionsDirective) then
    Self.SetConserveConnections(UserAgent, ConfigurationLine, PendingActions)
  else if IsEqual(FirstToken, FromDirective) then
    Self.AddFrom(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, HostNameDirective) then
    Self.AddHostName(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, InstanceIDDirective) then
    Self.SetInstanceID(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ListenDirective) then
    Self.AddTransport(UserAgent.Dispatcher, ConfigurationLine)
  else if IsEqual(FirstToken, MappedRouteDirective) then
    Self.AddMappedRoute(UserAgent, ConfigurationLine, PendingActions)
  else if IsEqual(FirstToken, MockDnsDirective) then
    Self.AddMockDnsEntry(UserAgent, ConfigurationLine, PendingActions)
  else if IsEqual(FirstToken, MockLocalAddressDirective) then
    Self.AddMockLocalAddress(UserAgent, ConfigurationLine, PendingActions)
  else if IsEqual(FirstToken, MockRouteDirective) then
    Self.AddMockRoute(UserAgent, ConfigurationLine, PendingActions)
  else if IsEqual(FirstToken, NameServerDirective) then
    Self.AddLocator(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, RegisterDirective) then
    Self.RegisterUA(UserAgent, ConfigurationLine, PendingActions)
  else if IsEqual(FirstToken, RegistrarDatabaseDirective) then
    Self.SetRegistrarDatabase(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, RegistrarUseGruuDirective) then
    Self.SetRegistrarUseGruu(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, ResolveNamesLocallyFirstDirective) then
    Self.UseLocalResolution(UserAgent, ConfigurationLine, PendingActions)
  else if IsEqual(FirstToken, RouteHeaderDirective) then
    Self.AddRouteHeader(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, RoutingTableDirective) then
    Self.AddRoutingTable(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, SupportEventDirective) then
    Self.AddSupportForEventPackage(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, SuppressLocalResponsesDirective) then
    Self.SetSuppressLocalResponses(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, UseGruuDirective) then
    Self.UseGruu(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, UseInboundConnectionsDirective) then
    Self.UseInboundConnections(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, UserAgentNameDirective) then
    Self.UserAgentName(UserAgent, ConfigurationLine)
  else if IsEqual(FirstToken, UseTransportDirective) then
    Self.AddUseTransport(UserAgent, ConfigurationLine);
end;

procedure TIdSipStackConfigurator.PostConfigurationActions(UA: TIdSipUserAgent);
var
  RegMod: TIdSipRegisterModule;
begin
  if UA.UsesModule(TIdSipRegisterModule) then begin
    RegMod := UA.ModuleFor(TIdSipRegisterModule) as TIdSipRegisterModule;

    if not Assigned(RegMod.BindingDB) then
      RegMod.BindingDB := TIdSipInMemoryBindingDatabase.Create;
  end;
end;

procedure TIdSipStackConfigurator.PreConfigurationActions(UA: TIdSipUserAgent);
var
  RegMod: TIdSipRegisterModule;
begin
  if UA.UsesModule(TIdSipRegisterModule) then begin
    RegMod := UA.ModuleFor(TIdSipRegisterModule) as TIdSipRegisterModule;

    if not Assigned(RegMod.BindingDB) then
      RegMod.BindingDB := TIdSipInMemoryBindingDatabase.Create;
  end;
end;

procedure TIdSipStackConfigurator.RegisterUA(UserAgent: TIdSipUserAgent;
                                             const RegisterLine: String;
                                             PendingActions: TObjectList);
var
  Line:         String;
  Registrar:    TIdSipUri;
  Registration: TIdSipPendingRegistration;
begin
  // See class comment for the format for this directive.
  Line := RegisterLine;
  EatDirective(Line);

  Line := Trim(Line);

  Registrar := TIdSipUri.Create(Line);
  try
    Registration := TIdSipPendingRegistration.Create(UserAgent, Registrar);

    PendingActions.Add(Registration);
  finally
    Registrar.Free;
  end;
end;

procedure TIdSipStackConfigurator.SetConserveConnections(UserAgent: TIdSipUserAgent;
                                                         const ConserveConnectionsLine: String;
                                                         PendingActions: TObjectList);
var
  Line: String;
  Setter: TIdSipPendingTcpTransportConfiguration;
begin
  Line := ConserveConnectionsLine;
  EatDirective(Line);

  Setter := TIdSipPendingTcpTransportConfiguration.Create(UserAgent);
  Setter.ConserveConnections := StrAsBool(Line);

  PendingActions.Add(Setter);
end;

procedure TIdSipStackConfigurator.SetRegistrarDatabase(UserAgent: TIdSipUserAgent;
                                                       const RegistrarDatabaseLine: String);
var
  DatabaseType: String;
  Line:         String;
  RegMod:       TIdSipRegisterModule;
begin
  Line := RegistrarDatabaseLine;
  EatDirective(Line);

  RegMod := Self.InstantiateRegistrarModule(UserAgent);

  DatabaseType := Fetch(Line, ';');

  if IsEqual(DatabaseType, MockKeyword) then begin
    if IsEqual(Line, MatchOnlyUsernameOption) then
      RegMod.BindingDB := TIdSipNameMatchingMockBindingDatabase.Create
    else
      RegMod.BindingDB := TIdSipInMemoryBindingDatabase.Create;
  end;

  // Insert your special database type creation code here.

  // Fallback position: it means someone specified an unknown database type.
  if not Assigned(RegMod.BindingDB) then
    RegMod.BindingDB := TIdSipInMemoryBindingDatabase.Create;
end;

procedure TIdSipStackConfigurator.SetRegistrarUseGruu(UserAgent: TIdSipUserAgent;
                                                      const UseGruuLine: String);
var
  Line:   String;
  RegMod: TIdSipRegisterModule;
begin
  Line := UseGruuLine;
  EatDirective(Line);

  RegMod := Self.InstantiateRegistrarModule(UserAgent);

  RegMod.UseGruu := StrAsBool(Line);
end;

procedure TIdSipStackConfigurator.SetSuppressLocalResponses(UserAgent: TIdSipUserAgent;
                                                            const SuppressLocalResponsesLine: String);
var
  Line:   String;
  InviteMod: TIdSipInviteModule;
begin
  Line := SuppressLocalResponsesLine;
  EatDirective(Line);

  InviteMod := Self.InstantiateInviteModule(UserAgent);

  InviteMod.SuppressLocalResponses := StrAsBool(Line);
end;

procedure TIdSipStackConfigurator.UseGruu(UserAgent: TIdSipAbstractCore;
                                          const UseGruuLine: String);
var
  Line: String;
begin
  Line := UseGruuLine;
  EatDirective(Line);

  UserAgent.UseGruu := StrAsBool(Line);
end;

procedure TIdSipStackConfigurator.UseInboundConnections(UserAgent: TIdSipAbstractCore;
                                                        const UseInboundConnectionsLine: String);
var
  Line: String;
begin
  Line := UseInboundConnectionsLine;
  EatDirective(Line);

  UserAgent.UseInboundConnections := StrToBool(Line);
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

  Pending := TIdSipPendingLocalResolutionAction.Create(UserAgent, StrAsBool(Line));
  Self.AddPendingConfiguration(PendingActions, Pending);
end;

procedure TIdSipStackConfigurator.UserAgentName(UserAgent: TIdSipAbstractCore;
                                                const UserAgentNameLine: String);
var
  Line: String;
begin
  Line := UserAgentNameLine;
  EatDirective(Line);

  UserAgent.UserAgentName := Line;
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
//* TIdSipUserAgentFactory                                                     *
//******************************************************************************
//* TIdSipUserAgentFactory Public methods **************************************

constructor TIdSipUserAgentFactory.Create;
begin
  inherited Create;

  Self.fConfiguration := TStringList.Create;
  Self.fListeners     := TIdSipTransactionUserListeners.Create;
end;

destructor TIdSipUserAgentFactory.Destroy;
begin
  Self.fListeners.Free;
  Self.fConfiguration.Free;

  inherited Destroy;
end;

function TIdSipUserAgentFactory.Copy: TIdSipUserAgentFactory;
begin
  Result := TIdSipUserAgentFactoryClass(Self.ClassType).Create;
  Result.Configuration := Self.Configuration;
  Result.Listeners     := Self.Listeners;
  Result.Timer         := Self.Timer;
end;

function TIdSipUserAgentFactory.CreateUserAgent: TIdSipUserAgent;
var
  C: TIdSipStackConfigurator;
begin
  C := TIdSipStackConfigurator.Create;
  try
    Result := C.CreateUserAgent(Self.Configuration, Self.Timer, Self.Listeners.AsArray);
  finally
    C.Free;
  end;
end;

//* TIdSipUserAgentFactory Private methods *************************************

procedure TIdSipUserAgentFactory.SetConfiguration(Value: TStrings);
begin
  Self.fConfiguration.Clear;
  Self.fConfiguration.AddStrings(Value);
end;

procedure TIdSipUserAgentFactory.SetListeners(Value: TIdSipTransactionUserListeners);
begin
  Self.Listeners.Clear;
  Self.Listeners.Add(Value);
end;

//******************************************************************************
//* TIdSipPendingConfigurationAction                                           *
//******************************************************************************
//* TIdSipPendingConfigurationAction Public methods ****************************

procedure TIdSipPendingConfigurationAction.Execute;
begin
  RaiseAbstractError(Self.ClassName, 'Execute');
end;

//******************************************************************************
//* TIdSipPendingCoreConfigurationAction                                       *
//******************************************************************************
//* TIdSipPendingCoreConfigurationAction Public methods ************************

constructor TIdSipPendingCoreConfigurationAction.Create(Core: TIdSipAbstractCore);
begin
  inherited Create;

  Self.fCore := Core;
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
//* TIdSipPendingMappedRouteAction                                             *
//******************************************************************************
//* TIdSipPendingMappedRouteAction Public methods ******************************

procedure TIdSipPendingMappedRouteAction.Execute;
begin
  Self.Core.RoutingTable.AddMappedRoute(Self.Network, Self.Mask, Self.Gateway, Self.MappedPort);
end;

//******************************************************************************
//* TIdSipPendingMockLocalAddressAction                                        *
//******************************************************************************
//* TIdSipPendingMockLocalAddressAction Public methods *************************

procedure TIdSipPendingMockLocalAddressAction.Execute;
begin
  if (Self.Core.RoutingTable is TIdMockRoutingTable) then
    (Self.Core.RoutingTable as TIdMockRoutingTable).AddLocalAddress(Self.Address);
end;

//******************************************************************************
//* TIdSipPendingMockDnsARecordAction                                          *
//******************************************************************************
//* TIdSipPendingMockDnsARecordAction Public methods ***************************

procedure TIdSipPendingMockDnsARecordAction.Execute;
begin
  if (Self.Core.Locator is TIdSipMockLocator) then
    (Self.Core.Locator as TIdSipMockLocator).AddA(Self.HostName, Self.IPAddress);
end;

//******************************************************************************
//* TIdSipPendingMockDnsAAAARecordAction                                       *
//******************************************************************************
//* TIdSipPendingMockDnsAAAARecordAction Public methods ************************

procedure TIdSipPendingMockDnsAAAARecordAction.Execute;
begin
  if (Self.Core.Locator is TIdSipMockLocator) then
    (Self.Core.Locator as TIdSipMockLocator).AddAAAA(Self.HostName, Self.IPAddress);
end;

//******************************************************************************
//* TIdSipPendingMockDnsNAPTRRecordAction                                      *
//******************************************************************************
//* TIdSipPendingMockDnsNAPTRRecordAction Public methods ***********************

procedure TIdSipPendingMockDnsNAPTRRecordAction.Execute;
begin
  if (Self.Core.Locator is TIdSipMockLocator) then
    (Self.Core.Locator as TIdSipMockLocator).AddNAPTR(Self.Key,
                                                      Self.Order,
                                                      Self.Preference,
                                                      Self.Flags,
                                                      Self.Service,
                                                      Self.Replacement);
end;

//******************************************************************************
//* TIdSipPendingMockDnsSRVRecordAction                                        *
//******************************************************************************
//* TIdSipPendingMockDnsSRVRecordAction Public methods *************************

procedure TIdSipPendingMockDnsSRVRecordAction.Execute;
begin
  if (Self.Core.Locator is TIdSipMockLocator) then
    (Self.Core.Locator as TIdSipMockLocator).AddSRV(Self.Domain,
                                                    Self.Service,
                                                    Self.Priority,
                                                    Self.Weight,
                                                    Self.Port,
                                                    Self.Target);
end;

//******************************************************************************
//* TIdSipPendingMockRouteAction                                               *
//******************************************************************************
//* TIdSipPendingMockRouteAction Public methods ********************************

procedure TIdSipPendingMockRouteAction.Execute;
var
  RT: TIdMockRoutingTable;
begin
  RT := Self.Core.RoutingTable as TIdMockRoutingTable;

  RT.AddOsRoute(Self.Destination,
                Self.Mask,
                Self.Gateway,
                Self.Metric,
                Self.InterfaceIndex,
                Self.LocalAddress);
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
//* TIdSipPendingRegistration                                                  *
//******************************************************************************
//* TIdSipPendingRegistration Public methods ***********************************

constructor TIdSipPendingRegistration.Create(UA: TIdSipUserAgent; Registrar: TIdSipUri);
begin
  inherited Create;

  Self.fRegistrar := TIdSipUri.Create(Registrar.Uri);
  Self.fUA        := UA;
end;

procedure TIdSipPendingRegistration.Execute;
var
  PlaceholderContact: TIdSipContactHeader;
  Reg:                TIdSipOutboundRegisterModule;
  Wait:               TIdSipReregisterWait;
begin
  Reg := Self.UA.RegisterModule;

  Reg.AutoReRegister := true;
  Reg.HasRegistrar   := true;
  Reg.Registrar      := Self.Registrar;

  PlaceholderContact := TIdSipContactHeader.Create;
  try
    PlaceholderContact.Value  := Self.UA.From.FullValue;

    if UA.UseGruu then
      PlaceholderContact.SipInstance := Self.UA.InstanceID;

    Wait := TIdSipReregisterWait.Create;
    Wait.AddressOfRecord  := Self.UA.From;
    Wait.RegisterModuleID := Reg.ID;
    Wait.Registrar        := Self.Registrar;
    // We're not actually going to register the From: the Contact header will be
    // rewritten on its way to the network.
    Wait.Bindings.Add(PlaceholderContact);
  finally
    PlaceholderContact.Free;
  end;

  Self.UA.Timer.AddEvent(TriggerImmediately, Wait);
end;

//******************************************************************************
//* TIdSipPendingTcpTransportConfiguration                                     *
//******************************************************************************
//* TIdSipPendingTcpTransportConfiguration Public methods **********************

procedure TIdSipPendingTcpTransportConfiguration.Execute;
var
  I: Integer;
begin
  for I := 0 to Self.Core.Dispatcher.TransportCount - 1 do
    Self.Core.Dispatcher.Transports[I].ConserveConnections := Self.ConserveConnections;
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
begin
  TIdObjectRegistry.Singleton.WithExtantObjectDo(Self.UserAgentID, Self.ActOnUserAgent);
end;

//* TIdSipReconfigureStackWait Private methods *********************************

procedure TIdSipReconfigureStackWait.ActOnUserAgent(O: TObject);
var
  Configurator: TIdSipStackConfigurator;
begin
  if not (O is TIdSipUserAgent) then
    Exit;

  Configurator := TIdSipStackConfigurator.Create;
  try
    Configurator.UpdateConfiguration(O as TIdSipUserAgent, Self.Configuration);
  finally
    Configurator.Free;
  end;
end;

procedure TIdSipReconfigureStackWait.SetConfiguration(Value: TStrings);
begin
  Self.fConfiguration.Assign(Value);
end;

end.
