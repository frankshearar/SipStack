{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSdp;

interface

uses
  Classes, IdConnectionBindings, IdInterfacedObject, IdRTP, IdRTPServer, IdSdp,
  IdSimpleParser, IdTcpClient, IdTcpServer, IdTimerQueue, IdUDPServer, SyncObjs,
  SysUtils, TestFramework, TestFrameworkEx, TestFrameworkRtp;

type
  TIdSdpTestMediaListener = class(TIdInterfacedObject,
                                  IIdSdpMediaListener,
                                  IIdSdpMediaSendListener)
  private
    fBindingParam: TIdConnectionBindings;
    fChunkParam:   TStream;
    fFormatParam:  String;
    fLayerIDParam: Integer;
    fReceivedData: Boolean;
    fSentData:     Boolean;
    fStreamParam:  TIdSdpBaseMediaStream;

    procedure OnData(Stream: TIdSdpBaseMediaStream;
                     Chunk: TStream;
                     Format: String;
                     Binding: TIdConnectionBindings);
    procedure OnSentData(Stream: TIdSdpBaseMediaStream;
                         Chunk: TStream;
                         Format: String;
                         LayerID: Integer);
  public
    constructor Create; override;

    property BindingParam: TIdConnectionBindings read fBindingParam;
    property ChunkParam:   TStream               read fChunkParam;
    property FormatParam:  String                read fFormatParam;
    property LayerIDParam: Integer               read fLayerIDParam;
    property ReceivedData: Boolean               read fReceivedData;
    property SentData:     Boolean               read fSentData;
    property StreamParam:  TIdSdpBaseMediaStream read fStreamParam;
  end;

  TIdSdpTestConnectionListener = class(TIdInterfacedObject,
                                       IIdSdpTcpConnectionListener)
  private
    fConnectionParam:    TIdSdpBaseTcpConnection;
    fDataParam:          TStream;
    fOnExceptionCalled:  Boolean;
    fExceptionMessage:   String;
    fExceptionType:      ExceptClass;
    fOnConnectCalled:    Boolean;
    fOnDataCalled:       Boolean;
    fOnDisconnectCalled: Boolean;

    procedure OnConnect(Connection: TIdSdpBaseTcpConnection);
    procedure OnData(Connection: TIdSdpBaseTcpConnection;
                     Data: TStream);
    procedure OnDisconnect(Connection: TIdSdpBaseTcpConnection);
    procedure OnException(Connection: TIdSdpBaseTcpConnection;
                          ExceptionType: ExceptClass;
                          ExceptionMessage: String);
  public
    constructor Create; override;
    destructor  Destroy; override;

    property ConnectionParam:    TIdSdpBaseTcpConnection read fConnectionParam;
    property DataParam:          TStream                 read fDataParam;
    property ExceptionMessage:   String                  read fExceptionMessage;
    property ExceptionType:      ExceptClass             read fExceptionType;
    property OnConnectCalled:    Boolean                 read fOnConnectCalled;
    property OnDataCalled:       Boolean                 read fOnDataCalled;
    property OnDisconnectCalled: Boolean                 read fOnDisconnectCalled;
    property OnExceptionCalled:  Boolean                 read fOnExceptionCalled;
  end;

  TestFunctions = class(TTestCase)
  published
    procedure TestAddressTypeToStr;
    procedure TestAppropriateSetupType;
    procedure TestBandwidthTypeToStr;
    procedure TestConnectionTypeToStr;
    procedure TestDirectionToStr;
    procedure TestKeyTypeToStr;
    procedure TestMediaTypeToStr;
    procedure TestSetupTypeToStr;
    procedure TestStrToAddressType;
    procedure TestStrToBandwidthType;
    procedure TestStrToConnectionType;
    procedure TestStrToDirection;
    procedure TestStrToKeyType;
    procedure TestStrToMediaType;
    procedure TestStrToSetupType;
  end;

  TestTIdSdpAttribute = class(TTestCase)
  private
    A: TIdSdpAttribute;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestCopy;
    procedure TestCreateAttribute;
    procedure TestEquals;
    procedure TestIsRTPMap;
    procedure TestPrintOnNoValue;
    procedure TestPrintOnWithValue;
  end;

  TestTIdSdpRTPMapAttribute = class(TTestCase)
  private
    A: TIdSdpRTPMapAttribute;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestGetName;
    procedure TestGetValue;
    procedure TestIsRTPMap;
    procedure TestSetValue;
    procedure TestSetValueWithParameters;
  end;

  TestTIdSdpBandwidth = class(TTestCase)
  private
    B: TIdSdpBandwidth;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAssignWithUnknownBandwidthType;
    procedure TestPrintOn;
    procedure TestPrintOnUnknownBandwidthType;
  end;

  TestTIdSdpConnection = class(TTestCase)
  private
    C: TIdSdpConnection;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddressRoutableAddress;
    procedure TestAssign;
    procedure TestCopy;
    procedure TestPrintOnMulticast;
    procedure TestPrintOnMulticastWithNumberNoTtl;
    procedure TestPrintOnMulticastWithTtl;
    procedure TestPrintOnMulticastWithTtlAndNumber;
    procedure TestPrintOnUnicast;
    procedure TestPrintOnUsesRoutableAddress;
  end;

  TestTIdSdpKey = class(TTestCase)
  private
    K: TIdSdpKey;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAssignWithUnknownKeyType;
    procedure TestPrintOnJustMethod;
    procedure TestPrintOnMethodPlusValue;
    procedure TestPrintOnPromptWithKeyData;
    procedure TestPrintOnUnknownKeyType;
    procedure TestPrintOnUriWithNoKeyData;
  end;

  TestTIdSdpMediaDescription = class(TTestCase)
  private
    M: TIdSdpMediaDescription;

    procedure ConfigureComplicatedly(Desc: TIdSdpMediaDescription);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAttributeAndCount;
    procedure TestAddRTPMapAttribute;
    procedure TestAddFormatAndFormatCount;
    procedure TestAssign;
    procedure TestAssignUnknownMediaType;
    procedure TestEquals;
    procedure TestFormatClear;
    procedure TestGetFormat;
    procedure TestHasFormat;
    procedure TestInitialState;
    procedure TestIsRefusedStream;
    procedure TestIsText;
    procedure TestIsValidFormatRtpAvp;
    procedure TestIsValidFormatRtpSavp;
    procedure TestIsValidFormatTcp;
    procedure TestIsValidFormatUdp;
    procedure TestMediaTypeAsString;
    procedure TestPrintOnBasic;
    procedure TestPrintOnFull;
    procedure TestPrintOnWithPortCount;
    procedure TestPrintOnWithUnknownMediaType;
    procedure TestUsesBinding;
    procedure TestUsesRtpProtocol;
  end;

  TestTIdSdpOrigin = class(TTestCase)
  private
    O: TIdSdpOrigin;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestPrintOn;
    procedure TestUsernameEncode;
    procedure TestUsernameWithSpaces;
  end;

  TestTIdSdpRepeat = class(TTestCase)
  private
    R: TIdSdpRepeat;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestPrintOn;
  end;

  TestTIdSdpTime = class(TTestCase)
  private
    T: TIdSdpTime;
    S: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestPrintOn;
    procedure TestPrintOnWithRepeats;
    procedure TestPrintOnWithZoneAdjustments;
  end;

  TestTIdSdpAttributes = class(TTestCase)
  private
    A: TIdSdpAttributes;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddMultipleAttributes;
    procedure TestAddUsingString;
    procedure TestAssign;
    procedure TestClear;
    procedure TestDirection;
    procedure TestEquals;
    procedure TestHasAttribute;
    procedure TestHasAttributeNamed;
    procedure TestPrintOn;
    procedure TestSetDirection;
    procedure TestSetSetupType;
    procedure TestSetupType;
    procedure TestInitialSetDirection;
    procedure TestValueFor;
  end;

  TestTIdSdpRTPMapAttributes = class(TTestCase)
  private
    A: TIdSdpRTPMapAttributes;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddMultipleAttributes;
    procedure TestAssign;
    procedure TestClear;
    procedure TestEncodingFor;
    procedure TestEquals;
    procedure TestFormatFor;
    procedure TestPrintOn;
  end;

  TestTIdSdpBandwidths = class(TTestCase)
  private
    B: TIdSdpBandwidths;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddMultipleBandwidths;
    procedure TestAssign;
    procedure TestClear;
    procedure TestPrintOn;
  end;

  TestTIdSdpConnections = class(TTestCase)
  private
    C: TIdSdpConnections;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAddConnection;
    procedure TestAddMultipleConnections;
    procedure TestAssign;
    procedure TestClear;
    procedure TestPrintOn;
  end;

  TestTIdSdpMediaDescriptions = class(TTestCase)
  private
    M: TIdSdpMediaDescriptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAssign;
    procedure TestAllDescriptionsHaveConnections;
    procedure TestClear;
    procedure TestPrintOn;
  end;

  TestTIdSdpRepeats = class(TTestCase)
  private
    R: TIdSdpRepeats;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAssign;
    procedure TestClear;
    procedure TestPrintOn;
  end;

  TestTIdSdpTimes = class(TTestCase)
  private
    T: TIdSdpTimes;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAssign;
    procedure TestClear;
    procedure TestPrintOn;
  end;

  TestTIdSdpZoneAdjustments = class(TTestCase)
  private
    Z: TIdSdpZoneAdjustments;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAssign;
    procedure TestClear;
    procedure TestPrintOn;
  end;

  TestTIdSdpPayload = class(TTestCase)
  private
    Payload: TIdSdpPayload;

    procedure SetToMinimumPayload(P: TIdSdpPayload);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddConnection;
    procedure TestAsString;
    procedure TestConnectionAt;
    procedure TestConnectionCount;
    procedure TestCreateFromStream;
    procedure TestCreateFromStreamString;
    procedure TestEquals;
    procedure TestEqualsDifferentAttributes;
    procedure TestEqualsDifferentBandwidths;
    procedure TestEqualsDifferentEmailAddress;
    procedure TestEqualsDifferentInfo;
    procedure TestEqualsDifferentKey;
    procedure TestEqualsDifferentOrigin;
    procedure TestEqualsDifferentPhoneNumber;
    procedure TestEqualsDifferentRTPMapAttributes;
    procedure TestEqualsDifferentSessionName;
    procedure TestEqualsDifferentTimes;
    procedure TestEqualsDifferentUri;
    procedure TestEqualsDifferentVersion;
    procedure TestGetRtpMapAttributes;
    procedure TestInitializeOnEmptySdpPayload;
    procedure TestInitializeOnSingleMediaSdp;
    procedure TestMediaDescriptionAt;
    procedure TestMediaDescriptionCount;
    procedure TestMediaDescriptionGetsSessionConnections;
    procedure TestMediaDescriptionGetsSetupTypeAttribute;
    procedure TestMimeType;
    procedure TestNewSessionConnectionGetsCopiedToMediaDescriptions;
    procedure TestNoSessionNamePrintsDash;
    procedure TestPrintOnBasic;
    procedure TestPrintOnWithAttributes;
    procedure TestPrintOnWithBandwidth;
    procedure TestPrintOnWithEmail;
    procedure TestPrintOnWithInfo;
    procedure TestPrintOnWithKey;
    procedure TestPrintOnWithMediaDescriptions;
    procedure TestPrintOnWithPhoneNumber;
    procedure TestPrintOnWithTimes;
    procedure TestPrintOnWithUri;
    procedure TestReadFromString;
    procedure TestReadFromStringEmpty;
    procedure TestRemoveLastMediaDescription;
    procedure TestSetConnectionAddress;
  end;

  TestTIdSdpParser = class(TTestCase)
  private
    P:       TIdSdpParser;
    Payload: TIdSdpPayload;

    procedure CheckMalformedAttribute(const Value: String);
    procedure CheckMalformedConnection(const Value: String);
    procedure CheckMalformedOrigin(const OriginValue: String);
    procedure CheckMalformedPhoneNumber(const Value: String);
    procedure CheckMalformedKey(const KeyValue: String);
    procedure CheckMalformedMediaDescription(const Value: String);
    procedure CheckMalformedOptionalSessionHeader(const Name, Value: String);
    procedure CheckMalformedTimeWithRepeat(const RepeatValue: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsAddressType;
    procedure TestIsBandwidthType;
    procedure TestIsByteString;
    procedure TestIsDirection;
    procedure TestIsKeyData;
    procedure TestIsKeyType;
    procedure TestIsMediaType;
    procedure TestIsMimeType;
    procedure TestIsMulticastAddress;
    procedure TestIsNetType;
    procedure TestIsPhone;
    procedure TestIsPhoneNumber;
    procedure TestIsPort;
    procedure TestIsProtocol;
    procedure TestIsText;
    procedure TestIsTime;
    procedure TestIsToken;
    procedure TestParseAttribute;
    procedure TestParseAttributeWithValue;
    procedure TestParseAttributeMalformedName;
    procedure TestParseAttributeMalformedValue;
    procedure TestParseBandwidth;
    procedure TestParseBandwidthMalformed;
    procedure TestParseBandwidthMultipleHeaders;
    procedure TestParseBandwidthUnknown;
    procedure TestParseConnectionInSessionAndMediaDescription;
    procedure TestParseConnectionMalformed;
    procedure TestParseConnectionMalformedMulticastAddress;
    procedure TestParseConnectionMalformedUnicastAddress;
    procedure TestParseConnectionMulticastAddress;
    procedure TestParseConnectionMulticastAddressWithMultiple;
    procedure TestParseConnectionMulticastIPv6AddressWithMultiple;
    procedure TestParseConnectionUnknownAddrType;
    procedure TestParseConnectionUnknownNetType;
    procedure TestParseConnectionUnicastAddress;
    procedure TestParseEmail;
    procedure TestParseEmailBracketedName;
    procedure TestParseEmailRfc822;
    procedure TestParseEmptyStream;
    procedure TestParseHeaderMissingName;
    procedure TestParseHeaderMissingValue;
    procedure TestParseHeadersInvalidOrder;
    procedure TestParseInfo;
    procedure TestParseInfoIllegalCharacters;
    procedure TestParseKey;
    procedure TestParseKeyMalformedPrompt;
    procedure TestParseKeyMalformedClear;
    procedure TestParseKeyMalformedBase64;
    procedure TestParseKeyMalformedUri;
    procedure TestParseKeyUnknownKeyType;
    procedure TestParseKeyUnknownKeyTypeWithValue;
    procedure TestParseLinphoneSessionDescription;
    procedure TestParseMediaDescription;
    procedure TestParseMediaDescriptionWithSessionAttributes;
    procedure TestParseMediaDescriptionWithSessionConnections;
    procedure TestParseMediaDescriptionMalformedFormatList;
    procedure TestParseMediaDescriptionNonRtpFormatList;
    procedure TestParseMediaDescriptionMalformedPort;
    procedure TestParseMediaDescriptionMalformedTransport;
    procedure TestParseMediaDescriptionMissingFormatList;
    procedure TestParseMediaDescriptionMissingInformation;
    procedure TestParseMediaDescriptionMissingKey;
    procedure TestParseMediaDescriptionMissingPort;
    procedure TestParseMediaDescriptionsMissingSessionConnection;
    procedure TestParseMediaDescriptions;
    procedure TestParseMediaDescriptionUnknownBandwidthType;
    procedure TestParseMediaDescriptionUnknownMediaType;
    procedure TestParseMediaDescriptionWithAttributes;
    procedure TestParseMediaDescriptionWithBandwidth;
    procedure TestParseMediaDescriptionWithConnection;
    procedure TestParseMediaDescriptionWithKey;
    procedure TestParseMediaDescriptionWithMultipleFormats;
    procedure TestParseMediaDescriptionWithPortCount;
    procedure TestParseMinimumPayload;
    procedure TestParseMissingOrigin;
    procedure TestParseMissingSession;
    procedure TestParseMissingSessionConnection;
    procedure TestParseMissingVersion;
    procedure TestParseOriginIPv6Address;
    procedure TestParseOriginMalformed;
    procedure TestParseOriginMalformedUsername;
    procedure TestParseOriginMalformedSessionID;
    procedure TestParseOriginMalformedSessionVersion;
    procedure TestParseOriginMalformedNetType;
    procedure TestParsePhoneNumber;
    procedure TestParsePhoneNumberMultipleHeaders;
    procedure TestParsePhoneNumberWithAngleBracketsButNoName;
    procedure TestParsePhoneNumberWithStuffOutsideComment;
    procedure TestParsePhoneNumberWithUnsafeChars;
    procedure TestParseRedefinedPayloadType;
    procedure TestParseSessionIllegalCharacters;
    procedure TestParseSomeMediaDescriptionsLackConnectionAndNoSessionConnection;
    procedure TestParseTimeMultipleHeaders;
    procedure TestParseTimeSingleBounded;
    procedure TestParseTimeSingleUnbounded;
    procedure TestParseTimeSingleUnboundedEndTime;
    procedure TestParseTimeWithMalformedMultipleRepeat;
    procedure TestParseTimeWithMalformedSingleRepeat;
    procedure TestParseTimeWithRepeatAndZoneAdjustment;
    procedure TestParseTimeWithRepeatBeforeZoneAdjustment;
    procedure TestParseTimeWithSingleRepeat;
    procedure TestParseTimeWithSingleTypedRepeat;
    procedure TestParseTimeWithSingleZoneAdjustment;
    procedure TestParseUri;
    procedure TestParseVersionMalformed;
    procedure TestParseVersionMultipleHeaders;
  end;

  TIdSdpTestCase = class(TTestCase,
                         IIdSdpMediaListener,
                         IIdSdpMediaSendListener)
  protected
    ReceivedData: Boolean;
    SentData:     Boolean;

    procedure CheckDataSent(Msg: String);
    procedure CheckNoDataSent(Msg: String);
    procedure OnData(Stream: TIdSdpBaseMediaStream;
                     Chunk: TStream;
                     Format: String;
                     Binding: TIdConnectionBindings); virtual;
    procedure OnSentData(Stream: TIdSdpBaseMediaStream;
                         Chunk: TStream;
                         Format: String;
                         LayerID: Integer); virtual;
  public
    procedure SetUp; override;

    procedure CheckPortActive(Address: String;
                              Port: Cardinal;
                              Msg: String); virtual;
    procedure CheckPortFree(Address: String;
                              Port: Cardinal;
                              Msg: String); virtual;
  end;

  TestTIdSdpBaseMediaStream = class(TIdSdpTestCase)
  protected
    DataFormat:     String;
    Desc:           TIdSdpPayload;
    SendingBinding: TIdConnectionBindings;
    Timer:          TIdDebugTimerQueue;

    procedure AddSendingChecking(Stream: TIdSdpBaseMediaStream); virtual;
    procedure Activate(Stream: TIdSdpBaseMediaStream); virtual;
    function  BasicMediaDesc(Port: Cardinal = 8000; PortCount: Cardinal = 1): String; virtual;
    procedure CheckPortAdjusted(Expected, Received: Cardinal; IsNullStream: Boolean; StreamClass: String);
    function  CreateStream: TIdSdpBaseMediaStream; virtual;
    function  InactiveMediaDesc: String;
    function  LocalDescription: TIdSdpMediaDescription;
    procedure ReceiveDataOn(S: TIdSdpBaseMediaStream); virtual;
    function  RefusedMediaDesc: String;
    function  RemoteDescription: TIdSdpMediaDescription;
    procedure RemoveSendingChecking(Stream: TIdSdpBaseMediaStream); virtual;
    procedure SendData(Stream: TIdSdpBaseMediaStream); virtual;
    procedure SetLocalMediaDesc(Stream: TIdSdpBaseMediaStream;
                                const MediaDesc: String);
    procedure SetRemoteMediaDesc(Stream: TIdSdpBaseMediaStream;
                                 const MediaDesc: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddDataListener;
    procedure TestAddDataSendListener;
    procedure TestIsReceiver;
    procedure TestIsSender;
    procedure TestPutOnHoldRecvOnly;
    procedure TestPutOnHoldSendRecv;
    procedure TestPutOnHoldWhileOnHold;
    procedure TestSendData; virtual;
    procedure TestSendDataWhenNotSender;
    procedure TestSetTimer; virtual;
    procedure TestStartListeningPortAboveAllowedRange; virtual;
    procedure TestStartListeningPortBelowAllowedRange; virtual;
    procedure TestStartListeningRefusedStream;
    procedure TestTakeOffHold;
    procedure TestUnusedPortsSwitchOff; virtual;
  end;

  TestTIdSDPMediaStream = class(TestTIdSdpBaseMediaStream,
                                IIdRTPSendListener)
  private
    AVP:            TIdRTPProfile;
    Media:          TIdSDPMediaStream;
    Sender:         TIdSDPMediaStream;
    SentBye:        Boolean;
    SentData:       Boolean;
    SentControl:    Boolean;
    Text:           TStream;

    procedure OnSendRTCP(Packet: TIdRTCPPacket;
                         Binding: TIdConnectionBindings);
    procedure OnSendRTP(Packet: TIdRTPPacket;
                        Binding: TIdConnectionBindings);
    procedure ReceiveControlOn(S: TIdSDPMediaStream);
    function  T140PT: TIdRTPPayloadType;
  protected
    function  BasicMediaDesc(Port: Cardinal = 8000; PortCount: Cardinal = 1): String; override;
    function  CreateStream: TIdSdpBaseMediaStream; override;
    procedure ReceiveDataOn(S: TIdSdpBaseMediaStream); override;
    procedure SendData(Stream: TIdSdpBaseMediaStream); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddRTPListener;
    procedure TestAddRTPSendListener;
    procedure TestHierarchicallyEncodedStream;
    procedure TestLayeredCodecAddressesAndPorts;
    procedure TestMatchPort;
    procedure TestPortsAndPortCount;
    procedure TestReceiveDataWhenNotReceiver;
    procedure TestRemoteMembersControlAddressAndPortSet;
    procedure TestRemoveDataListener;
    procedure TestRemoveDataSendListener;
    procedure TestRemoveRTPSendListener;
    procedure TestRTPListenersGetRTCP;
    procedure TestRTPListenersGetRTP;
    procedure TestSetRemoteDescriptionSendsNoPackets;
    procedure TestSetRemoteDescriptionRegistersRemoteRtpMaps;
    procedure TestSetTimer; override;
    procedure TestStartListening;
    procedure TestStartListeningRegistersLocalRtpMaps;
    procedure TestStartListeningTriesConsecutivePorts;
    procedure TestStopListeningStopsListening;
    procedure TestUnusedPortsSwitchOff; override;
  end;

  TestTIdSdpNullMediaStream = class(TestTIdSdpBaseMediaStream)
  protected
    function  BasicMediaDesc(Port: Cardinal = 8000; PortCount: Cardinal = 1): String; override;
    function  CreateStream: TIdSdpBaseMediaStream; override;
    procedure ReceiveDataOn(S: TIdSdpBaseMediaStream); override;
    procedure SendData(Stream: TIdSdpBaseMediaStream); override;
  published
    procedure TestSendData; override;
    procedure TestSetTimer; override;
  end;

  TestTIdSdpTcpConnectionRegistry = class(TTestCase)
  private
    Agent: TIdSdpMockTcpConnection;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClientConnectedTo;
    procedure TestFindConnection;
    procedure TestFindConnectionNoSuchRegisteredObject;
    procedure TestFindConnectionNotATcpConnection;
    procedure TestServerOn;
    procedure TestServerRunningOn;
  end;

  TestTIdSdpMockTcpConnection = class(TTestCase)
  private
    Conn:       TIdSdpMockTcpConnection;
    Data:       TStringStream;
    ReceivedOn: TIdConnectionBindings;
  protected
    procedure Activate(Connection: TIdSdpMockTcpConnection); virtual;
    function  CreateConnection: TIdSdpMockTcpConnection; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddDataListener; virtual;
    procedure TestDisconnect; virtual;
    procedure TestForceDisconnect; virtual;
    procedure TestRemotePartyAccepts; virtual;
    procedure TestRemoveDataListener; virtual;
    procedure TestSendData; virtual;
  end;

  TestTIdSdpMockTcpNullConnection = class(TestTIdSdpMockTcpConnection)
  protected
    procedure Activate(Connection: TIdSdpMockTcpConnection); override;
    function  CreateConnection: TIdSdpMockTcpConnection; override;
  published
    procedure TestAddDataListener; override;
    procedure TestConnectTo;
    procedure TestDisconnect; override;
    procedure TestForceDisconnect; override;
    procedure TestListenOn;
    procedure TestRemoveDataListener; override;
    procedure TestRemotePartyAccepts; override;
    procedure TestSendData; override;
  end;

  TestTIdSdpMockTcpClientConnection = class(TestTIdSdpMockTcpConnection)
  protected
    procedure Activate(Connection: TIdSdpMockTcpConnection); override;
    function  CreateConnection: TIdSdpMockTcpConnection; override;
  published
    procedure TestConnectTo;
    procedure TestDisconnect; override;
    procedure TestForceDisconnect; override;
  end;

  TestTIdSdpMockTcpServerConnection = class(TestTIdSdpMockTcpConnection)
  protected
    procedure Activate(Connection: TIdSdpMockTcpConnection); override;
    function  CreateConnection: TIdSdpMockTcpConnection; override;
  published
    procedure TestDisconnect; override;
    procedure TestForceDisconnect; override;
    procedure TestListenOn;
    procedure TestRemotePartyAccepts; override;
  end;

  TestTIdSdpTcpClient = class(TThreadingTestCase)
  private
    Client:       TIdSdpTcpClient;
    FireOnceFlag: Boolean;
    Lock:         TCriticalSection;
    RecvBinding:  TIdConnectionBindings;
    Server:       TIdTcpServer;

    procedure ReceiveMessage(Sender: TObject; Msg: String; ReceivedOn: TIdConnectionBindings);
    procedure SendData(Thread: TIdPeerThread);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReceiveMessageRecordsBinding;
  end;

  TestTIdSdpTcpClientConnection = class(TThreadingTestCase,
                                        IIdSdpTcpConnectionListener)
  private
    CollectedReceivedData: String;
    Connected:             Boolean;
    Connection:            TIdSdpTcpClientConnection;
    ConnectionAddress:     String;
    ConnectionPort:        Cardinal;
    DisconnectEvent:       TEvent;
    ExceptionMessage:      String;
    ExceptionType:         ExceptClass;
    FirstExecute:          Boolean;
    Listener:              TIdSdpTestConnectionListener;
    ReceivedData:          String;
    Server:                TIdTcpServer;
    ServerEvent:           TEvent;
    TestData:              TStringStream;
    Timer:                 TIdTimerQueue;

    procedure DisconnectClient;
    procedure DoNothing(Thread: TIdPeerThread);
    procedure OnConnect(Connection: TIdSdpBaseTcpConnection);
    procedure OnData(Connection: TIdSdpBaseTcpConnection; Data: TStream); 
    procedure OnDisconnect(Connection: TIdSdpBaseTcpConnection);
    procedure OnException(Connection: TIdSdpBaseTcpConnection;
                          ExceptionType: ExceptClass;
                          ExceptionMessage: String);
    procedure ReadTestData(Thread: TIdPeerThread);
    procedure RegisterConnectionAttempt(Thread: TIdPeerThread);
    function  ServerIP: String;
    function  ServerPort: Integer;
    procedure WriteTestData; overload;
    procedure WriteTestData(Chunk: TStream); overload;
    procedure WriteTestData(Thread: TIdPeerThread); overload;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddressAndPortWithNoConnection;
    procedure TestConnectTo;
    procedure TestConnectToNonexistentPeer;
    procedure TestConnectToTwice;
    procedure TestDisconnect;
    procedure TestDisconnectTwice;
    procedure TestLocalDisconnectNotifiesListeners;
    procedure TestReceiveData;
    procedure TestReceiveDataMultipleTimes;
    procedure TestRemoteDisconnectionNotifiesListeners;
    procedure TestSendData;
  end;

  TestTIdSdpTcpServerConnection = class(TThreadingTestCase,
                                        IIdSdpTcpConnectionListener)
  private
    Client:            TIdTcpClient;
    Connected:         Boolean;
    Connection:        TIdSdpTcpServerConnection;
    LocalIP:           String;
    LocalPort:         Integer;
    ReceivedData:      String;
    TestData:          TStringStream;
    Timer:             TIdTimerQueue;

    procedure CheckPortFree(Address: String; Port: Integer);
    procedure CheckPortUsed(Address: String; Port: Integer);
    procedure OnConnect(Connection: TIdSdpBaseTcpConnection);
    procedure OnData(Connection: TIdSdpBaseTcpConnection; Data: TStream);
    procedure OnDisconnect(Connection: TIdSdpBaseTcpConnection);
    procedure OnException(Connection: TIdSdpBaseTcpConnection;
                          ExceptionType: ExceptClass;
                          ExceptionMessage: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddressAndPort;
    procedure TestAddressAndPortWithNoConnection;
    procedure TestAddressAndPortWhenNotListening;
    procedure TestConnectionNotifiesListeners;
    procedure TestDisconnect;
    procedure TestLocalDisconnectionNotifiesListeners;
    procedure TestListenOn;
    procedure TestPeerAddressAndPort;
    procedure TestPeerAddressAndPortNoConnection;
    procedure TestReceiveData;
    procedure TestRemoteDisconnectionNotifiesListeners;
    procedure TestSendData;
  end;

  TestTIdSdpTcpNullConnection = class(TTestCase)
  private
    Connection: TIdSdpTcpNullConnection;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddressAndPort;
    procedure TestPeerAddressAndPort;
  end;

  TestTIdSdpTcpMediaStream = class(TestTIdSdpBaseMediaStream)
  private
    ReceivingBinding: TIdConnectionBindings;
    Text:             TStringStream;

    function  ActiveMediaDesc(Port: Cardinal = 8000;
                              PortCount: Cardinal = 1): String;
    function  ActPassMediaDesc(Port: Cardinal = 8000): String;
    procedure CheckAtLeastOneNullConnectionCreated;
    procedure CheckClientConnectionFormed(WhenOffer: Boolean; Offer, Answer: String);
    procedure CheckClientConnectionFormedEx(WhenOffer, SetLocalDescFirst: Boolean; Offer, Answer: String);
    procedure CheckNullConnectionFormed(WhenOffer: Boolean; Offer, Answer: String);
    procedure CheckNullConnectionFormedEx(WhenOffer, SetLocalDescFirst: Boolean; Offer, Answer: String);
    procedure CheckNullConnectionFormedForInvalidResponse(WhenOffer: Boolean; Offer, Answer: String);
    procedure CheckNullConnectionFormedForInvalidResponseEx(WhenOffer, SetLocalDescFirst: Boolean; Offer, Answer: String);
    procedure CheckServerConnectionFormed(WhenOffer: Boolean; Offer, Answer: String);
    procedure CheckServerConnectionFormedEx(WhenOffer, SetLocalDescFirst: Boolean; Offer, Answer: String);
    function  FindMockClient(Desc: TIdSdpMediaDescription): TIdSdpMockTcpConnection;
    function  FindMockServer(Desc: TIdSdpMediaDescription): TIdSdpMockTcpConnection;
    function  FindSoleMockClient: TIdSdpMockTcpConnection;
    function  FindSoleMockServer: TIdSdpMockTcpConnection;
    function  HoldConnMediaDesc(Port: Cardinal = 8000): String;
    function  PassiveMediaDesc(Port: Cardinal = 8000;
                               PortCount: Cardinal = 1): String;
    procedure SetMediaDescriptions(S: TIdSdpBaseMediaStream; IsOffer, SetLocalDescFirst: Boolean; Offer, Answer: String);
    function  SetupMediaDesc(SetupType: TIdSdpSetupType;
                             Port: Cardinal = 8000;
                             PortCount: Cardinal = 1): String;
    function  UnknownMediaDesc(Port: Cardinal = 8000): String;
  protected
    procedure Activate(Stream: TIdSdpBaseMediaStream); override;
    function  BasicMediaDesc(Port: Cardinal = 8000; PortCount: Cardinal = 1): String; override;
    function  CreateStream: TIdSdpBaseMediaStream; override;
    procedure ReceiveDataOn(S: TIdSdpBaseMediaStream); override;
    procedure SendData(Stream: TIdSdpBaseMediaStream); override;
  public
    procedure Setup; override;
    procedure TearDown; override;

    procedure CheckPortActive(Address: String;
                              Port: Cardinal;
                              Msg: String); override;
    procedure CheckPortFree(Address: String;
                              Port: Cardinal;
                              Msg: String); override;
  published
    procedure TestPortsAndPortCount;
    procedure TestReceivedDataFormat;
    procedure TestReceivedDataFormatMultipleFormats;
    procedure TestSetRemoteDescriptionDoesntTerminateExistingConnections;
    procedure TestSetTimer; override;
    procedure TestStartListeningPortAboveAllowedRange; override;
    procedure TestStartListeningPortBelowAllowedRange; override;
    procedure TestStopListening;
    procedure TestUnusedPortsSwitchOff; override;
    procedure TestWhenAnswerReceiveActiveSendActive;
    procedure TestWhenAnswerReceiveActiveSendActPass;
    procedure TestWhenAnswerReceiveActiveSendBlank;
    procedure TestWhenAnswerReceiveActiveSendHoldConn;
    procedure TestWhenAnswerReceiveActiveSendPassive;
    procedure TestWhenAnswerReceiveActiveSendUnknown;
    procedure TestWhenAnswerReceiveActPassSendActive;
    procedure TestWhenAnswerReceiveActPassSendActPass;
    procedure TestWhenAnswerReceiveActPassSendBlank;
    procedure TestWhenAnswerReceiveActPassSendHoldConn;
    procedure TestWhenAnswerReceiveActPassSendPassive;
    procedure TestWhenAnswerReceiveActPassSendUnknown;
    procedure TestWhenAnswerReceiveBlankSendActive;
    procedure TestWhenAnswerReceiveBlankSendActPass;
    procedure TestWhenAnswerReceiveBlankSendBlank;
    procedure TestWhenAnswerReceiveBlankSendHoldConn;
    procedure TestWhenAnswerReceiveBlankSendPassive;
    procedure TestWhenAnswerReceiveBlankSendUnknown;
    procedure TestWhenAnswerReceiveHoldConnSendActive;
    procedure TestWhenAnswerReceiveHoldConnSendActPass;
    procedure TestWhenAnswerReceiveHoldConnSendBlank;
    procedure TestWhenAnswerReceiveHoldConnSendHoldConn;
    procedure TestWhenAnswerReceiveHoldConnSendPassive;
    procedure TestWhenAnswerReceiveHoldConnSendUnknown;
    procedure TestWhenAnswerReceivePassiveSendActive;
    procedure TestWhenAnswerReceivePassiveSendActPass;
    procedure TestWhenAnswerReceivePassiveSendBlank;
    procedure TestWhenAnswerReceivePassiveSendHoldConn;
    procedure TestWhenAnswerReceivePassiveSendPassive;
    procedure TestWhenAnswerReceivePassiveSendUnknown;
    procedure TestWhenAnswerReceiveUnknownSendActive;
    procedure TestWhenAnswerReceiveUnknownSendActPass;
    procedure TestWhenAnswerReceiveUnknownSendBlank;
    procedure TestWhenAnswerReceiveUnknownSendHoldConn;
    procedure TestWhenAnswerReceiveUnknownSendPassive;
    procedure TestWhenAnswerReceiveUnknownSendUnknown;
    procedure TestWhenOfferActPassStartsListening;
    procedure TestWhenOfferActPassStopsListeningWhenAnswerHoldConn;
    procedure TestWhenOfferActPassStopsListeningWhenAnswerPassive;
    procedure TestWhenOfferPassiveStartsListening;
    procedure TestWhenOfferPassiveStopsListeningWhenAnswerHoldConn;
    procedure TestWhenOfferSendActiveReceiveActive;
    procedure TestWhenOfferSendActiveReceiveActPass;
    procedure TestWhenOfferSendActiveReceiveBlank;
    procedure TestWhenOfferSendActiveReceivePassive;
    procedure TestWhenOfferSendActiveReceiveHoldConn;
    procedure TestWhenOfferSendActiveReceiveUnknown;
    procedure TestWhenOfferSendActPassReceiveActive;
    procedure TestWhenOfferSendActPassReceiveActPass;
    procedure TestWhenOfferSendActPassReceiveBlank;
    procedure TestWhenOfferSendActPassReceiveHoldConn;
    procedure TestWhenOfferSendActPassReceivePassive;
    procedure TestWhenOfferSendActPassReceiveUnknown;
    procedure TestWhenOfferSendBlankReceiveActive;
    procedure TestWhenOfferSendBlankReceiveActPass;
    procedure TestWhenOfferSendBlankReceiveBlank;
    procedure TestWhenOfferSendBlankReceivePassive;
    procedure TestWhenOfferSendBlankReceiveHoldConn;
    procedure TestWhenOfferSendBlankReceiveUnknown;
    procedure TestWhenOfferSendHoldConnReceiveActive;
    procedure TestWhenOfferSendHoldConnReceiveActPass;
    procedure TestWhenOfferSendHoldConnReceiveBlank;
    procedure TestWhenOfferSendHoldConnReceiveHoldConn;
    procedure TestWhenOfferSendHoldConnReceivePassive;
    procedure TestWhenOfferSendHoldConnReceiveUnknown;
    procedure TestWhenOfferSendPassiveReceiveActive;
    procedure TestWhenOfferSendPassiveReceiveActPass;
    procedure TestWhenOfferSendPassiveReceiveBlank;
    procedure TestWhenOfferSendPassiveReceiveHoldConn;
    procedure TestWhenOfferSendPassiveReceivePassive;
    procedure TestWhenOfferSendPassiveReceiveUnknown;
    procedure TestWhenOfferSendUnknownReceiveActive;
    procedure TestWhenOfferSendUnknownReceiveActPass;
    procedure TestWhenOfferSendUnknownReceiveBlank;
    procedure TestWhenOfferSendUnknownReceiveHoldConn;
    procedure TestWhenOfferSendUnknownReceivePassive;
    procedure TestWhenOfferSendUnknownReceiveUnknown;
  end;

  TestTIdSDPMultimediaSession = class(TIdSdpTestCase)
  private
    LocalPort:   Cardinal;
    MS:          TIdSDPMultimediaSession;
    PortBlocker: TIdMockRTPPeer;
    Profile:     TIdRTPProfile;
    RemotePort:  Cardinal;
    Server:      TIdUDPServer;

    procedure CheckOrigin(ExpectedNetType: String;
                          ExpectedAddressType: TIdIPVersion;
                          ExpectedAddress: String;
                          ExpectedSessionDescription: String);
    procedure CheckSessionName(ExpectedName: String;
                               SessionDescription: String;
                               Msg: String);
    procedure CheckOriginUsername(ExpectedUsername: String;
                                  SessionDescription: String);
    function  MediaDescription(Address: String; Port: Cardinal; Protocol: String = Id_SDP_RTPAVP): String;
    function  MultiStreamSDP(LowPort, HighPort: Cardinal; Protocol: String = Id_SDP_RTPAVP): String;
    function  MultiStreamSDPSameIP(LowPort, HighPort: Cardinal; Protocol: String = Id_SDP_RTPAVP): String;
    procedure ReceiveDataOfType(PayloadType: Cardinal);
    function  RefusedStreamSDP(Port: Cardinal): String;
    function  SingleStreamSDP(Port: Cardinal;
                              PayloadType: Cardinal = 96): String;
    procedure StartServerOnPort(Port: Integer);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddressTypeFor;
    procedure TestDifferentPayloadTypesForSameEncoding;
    procedure TestIndexOfStream;
    procedure TestIsListening;
    procedure TestLocalSessionDescription;
    procedure TestLocalSessionDescriptionWithRefusedStream;
    procedure TestLocalSessionDescriptionWithTimeHeader;
    procedure TestLocalSessionDescriptionWithWeirdTimeHeader;
    procedure TestLocalSessionVersionIncrements;
    procedure TestMimeType;
    procedure TestNetTypeFor;
    procedure TestOnHold;
    procedure TestPortAboveHighestAllowedPort;
    procedure TestPortAsHighestAllowedPort;
    procedure TestPortAsLowestAllowedPort;
    procedure TestPortBelowLowestAllowedPort;
    procedure TestPortThreePortRange;
    procedure TestPutOnHold;
    procedure TestSetIsOffer;
    procedure TestSetIsOfferSetsStreamsIsOffer;
    procedure TestSetLocalMachineName;
    procedure TestSetLocalSessionName;
    procedure TestSetRemoteDescription;
    procedure TestSetRemoteDescriptionMalformedSdp;
    procedure TestSetRemoteDescriptionWithSdpPayload;
    procedure TestSetUsername;
    procedure TestStartListeningSingleStream;
    procedure TestStartListeningMalformedSdp;
    procedure TestStartListeningMultipleStreams;
    procedure TestStartListeningNoAvailablePorts;
    procedure TestStartListeningPortsOutsideAllowedRange;
    procedure TestStartListeningTCP;
    procedure TestStopListening;
    procedure TestStreamNeedsMorePortsThanAvailable;
    procedure TestTakeOffHold;
  end;

  TIdSdpTcpConnectionWaitTestCase = class(TTestCase)
  protected
    Connection: TIdSdpMockTcpConnection;
    Data:       TStringStream;
    Wait:       TIdSdpTcpConnectionWait;

    procedure CheckTriggerDoesNothing(Msg: String); virtual;
    procedure CheckTriggerFired; virtual;
    function  CreateWait: TIdSdpTcpConnectionWait; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTrigger;
    procedure TestTriggerWithNonexistentConnection;
    procedure TestTriggerWithWrongTypeOfObject;
  end;

  TestTIdSdpTcpReceiveDataWait = class(TIdSdpTcpConnectionWaitTestCase)
  private
    Binding:  TIdConnectionBindings;
    RecvWait: TIdSdpTcpReceiveDataWait;
  protected
    procedure CheckTriggerDoesNothing(Msg: String); override;
    procedure CheckTriggerFired; override;
    function  CreateWait: TIdSdpTcpConnectionWait; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSdpTcpSendDataWait = class(TIdSdpTcpConnectionWaitTestCase)
  private
    SendWait: TIdSdpTcpSendDataWait;
  protected
    procedure CheckTriggerDoesNothing(Msg: String); override;
    procedure CheckTriggerFired; override;
    function  CreateWait: TIdSdpTcpConnectionWait; override;
  public
    procedure SetUp; override;
  end;

  TestTIdSdpTcpConnectionConnectedWait = class(TIdSdpTcpConnectionWaitTestCase)
  private
    Listener: TIdSdpTestConnectionListener;
  protected
    procedure CheckTriggerDoesNothing(Msg: String); override;
    procedure CheckTriggerFired; override;
    function  CreateWait: TIdSdpTcpConnectionWait; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSdpTcpConnectionDisconnectedWait = class(TIdSdpTcpConnectionWaitTestCase)
  private
    Listener: TIdSdpTestConnectionListener;
  protected
    procedure CheckTriggerDoesNothing(Msg: String); override;
    procedure CheckTriggerFired; override;
    function  CreateWait: TIdSdpTcpConnectionWait; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSdpTcpConnectionExceptionWait = class(TIdSdpTcpConnectionWaitTestCase)
  private
    ExceptionType:    ExceptClass;
    ExceptionMessage: String;
    Listener:         TIdSdpTestConnectionListener;
  protected
    procedure CheckTriggerDoesNothing(Msg: String); override;
    procedure CheckTriggerFired; override;
    function  CreateWait: TIdSdpTcpConnectionWait; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TIdSdpMediaListenerMethodTestCase = class(TTestCase)
  protected
    Chunk:    TStream;
    Format:   String;
    Listener: TIdSdpTestMediaListener;
    Stream:   TIdSdpMediaStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSdpMediaListenerOnDataMethod = class(TIdSdpMediaListenerMethodTestCase)
  private
    Binding:  TIdConnectionBindings;
    Method:   TIdSdpMediaListenerOnDataMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSdpMediaListenerOnSentDataMethod = class(TIdSdpMediaListenerMethodTestCase)
  private
    Method: TIdSdpMediaListenerOnSentDataMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TIdSdpTcpConnectionMethodTestCase = class(TTestCase)
  protected
    Connection: TIdSdpMockTcpConnection;
    Listener:   TIdSdpTestConnectionListener;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTIdSdpTcpConnectionOnConnectMethod = class(TIdSdpTcpConnectionMethodTestCase)
  private
    Method: TIdSdpTcpConnectionOnConnectMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSdpTcpConnectionOnDataMethod = class(TIdSdpTcpConnectionMethodTestCase)
  private
    Data:   TStringStream;
    Method: TIdSdpTcpConnectionOnDataMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSdpTcpConnectionOnDisconnectMethod = class(TIdSdpTcpConnectionMethodTestCase)
  private
    Method: TIdSdpTcpConnectionOnDisconnectMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

  TestTIdSdpTcpConnectionOnExceptionMethod = class(TIdSdpTcpConnectionMethodTestCase)
  private
    Method: TIdSdpTcpConnectionOnExceptionMethod;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRun;
  end;

const
  MinimumPayloadSansConnection = 'v=0'#13#10
                 + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                 + 's=Minimum Session Info'#13#10;
  MinimumPayloadWithConnection = MinimumPayloadSansConnection
                               + 'c=IN IP4 127.0.0.1'#13#10;
  DefaultConnection = 'c=IN IP4 224.2.17.12/127'#13#10;
  MinimumPayload = MinimumPayloadSansConnection
                 + DefaultConnection;

implementation

uses
  IdException, IdRegisteredObject, IdSocketHandle, IdStack, IdUnicode,
  IdTCPConnection;

const
  OneSecond = 1000; // milliseconds

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSdp unit tests');
  Result.AddTest(TestFunctions.Suite);
  Result.AddTest(TestTIdSdpAttribute.Suite);
  Result.AddTest(TestTIdSdpRTPMapAttribute.Suite);
  Result.AddTest(TestTIdSdpBandwidth.Suite);
  Result.AddTest(TestTIdSdpConnection.Suite);
  Result.AddTest(TestTIdSdpKey.Suite);
  Result.AddTest(TestTIdSdpMediaDescription.Suite);
  Result.AddTest(TestTIdSdpOrigin.Suite);
  Result.AddTest(TestTIdSdpRepeat.Suite);
  Result.AddTest(TestTIdSdpTime.Suite);
  Result.AddTest(TestTIdSdpAttributes.Suite);
  Result.AddTest(TestTIdSdpRTPMapAttributes.Suite);
  Result.AddTest(TestTIdSdpBandwidths.Suite);
  Result.AddTest(TestTIdSdpConnections.Suite);
  Result.AddTest(TestTIdSdpMediaDescriptions.Suite);
  Result.AddTest(TestTIdSdpRepeats.Suite);
  Result.AddTest(TestTIdSdpTimes.Suite);
  Result.AddTest(TestTIdSdpZoneAdjustments.Suite);
  Result.AddTest(TestTIdSdpParser.Suite);
  Result.AddTest(TestTIdSdpPayload.Suite);
  Result.AddTest(TestTIdSDPMediaStream.Suite);
  Result.AddTest(TestTIdSdpNullMediaStream.Suite);
  Result.AddTest(TestTIdSdpTcpConnectionRegistry.Suite);
  Result.AddTest(TestTIdSdpTcpClient.Suite);
  Result.AddTest(TestTIdSdpTcpClientConnection.Suite);
  Result.AddTest(TestTIdSdpTcpServerConnection.Suite);
  Result.AddTest(TestTIdSdpTcpNullConnection.Suite);
  Result.AddTest(TestTIdSdpMockTcpNullConnection.Suite);
  Result.AddTest(TestTIdSdpMockTcpClientConnection.Suite);
  Result.AddTest(TestTIdSdpMockTcpServerConnection.Suite);
  Result.AddTest(TestTIdSdpTcpMediaStream.Suite);
  Result.AddTest(TestTIdSDPMultimediaSession.Suite);
  Result.AddTest(TestTIdSdpTcpConnectionConnectedWait.Suite);
  Result.AddTest(TestTIdSdpTcpConnectionDisconnectedWait.Suite);
  Result.AddTest(TestTIdSdpTcpConnectionExceptionWait.Suite);
  Result.AddTest(TestTIdSdpTcpReceiveDataWait.Suite);
  Result.AddTest(TestTIdSdpTcpSendDataWait.Suite);
  Result.AddTest(TestTIdSdpMediaListenerOnDataMethod.Suite);
  Result.AddTest(TestTIdSdpMediaListenerOnSentDataMethod.Suite);
  Result.AddTest(TestTIdSdpTcpConnectionOnConnectMethod.Suite);
  Result.AddTest(TestTIdSdpTcpConnectionOnDataMethod.Suite);
  Result.AddTest(TestTIdSdpTcpConnectionOnDisconnectMethod.Suite);
  Result.AddTest(TestTIdSdpTcpConnectionOnExceptionMethod.Suite);
end;

const
  ArbitraryPort = 8000;
  IPv4LocalHost = '127.0.0.1';
  IPv6LocalHost = '::1';

//******************************************************************************
//* TIdSdpTestMediaListener                                                    *
//******************************************************************************
//* TIdSdpTestMediaListener Public methods *************************************

constructor TIdSdpTestMediaListener.Create;
begin
  inherited Create;

  Self.fReceivedData := false;
end;

//* TIdSdpTestMediaListener Private methods ************************************

procedure TIdSdpTestMediaListener.OnData(Stream: TIdSdpBaseMediaStream;
                                         Chunk: TStream;
                                         Format: String;
                                         Binding: TIdConnectionBindings);
begin
  Self.fReceivedData := true;
  Self.fBindingParam := Binding;
  Self.fChunkParam   := Chunk;
  Self.fFormatParam  := Format;
  Self.fStreamParam  := Stream;
end;

procedure TIdSdpTestMediaListener.OnSentData(Stream: TIdSdpBaseMediaStream;
                                             Chunk: TStream;
                                             Format: String;
                                             LayerID: Integer);
begin
  Self.fSentData     := true;
  Self.fChunkParam   := Chunk;
  Self.fFormatParam  := Format;
  Self.fLayerIDParam := LayerID;
  Self.fStreamParam  := Stream;
end;

//******************************************************************************
//* TIdSdpTestConnectionListener                                               *
//******************************************************************************
//* TIdSdpTestConnectionListener Public methods ********************************

constructor TIdSdpTestConnectionListener.Create;
begin
  inherited Create;

  Self.fDataParam          := TMemoryStream.Create;
  Self.fOnConnectCalled    := false;
  Self.fOnDataCalled       := false;
  Self.fOnDisconnectCalled := false;
  Self.fOnExceptionCalled  := false;
end;

destructor TIdSdpTestConnectionListener.Destroy;
begin
  Self.fDataParam.Free;

  inherited Destroy;
end;

//* TIdSdpTestConnectionListener Private methods *******************************

procedure TIdSdpTestConnectionListener.OnConnect(Connection: TIdSdpBaseTcpConnection);
begin
  Self.fConnectionParam := Connection;
  Self.fOnConnectCalled := true;
end;

procedure TIdSdpTestConnectionListener.OnData(Connection: TIdSdpBaseTcpConnection;
                                              Data: TStream);
begin
  Self.fConnectionParam := Connection;
  Self.fDataParam.CopyFrom(Data, 0);
  Self.fOnDataCalled    := true;
end;

procedure TIdSdpTestConnectionListener.OnDisconnect(Connection: TIdSdpBaseTcpConnection);
begin
  Self.fConnectionParam    := Connection;
  Self.fOnDisconnectCalled := true;
end;

procedure TIdSdpTestConnectionListener.OnException(Connection: TIdSdpBaseTcpConnection;
                                                   ExceptionType: ExceptClass;
                                                   ExceptionMessage: String);
begin
  Self.fOnExceptionCalled := true;
  Self.fConnectionParam   := Connection;
  Self.fExceptionMessage  := ExceptionMessage;
  Self.fExceptionType     := ExceptionType;
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestAddressTypeToStr;
var
  A: TIdIPVersion;
begin
  CheckEquals(Id_SDP_IP4, AddressTypeToStr(Id_IPv4), 'Id_IPv4');
  CheckEquals(Id_SDP_IP6, AddressTypeToStr(Id_IPv6), 'Id_IPv6');

  // To check that ALL TIdIPVersions can be converted
  for A := Low(TIdIPVersion) to High(TIdIPVersion) do
    AddressTypeToStr(A);
end;

procedure TestFunctions.TestAppropriateSetupType;
begin
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stActive, false, false)),   'AppropriateSetupType(stActive, false, false)');
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stActive, false, true)),    'AppropriateSetupType(stActive, false, true)');
  CheckEquals(SetupTypeToStr(stPassive),  SetupTypeToStr(AppropriateSetupType(stActive, true, false)),    'AppropriateSetupType(stActive, true, false)');
  CheckEquals(SetupTypeToStr(stPassive),  SetupTypeToStr(AppropriateSetupType(stActive, true, true)),     'AppropriateSetupType(stActive, true, true)');

  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stActPass, false, false)),  'AppropriateSetupType(stActPass, false, false)');
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stActPass, false, true)),   'AppropriateSetupType(stActPass, false, true)');
  CheckEquals(SetupTypeToStr(stPassive),  SetupTypeToStr(AppropriateSetupType(stActPass, true, false)),   'AppropriateSetupType(stActPass, true, false)');
  CheckEquals(SetupTypeToStr(stActive),   SetupTypeToStr(AppropriateSetupType(stActPass, true, true)),    'AppropriateSetupType(stActPass, true, true)');

  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stPassive, false, false)),  'AppropriateSetupType(stPassive, false, false)');
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stPassive, false, true)),   'AppropriateSetupType(stPassive, false, true)');
  CheckEquals(SetupTypeToStr(stActive),   SetupTypeToStr(AppropriateSetupType(stPassive, true, false)),   'AppropriateSetupType(stPassive, true, false)');
  CheckEquals(SetupTypeToStr(stActive),   SetupTypeToStr(AppropriateSetupType(stPassive, true, true)),    'AppropriateSetupType(stPassive, true, true)');

  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stHoldConn, false, false)), 'AppropriateSetupType(stHoldConn, false, false)');
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stHoldConn, false, true)),  'AppropriateSetupType(stHoldConn, false, true)');
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stHoldConn, true, false)),  'AppropriateSetupType(stHoldConn, true, false)');
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stHoldConn, true, true)),   'AppropriateSetupType(stHoldConn, true, true)');

  // And sanity check against malformed values:
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stUnknown, false, false)),  'AppropriateSetupType(stUnknown, false, false)');
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stUnknown, false, true)),   'AppropriateSetupType(stUnknown, false, true)');
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stUnknown, true, false)),   'AppropriateSetupType(stUnknown, true, false)');
  CheckEquals(SetupTypeToStr(stHoldConn), SetupTypeToStr(AppropriateSetupType(stUnknown, true, true)),    'AppropriateSetupType(stUnknown, true, true)');
end;

procedure TestFunctions.TestBandwidthTypeToStr;
var
  B: TIdSdpBandwidthType;
begin
  CheckEquals(Id_SDP_ConferenceTotal,     BandwidthTypeToStr(btConferenceTotal),     'btConferenceTotal');
  CheckEquals(Id_SDP_ApplicationSpecific, BandwidthTypeToStr(btApplicationSpecific), 'btApplicationSpecific');
  CheckEquals(Id_SDP_RS,                  BandwidthTypeToStr(btRS),                  'btRS');
  CheckEquals(Id_SDP_RR,                  BandwidthTypeToStr(btRR),                  'btRR');

  // To check that ALL TIdSdpBandwidthTypes can be converted
  for B := Low(TIdSdpBandwidthType) to High(TIdSdpBandwidthType) do
    BandwidthTypeToStr(B);
end;

procedure TestFunctions.TestConnectionTypeToStr;
var
  C: TIdSdpConnectionType;
begin
  CheckEquals(RSSDPConnectionExisting, ConnectionTypeToStr(ctExisting), 'ctExisting');
  CheckEquals(RSSDPConnectionNew,      ConnectionTypeToStr(ctNew),      'ctNew');

  // To check that ALL TIdSdpConnectionTypes can be converted
  for C := Low(TIdSdpConnectionType) to High(TIdSdpConnectionType) do
    ConnectionTypeToStr(C);
end;

procedure TestFunctions.TestDirectionToStr;
var
  D: TIdSdpDirection;
begin
  CheckEquals('inactive', DirectionToStr(sdInactive), 'sdInactive');
  CheckEquals('recvonly', DirectionToStr(sdRecvOnly), 'sdRecvOnly');
  CheckEquals('sendonly', DirectionToStr(sdSendOnly), 'sdSendOnly');
  CheckEquals('sendrecv', DirectionToStr(sdSendRecv), 'sdSendRecv');

  // To check that ALL TIdSdpDirections can be converted
  for D := Low(TIdSdpDirection) to High(TIdSdpDirection) do
    DirectionToStr(D);
end;

procedure TestFunctions.TestKeyTypeToStr;
var
  K: TIdSdpKeyType;
begin
  CheckEquals(Id_SDP_Clear,  KeyTypeToStr(ktClear),  'ktClear');
  CheckEquals(Id_SDP_Base64, KeyTypeToStr(ktBase64), 'ktBase64');
  CheckEquals(Id_SDP_URI,    KeyTypeToStr(ktURI),    'ktURI');
  CheckEquals(Id_SDP_Prompt, KeyTypeToStr(ktPrompt), 'ktPrompt');

  // To check that ALL TIdSdpKeyTypes can be converted
  for K := Low(TIdSdpKeyType) to High(TIdSdpKeyType) do
    KeyTypeToStr(K);
end;

procedure TestFunctions.TestMediaTypeToStr;
var
  M: TIdSdpMediaType;
begin
  CheckEquals(RSSDPMediaTypeAudio,       MediaTypeToStr(mtAudio),       'mtAudio');
  CheckEquals(RSSDPMediaTypeVideo,       MediaTypeToStr(mtVideo),       'mtVideo');
  CheckEquals(RSSDPMediaTypeApplication, MediaTypeToStr(mtApplication), 'mtApplication');
  CheckEquals(RSSDPMediaTypeData,        MediaTypeToStr(mtData),        'mtData');
  CheckEquals(RSSDPMediaTypeControl,     MediaTypeToStr(mtControl),     'mtControl');
  CheckEquals(RSSDPMediaTypeText,        MediaTypeToStr(mtText),        'mtText');

  // To check that ALL TIdSdpMediaTypes can be converted
  for M := Low(TIdSdpMediaType) to High(TIdSdpMediaType) do
    MediaTypeToStr(M);
end;

procedure TestFunctions.TestSetupTypeToStr;
var
  ST: TIdSdpSetupType;
begin
  CheckEquals(RSSDPSetupActive,   SetupTypeToStr(stActive),   'stActive');
  CheckEquals(RSSDPSetupActPass,  SetupTypeToStr(stActPass),  'stActPass');
  CheckEquals(RSSDPSetupHoldConn, SetupTypeToStr(stHoldConn), 'stHoldConn');
  CheckEquals(RSSDPSetupPassive,  SetupTypeToStr(stPassive),  'stPassive');

  // To check that ALL TIdSdpSetupTypes can be converted
  for ST := Low(TIdSdpSetupType) to High(TIdSdpSetupType) do
    SetupTypeToStr(ST);
end;

procedure TestFunctions.TestStrToAddressType;
var
  AT: TIdIPVersion;
begin
  for AT := Low(TIdIPVersion) to High(TIdIPVersion) do
    if (AT <> Id_IPUnknown) then
      Check(AT = StrToAddressType(AddressTypeToStr(AT)),
            'Ord(AT) = ' + IntToStr(Ord(AT)));

  Check(Id_IPv4      = StrToAddressType(Id_SDP_IP4),     Id_SDP_IP4);
  Check(Id_IPv6      = StrToAddressType(Id_SDP_IP6),     Id_SDP_IP6);
  Check(Id_IPUnknown = StrToAddressType('unknownValue'), 'unknownValue');

  try
    StrToAddressType('');
    Fail('Failed to bail out: ''''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '''' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: ''''');
  end;

  try
    StrToAddressType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdIPVersion',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;
end;

procedure TestFunctions.TestStrToBandwidthType;
var
  BT: TIdSdpBandwidthType;
begin
  for BT := Low(TIdSdpBandwidthType) to High(TIdSdpBandwidthType) do
    if (BT <> btUnknown) then
      Check(BT = StrToBandwidthType(BandwidthTypeToStr(BT)),
            'Ord(BT) = ' + IntToStr(Ord(BT)));

  Check(btConferenceTotal     = StrToBandwidthType(Id_SDP_ConferenceTotal),     Id_SDP_ConferenceTotal);
  Check(btApplicationSpecific = StrToBandwidthType(Id_SDP_ApplicationSpecific), Id_SDP_ApplicationSpecific);
  Check(btRS                  = StrToBandwidthType(Id_SDP_RS),                  Id_SDP_RS);
  Check(btRR                  = StrToBandwidthType(Id_SDP_RR),                  Id_SDP_RR);
  Check(btUnknown             = StrToBandwidthType('unknownValue'),             'unknownValue');


  Check(btUnknown = StrToBandwidthType('halloo'), 'Converting ''halloo''');

  try
    StrToBandwidthType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdSdpBandwidthType',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;
end;

procedure TestFunctions.TestStrToConnectionType;
var
  CT: TIdSdpConnectionType;
begin
  for CT := Low(TIdSdpConnectionType) to High(TIdSdpConnectionType) do
    if (CT <> CTUnknown) then
      Check(CT = StrToConnectionType(ConnectionTypeToStr(CT)),
            'Ord(CT) = ' + IntToStr(Ord(CT)));

  Check(ctExisting = StrToConnectionType(RSSDPConnectionExisting),     RSSDPConnectionExisting);
  Check(ctNew      = StrToConnectionType(RSSDPConnectionNew),          RSSDPConnectionNew);
  Check(ctUnknown  = StrToConnectionType('unknownValue'), 'unknownValue');

  try
    StrToConnectionType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdSdpConnectionType',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;
end;

procedure TestFunctions.TestStrToDirection;
begin
  Check(sdInactive = StrToDirection(RSSDPDirectionInactive), RSSDPDirectionInactive);
  Check(sdRecvOnly = StrToDirection(RSSDPDirectionRecvOnly), RSSDPDirectionRecvOnly);
  Check(sdSendOnly = StrToDirection(RSSDPDirectionSendOnly), RSSDPDirectionSendOnly);
  Check(sdSendRecv = StrToDirection(RSSDPDirectionSendRecv), RSSDPDirectionSendRecv);
  Check(sdUnknown  = StrToDirection('unknownValue'),         'unknownValue');

  try
    StrToDirection('');
    Fail('Failed to bail out on empty string');
  except
    on EConvertError do;
  end;
end;

procedure TestFunctions.TestStrToKeyType;
var
  KT: TIdSdpKeyType;
begin
  for KT := Low(TIdSdpKeyType) to High(TIdSdpKeyType) do
    if (KT <> ktUnknown) then
      Check(KT = StrToKeyType(KeyTypeToStr(KT)),
            'Ord(KT) = ' + IntToStr(Ord(KT)));

  Check(ktClear   = StrToKeyType(Id_SDP_Clear),   Id_SDP_Clear);
  Check(ktBase64  = StrToKeyType(Id_SDP_Base64),  Id_SDP_Base64);
  Check(ktURI     = StrToKeyType(Id_SDP_URI),     Id_SDP_URI);
  Check(ktPrompt  = StrToKeyType(Id_SDP_Prompt),  Id_SDP_Prompt);
  Check(ktUnknown = StrToKeyType('unknownValue'), 'unknownValue');

  try
    StrToKeyType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdSdpKeyType',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;
end;

procedure TestFunctions.TestStrToMediaType;
var
  MT: TIdSdpMediaType;
begin
  for MT := Low(TIdSdpMediaType) to High(TIdSdpMediaType) do
    if (MT <> mtUnknown) then
      Check(MT = StrToMediaType(MediaTypeToStr(MT)),
            'Ord(MT) = ' + IntToStr(Ord(MT)));

  Check(mtAudio       = StrToMediaType(RSSDPMediaTypeAudio),       RSSDPMediaTypeAudio);
  Check(mtVideo       = StrToMediaType(RSSDPMediaTypeVideo),       RSSDPMediaTypeVideo);
  Check(mtApplication = StrToMediaType(RSSDPMediaTypeApplication), RSSDPMediaTypeApplication);
  Check(mtData        = StrToMediaType(RSSDPMediaTypeData),        RSSDPMediaTypeData);
  Check(mtControl     = StrToMediaType(RSSDPMediaTypeControl),     RSSDPMediaTypeControl);
  Check(mtText        = StrToMediaType(RSSDPMediaTypeText),        RSSDPMediaTypeText);
  Check(mtUnknown     = StrToMediaType('unknownValue'),            'unknownValue');

  try
    StrToMediaType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdSdpMediaType',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;
end;

procedure TestFunctions.TestStrToSetupType;
var
  ST: TIdSdpSetupType;
begin
  for ST := Low(TIdSdpSetupType) to High(TIdSdpSetupType) do
    if (ST <> stUnknown) then
      Check(ST = StrToSetupType(SetupTypeToStr(ST)),
            'Ord(ST) = ' + IntToStr(Ord(ST)));

  Check(stActive   = StrToSetupType(RSSDPSetupActive),   RSSDPSetupActive);
  Check(stActPass  = StrToSetupType(RSSDPSetupActPass),  RSSDPSetupActPass);
  Check(stHoldConn = StrToSetupType(RSSDPSetupHoldConn), RSSDPSetupHoldConn);
  Check(stPassive  = StrToSetupType(RSSDPSetupPassive),  RSSDPSetupPassive);
  Check(stUnknown  = StrToSetupType('unknownValue'),     'unknownValue');

  try
    StrToSetupType(' ');
    Fail('Failed to bail out: '' ''');
  except
    on E: EConvertError do
      CheckEquals('Couldn''t convert '' '' to type TIdSdpSetupType',
                  E.Message,
                  'Unexpected exception: '' ''');
  end;               
end;

//******************************************************************************
//* TestTIdSdpAttribute                                                        *
//******************************************************************************
//* TestTIdSdpAttribute Public methods *****************************************

procedure TestTIdSdpAttribute.SetUp;
begin
  inherited SetUp;

  Self.A := TIdSdpAttribute.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpAttribute.TearDown;
begin
  Self.S.Free;
  Self.A.Free;

  inherited TearDown;
end;

//* TestTIdSdpAttribute Published methods **************************************

procedure TestTIdSdpAttribute.TestAssign;
var
  Other: TIdSdpAttribute;
begin
  Other := TIdSdpAttribute.Create;
  try
    Other.Name := 'foo';
    Other.Value := 'bar';

    Self.A.Assign(Other);
    CheckEquals(Other.Name,  Self.A.Name,  'Name');
    CheckEquals(Other.Value, Self.A.Value, 'Value');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpAttribute.TestCopy;
var
  Clone: TIdSdpAttribute;
begin
  Self.A.Name  := 'abcd';
  Self.A.Value := 'efgh';

  Clone := Self.A.Copy;
  try
    CheckEquals(Self.A.Name,  Clone.Name,  'Name');
    CheckEquals(Self.A.Value, Clone.Value, 'Value');
  finally
    Clone.Free;
  end;
end;

procedure TestTIdSdpAttribute.TestCreateAttribute;
var
  Att: TIdSdpAttribute;
begin
  Att := TIdSdpAttribute.CreateAttribute('rtpmap:98 t140/1000');
  try
    CheckEquals(TIdSdpRTPMapAttribute.ClassName,
                Att.ClassName,
                'Incorrect class for rtpmap attribute');
    CheckEquals('rtpmap',       Att.Name,  'rtpmap attribute name');
    CheckEquals('98 t140/1000', Att.Value, 'rtpmap attribute value');
  finally
    Att.Free;
  end;

  Att := TIdSdpAttribute.CreateAttribute('98 t140/1000');
  try
    CheckEquals(TIdSdpAttribute.ClassName,
                Att.ClassName,
                'Incorrect class for "unspecial" attribute');
    CheckEquals('98 t140/1000', Att.Name,  'attribute name');
    CheckEquals('',             Att.Value, 'attribute value');
  finally
    Att.Free;
  end;
end;

procedure TestTIdSdpAttribute.TestEquals;
var
  Att: TIdSdpAttribute;
begin
  Att := TIdSdpAttribute.CreateAttribute('foo:bar');
  try
    Check(not Self.A.Equals(Att), 'A <> Att; 1');
    Check(not Att.Equals(Self.A), 'Att <> A; 1');

    Self.A.Name := Att.Name;
    Check(not Self.A.Equals(Att), 'A <> Att; 2');
    Check(not Att.Equals(Self.A), 'Att <> A; 2');

    Self.A.Value := Att.Value;
    Check(Self.A.Equals(Att), 'A = Att');
    Check(Att.Equals(Self.A), 'Att = A');

    Self.A.Value := Att.Value + '1';
    Check(not Self.A.Equals(Att), 'A <> Att; 3');
    Check(not Att.Equals(Self.A), 'Att <> A; 3');
  finally
    Att.Free;
  end;
end;

procedure TestTIdSdpAttribute.TestIsRTPMap;
begin
  Check(not Self.A.IsRTPMap, 'Shouldn''t be an RTP map');
end;

procedure TestTIdSdpAttribute.TestPrintOnNoValue;
begin
  Self.A.Name := 'rtpmap';

  Self.A.PrintOn(Self.S);

  CheckEquals('a=rtpmap'#13#10, Self.S.DataString, 'PrintOn');
end;

procedure TestTIdSdpAttribute.TestPrintOnWithValue;
begin
  Self.A.Name  := 'rtpmap';
  Self.A.Value := '98 T140';

  Self.A.PrintOn(Self.S);

  CheckEquals('a=rtpmap:98 T140'#13#10, Self.S.DataString, 'PrintOn');
end;

//******************************************************************************
//* TestTIdSdpRTPMapAttribute                                                  *
//******************************************************************************
//* TestTIdSdpRTPMapAttribute Public methods ***********************************

procedure TestTIdSdpRTPMapAttribute.SetUp;
begin
  inherited SetUp;

  Self.A := TIdSdpRTPMapAttribute.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpRTPMapAttribute.TearDown;
begin
  Self.S.Free;
  Self.A.Free;

  inherited TearDown;
end;

//* TestTIdSdpRTPMapAttribute Published methods ********************************

procedure TestTIdSdpRTPMapAttribute.TestCopy;
var
  Clone: TIdSdpAttribute;
begin
  Self.A.Name  := 'rtpmap';
  Self.A.Value := '98 T140/8000';

  Clone := Self.A.Copy;
  try
    CheckEquals(Self.A.ClassName,
                Clone.ClassName,
                'Type');
    CheckEquals(Self.A.Name,  Clone.Name,  'Name');
    CheckEquals(Self.A.Value, Clone.Value, 'Value');
  finally
    Clone.Free;
  end;
end;

procedure TestTIdSdpRTPMapAttribute.TestGetName;
begin
  CheckEquals(RTPMapAttribute, Self.A.Name, 'New rtpmap attribute');

  Self.A.Name := 'foo';

  CheckEquals(RTPMapAttribute, Self.A.Name, 'After trying to change its name');
end;

procedure TestTIdSdpRTPMapAttribute.TestGetValue;
begin
  Self.A.Value  := '98 T140/1000';
  Self.A.Format := '99';

  CheckEquals(Self.A.Format + ' ' + Self.A.Encoding.EncodingName,
              Self.A.Value,
              'Value');
end;

procedure TestTIdSdpRTPMapAttribute.TestIsRTPMap;
begin
  Check(Self.A.IsRTPMap, 'Should be an RTP map');
end;

procedure TestTIdSdpRTPMapAttribute.TestSetValue;
begin
  Self.A.Value := '98 T140/1000';

  CheckEquals(TIdRTPT140Payload.ClassName,
              Self.A.Encoding.ClassName,
              'Encoding');
  CheckEquals(T140EncodingName,
              Self.A.Encoding.Name,
              'Encoding name');
  CheckEquals(T140ClockRate,
              Self.A.Encoding.ClockRate,
              'Encoding clock rate');
  CheckEquals('',
              Self.A.Encoding.Parameters,
              'Encoding parameters');
  CheckEquals('98',
              Self.A.Format,
              'Format');
end;

procedure TestTIdSdpRTPMapAttribute.TestSetValueWithParameters;
begin
  Self.A.Value := '98 T140/1000/1';

  CheckEquals('1', Self.A.Encoding.Parameters, 'Encoding parameters');
end;

//******************************************************************************
//* TestTIdSdpBandwidth                                                        *
//******************************************************************************
//* TestTIdSdpBandwidth Public methods *****************************************

procedure TestTIdSdpBandwidth.SetUp;
begin
  inherited SetUp;

  Self.B := TIdSdpBandwidth.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpBandwidth.TearDown;
begin
  Self.S.Free;
  Self.B.Free;

  inherited TearDown;
end;

//* TestTIdSdpBandwidth Published methods **************************************

procedure TestTIdSdpBandwidth.TestAssign;
var
  Other: TIdSdpBandwidth;
begin
  Other := TIdSdpBandwidth.Create;
  try
    Other.BandwidthType := btConferenceTotal;
    Other.Bandwidth     := 42;

    Self.B.Assign(Other);
    Check(Other.BandwidthType = Self.B.BandwidthType, 'BandwidthType');
    CheckEquals(Other.BandwidthName, Self.B.BandwidthName, 'BandwidthName');
    CheckEquals(Other.Bandwidth, Self.B.Bandwidth, 'Bandwidth');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpBandwidth.TestAssignWithUnknownBandwidthType;
var
  Other: TIdSdpBandwidth;
begin
  Other := TIdSdpBandwidth.Create;
  try
    Other.BandwidthType := btUnknown;
    Other.BandwidthName := 'TIAS';
    Other.Bandwidth     := 42;

    Self.B.Assign(Other);
    Check(Other.BandwidthType = Self.B.BandwidthType, 'BandwidthType');
    CheckEquals(Other.BandwidthName, Self.B.BandwidthName, 'BandwidthName');
    CheckEquals(Other.Bandwidth, Self.B.Bandwidth, 'Bandwidth');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpBandwidth.TestPrintOn;
begin
  Self.B.BandwidthType := btConferenceTotal;
  Self.B.Bandwidth     := 42;

  Self.B.PrintOn(Self.S);

  CheckEquals('b=CT:42'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpBandwidth.TestPrintOnUnknownBandwidthType;
begin
  Self.B.BandwidthType := btUnknown;
  Self.B.BandwidthName := 'TIAS';
  Self.B.Bandwidth     := 42;

  Self.B.PrintOn(Self.S);

  CheckEquals('b=TIAS:42'#13#10, S.DataString, 'PrintOn with unknown bandwidth type');
end;

//******************************************************************************
//* TestTIdSdpConnection                                                       *
//******************************************************************************
//* TestTIdSdpConnection Public methods ****************************************

procedure TestTIdSdpConnection.SetUp;
begin
  inherited SetUp;

  Self.C := TIdSdpConnection.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpConnection.TearDown;
begin
  Self.S.Free;
  Self.C.Free;

  inherited TearDown;
end;

//* TestTIdSdpConnection Published methods *************************************

procedure TestTIdSdpConnection.TestAddressRoutableAddress;
const
  Binding   = '10.0.0.1';
  Localhost = '127.0.0.1';
begin
  Self.C.Address := Localhost;

  CheckEquals(Localhost,      Self.C.Address,         '1: Address not set');
  CheckEquals(Self.C.Address, Self.C.RoutableAddress, '1: RoutableAddress not defaulted to Address');

  Self.C.RoutableAddress := Binding;
  CheckEquals(Binding,   Self.C.RoutableAddress, '2: RoutableAddress value not overwritten');
  CheckEquals(Localhost, Self.C.Address,         '2: Address altered');

  Self.C.Address := '';
  Self.C.RoutableAddress := '';

  Self.C.RoutableAddress := Binding;
  CheckEquals(Binding,                Self.C.RoutableAddress, '3: RoutableAddress not set');
  CheckEquals(Self.C.RoutableAddress, Self.C.Address,         '3: Address not defaulted to RoutableAddress');

  Self.C.Address := Localhost;
  CheckEquals(Binding,   Self.C.RoutableAddress, '4: RoutableAddress altered');
  CheckEquals(Localhost, Self.C.Address,         '4: Address value not overwritten');
end;

procedure TestTIdSdpConnection.TestAssign;
var
  Other: TIdSdpConnection;
begin
  Other := TIdSdpConnection.Create;
  try
    Other.Address           := 'FF80::1';
    Other.AddressType       := Id_IPv6;
    Other.NetType           := Id_SDP_IN;
    Other.NumberOfAddresses := 2;
    Other.RoutableAddress   := 'F00F::1';
    Other.TTL               := 255;

    Self.C.Assign(Other);

    CheckEquals(Other.Address,           Self.C.Address,           'Address');
    Check      (Other.AddressType      = Self.C.AddressType,       'AddressType');
    CheckEquals(Other.NetType,           Self.C.NetType,           'NetType');
    CheckEquals(Other.NumberOfAddresses, Self.C.NumberOfAddresses, 'NumberOfAddresses');
    CheckEquals(Other.RoutableAddress,   Self.C.RoutableAddress,   'RoutableAddress');
    CheckEquals(Other.TTL,               Self.C.TTL,               'TTL');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpConnection.TestCopy;
var
  Copy: TIdSdpConnection;
begin
  Self.C.Address           := 'FF80::1';
  Self.C.AddressType       := Id_IPv6;
  Self.C.NetType           := Id_SDP_IN;
  Self.C.NumberOfAddresses := 2;
  Self.C.TTL               := 255;

  Copy := Self.C.Copy;
  try
    CheckEquals(Self.C.Address,           Copy.Address,           'Address');
    Check      (Self.C.AddressType      = Copy.AddressType,       'AddressType');
    CheckEquals(Self.C.NetType,           Copy.NetType,           'NetType');
    CheckEquals(Self.C.NumberOfAddresses, Copy.NumberOfAddresses, 'NumberOfAddresses');
    CheckEquals(Self.C.TTL,               Copy.TTL,               'TTL');
  finally
    Copy.Free;
  end;
end;

procedure TestTIdSdpConnection.TestPrintOnMulticast;
begin
  Self.C.Address     := '224.0.0.0';
  Self.C.AddressType := Id_IPv4;
  Self.C.NetType     := Id_SDP_IN;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 224.0.0.0'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnMulticastWithNumberNoTtl;
begin
  Self.C.Address           := '224.0.0.0';
  Self.C.AddressType       := Id_IPv4;
  Self.C.NetType           := Id_SDP_IN;
  Self.C.NumberOfAddresses := 127;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 224.0.0.0'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnMulticastWithTtl;
begin
  Self.C.Address     := '224.0.0.0';
  Self.C.AddressType := Id_IPv4;
  Self.C.NetType     := Id_SDP_IN;
  Self.C.TTL         := 127;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 224.0.0.0/127'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnMulticastWithTtlAndNumber;
begin
  Self.C.Address           := '224.0.0.0';
  Self.C.AddressType       := Id_IPv4;
  Self.C.NetType           := Id_SDP_IN;
  Self.C.NumberOfAddresses := 4;
  Self.C.TTL               := 127;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 224.0.0.0/127/4'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnUnicast;
begin
  Self.C.Address     := '0.0.0.255';
  Self.C.AddressType := Id_IPv4;
  Self.C.NetType     := Id_SDP_IN;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 0.0.0.255'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpConnection.TestPrintOnUsesRoutableAddress;
const
  Binding   = '10.0.0.1';
  LocalHost = '127.0.0.1';
begin
  Self.C.Address         := Localhost;
  Self.C.AddressType     := Id_IPv4;
  Self.C.NetType         := Id_SDP_IN;
  Self.C.RoutableAddress := Binding;

  Self.C.PrintOn(Self.S);

  CheckEquals('c=IN IP4 ' + Self.C.RoutableAddress + #13#10, S.DataString, 'PrintOn didn''t use the routable address');
end;

//******************************************************************************
//* TestTIdSdpKey                                                              *
//******************************************************************************
//* TestTIdSdpKey Public methods ***********************************************

procedure TestTIdSdpKey.SetUp;
begin
  inherited SetUp;

  Self.K := TIdSdpKey.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpKey.TearDown;
begin
  Self.S.Free;
  Self.K.Free;

  inherited TearDown;
end;

//* TestTIdSdpKey Published methods ********************************************

procedure TestTIdSdpKey.TestAssign;
var
  Other: TIdSdpKey;
begin
  Other := TIdSdpKey.Create;
  try
    Other.KeyType := ktURI;
    Other.Value := 'http://heehee.heehee/';

    Self.K.Assign(Other);
    Check(Other.KeyType = Self.K.KeyType, 'KeyType');
    CheckEquals(Other.Value, Self.K.Value, 'Value');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpKey.TestAssignWithUnknownKeyType;
var
  Other: TIdSdpKey;
begin
  Other := TIdSdpKey.Create;
  try
    Other.KeyType := ktUnknown;
    Other.KeyName := 'unknown-key-type';
    Other.Value   := '42';

    Self.K.Assign(Other);
    Check(Other.KeyType = Self.K.KeyType, 'KeyType');
    CheckEquals(Other.KeyName, Self.K.KeyName, 'KeyName');
    CheckEquals(Other.Value, Self.K.Value, 'Value');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpKey.TestPrintOnJustMethod;
begin
  Self.K.KeyType := ktPrompt;

  Self.K.PrintOn(Self.S);

  CheckEquals('k=prompt'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpKey.TestPrintOnMethodPlusValue;
begin
  Self.K.KeyType := ktUri;
  Self.K.Value   := 'tel://42';

  Self.K.PrintOn(Self.S);

  CheckEquals('k=uri:tel://42'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpKey.TestPrintOnPromptWithKeyData;
begin
  Self.K.KeyType := ktPrompt;
  Self.K.Value   := 'tel://42';

  Self.K.PrintOn(Self.S);

  CheckEquals('k=prompt'#13#10, S.DataString, 'PrintOn');
end;

procedure TestTIdSdpKey.TestPrintOnUnknownKeyType;
begin
  Self.K.KeyType := ktUnknown;
  Self.K.KeyName := 'rot26';
  Self.K.Value   := '42';

  Self.K.PrintOn(Self.S);

  CheckEquals('k=rot26:42'#13#10, S.DataString, 'PrintOn with unknown key type');
end;

procedure TestTIdSdpKey.TestPrintOnUriWithNoKeyData;
begin
  Self.K.KeyType := ktUri;

  Self.K.PrintOn(Self.S);

  CheckEquals('k=uri:'#13#10, S.DataString, 'PrintOn');
end;

//******************************************************************************
//* TestTIdSdpMediaDescription                                                 *
//******************************************************************************
//* TestTIdSdpMediaDescription Public methods **********************************

procedure TestTIdSdpMediaDescription.SetUp;
begin
  inherited SetUp;

  Self.M := TIdSdpMediaDescription.Create;
end;

procedure TestTIdSdpMediaDescription.TearDown;
begin
  Self.M.Free;

  inherited TearDown;
end;

//* TestTIdSdpMediaDescription Published methods *******************************

procedure TestTIdSdpMediaDescription.ConfigureComplicatedly(Desc: TIdSdpMediaDescription);
begin
  Desc.AddRTPMapAttribute('foo/42', 1);
  Desc.Bandwidths.Add;
  Desc.Bandwidths[0].Bandwidth := 666;
  Desc.Bandwidths[0].BandwidthType := btConferenceTotal;
  Desc.Connections.Add;
  Desc.Connections[0].AddressType := Id_IPv4;
  Desc.Connections[0].Address := '127.0.0.1';
  Desc.Connections[0].NetType := Id_SDP_IN;
  Desc.Connections[0].NumberOfAddresses := 2;
  Desc.Connections.Add;
  Desc.Connections[1].AddressType := Id_IPv4;
  Desc.Connections[1].Address := '::1';
  Desc.Connections[1].NetType := Id_SDP_IN;
  Desc.Connections[0].TTL := 1;
  Desc.AddFormat('2');
  Desc.AddFormat('4');
  Desc.Info := 'info';
  Desc.Key.KeyType := ktBase64;
  Desc.Key.Value := 'foo';
  Desc.MediaType := mtText;
  Desc.Port := 666;
  Desc.PortCount := 6;
  Desc.Protocol := Id_SDP_RTPAVP;
end;

//* TestTIdSdpMediaDescription Published methods *******************************

procedure TestTIdSdpMediaDescription.TestAddAttributeAndCount;
var
  I: Integer;
begin
  for I := 1 to 10 do begin
    Self.M.AddAttribute('foo', IntToStr(I));
    CheckEquals(I,
                Self.M.Attributes.Count,
                'Attribute not added, ' + IntToStr(I));
  end;
end;

procedure TestTIdSdpMediaDescription.TestAddRTPMapAttribute;
var
  A: TIdSdpRTPMapAttribute;
begin
  A := TIdSdpRTPMapAttribute.Create;
  try
    A.Value := '66 foo/42';

    Self.M.AddRTPMapAttribute(A.Encoding.EncodingName, StrToInt(A.Format));

    CheckEquals(1, Self.M.RTPMapAttributes.Count, 'Attribute not added');
    CheckEquals(A.AsString,
                Self.M.RTPMapAttributes[0].AsString,
                'RTP map attribute not added correctly');
  finally
    A.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestAddFormatAndFormatCount;
var
  I: Integer;
begin
  CheckEquals(0, Self.M.FormatCount, 'FormatCount on new media-description');

  for I := 1 to 1000 do begin
    Self.M.AddFormat(IntToStr(I));
    CheckEquals(I,
                Self.M.FormatCount,
                'FormatCount after ' + IntToStr(I) + ' AddFormat/s');
  end;
end;

procedure TestTIdSdpMediaDescription.TestAssign;
var
  NewDesc: TIdSdpMediaDescription;
begin
  Self.ConfigureComplicatedly(Self.M);

  NewDesc := TIdSdpMediaDescription.Create;
  try
    NewDesc.Assign(Self.M);
    Check(NewDesc.Equals(Self.M), 'NewDesc = M');
    Check(Self.M.Equals(NewDesc), 'M = NewDesc');
  finally
    NewDesc.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestAssignUnknownMediaType;
const
  UnknownMediaType = 'unknown-media-type';
var
  Other: TIdSdpMediaDescription;
begin
  Other := TIdSdpMediaDescription.Create;
  try
    Other.MediaName := UnknownMediaType;
    Other.MediaType := mtUnknown;

    Self.M.Assign(Other);

    Check(Other.MediaType = Self.M.MediaType, 'MediaType');
    CheckEquals(Other.MediaName, Self.M.MediaName, 'MediaName');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestEquals;
var
  I:         Integer;
  MediaDesc: TIdSdpMediaDescription;
begin
  Self.ConfigureComplicatedly(Self.M);

  MediaDesc := TIdSdpMediaDescription.Create;
  try
    Check(not Self.M.Equals(MediaDesc), 'Empty description');

    for I := 0 to Self.M.Attributes.Count - 1 do
      MediaDesc.AddAttribute(Self.M.Attributes[I].Name, Self.M.Attributes[I].Value);
    Check(not Self.M.Equals(MediaDesc), 'Attributes added');

    MediaDesc.Bandwidths.Add(Self.M.Bandwidths);
    Check(not Self.M.Equals(MediaDesc), 'Bandwidths added');

    MediaDesc.Connections.Add(Self.M.Connections);
    Check(not Self.M.Equals(MediaDesc), 'Connections added');

    for I := 0 to Self.M.FormatCount - 1 do
      MediaDesc.AddFormat(Self.M.Formats[I]);
    Check(not Self.M.Equals(MediaDesc), 'Formats added');

    MediaDesc.Info := Self.M.Info;
    Check(not Self.M.Equals(MediaDesc), 'Info added');

    MediaDesc.Key.Assign(Self.M.Key);
    Check(not Self.M.Equals(MediaDesc), 'Key added');

    MediaDesc.MediaType := Self.M.MediaType;
    Check(not Self.M.Equals(MediaDesc), 'MediaType added');

    MediaDesc.Port := Self.M.Port;
    Check(not Self.M.Equals(MediaDesc), 'Port added');

    MediaDesc.PortCount := Self.M.PortCount;
    Check(not Self.M.Equals(MediaDesc), 'Info added');

    MediaDesc.Protocol := Self.M.Protocol;
    Check(not Self.M.Equals(MediaDesc), 'Transport added');
  finally
    MediaDesc.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestFormatClear;
var
  I: Integer;
begin
  for I := 1 to 1000 do
    Self.M.AddFormat(IntToStr(I));

  Self.M.ClearFormats;
  CheckEquals(0, Self.M.FormatCount, 'FormatCount after ClearFormats');
end;

procedure TestTIdSdpMediaDescription.TestGetFormat;
var
  I: Integer;
begin
  for I := 0 to 999 do begin
    Self.M.AddFormat(IntToStr(I));
    CheckEquals(IntToStr(I), Self.M.Formats[I], 'Formats[' + IntToStr(I) + ']');
  end;
end;

procedure TestTIdSdpMediaDescription.TestHasFormat;
var
  Fmt: String;
begin
  Self.M.ClearFormats;
  Fmt := 'foo';
  Check(not Self.M.HasFormat(Fmt), 'Empty format list');
  Self.M.AddFormat(Fmt);
  Check(Self.M.HasFormat(Fmt), 'Format not added?');
end;

procedure TestTIdSdpMediaDescription.TestInitialState;
begin
  CheckEquals(1, Self.M.PortCount, 'PortCount');
end;

procedure TestTIdSdpMediaDescription.TestIsRefusedStream;
begin
  Self.M.Port := 8000;
  Check(not Self.M.IsRefusedStream, 'Port <> 0 but stream looks refused');

  Self.M.Port := 0;
  Check(Self.M.IsRefusedStream, 'Port = 0 but stream doesn''t look refused');
end;

procedure TestTIdSdpMediaDescription.TestIsText;
var
  MT: TIdSdpMediaType;
begin
  for MT := Low(TIdSdpMediaType) to High(TIdSdpMediaType) do begin
    Self.M.MediaType := MT;

    if (Self.M.MediaType = mtText) then
      Check(Self.M.IsText, 'Not marked as text when media type is ' + MediaTypeToStr(MT))
    else
      Check(not Self.M.IsText, 'Marked as text when media type is ' + MediaTypeToStr(MT))
  end;
end;

procedure TestTIdSdpMediaDescription.TestIsValidFormatRtpAvp;
begin
  Self.M.Protocol := Id_SDP_RTPAVP;
  Check(Self.M.IsValidFormat('0'),              'Format: "0"');
  Check(Self.M.IsValidFormat('96'),             'Format: "96"');
  Check(not Self.M.IsValidFormat('256'),        'Format: "256"');
  Check(not Self.M.IsValidFormat('-1'),         'Format: "-1"');
  Check(not Self.M.IsValidFormat('text/plain'), 'Format: "text/plain"');
end;

procedure TestTIdSdpMediaDescription.TestIsValidFormatRtpSavp;
begin
  Self.M.Protocol := Id_SDP_RTPSAVP;
  Check(Self.M.IsValidFormat('0'),              'Format: "0"');
  Check(Self.M.IsValidFormat('96'),             'Format: "96"');
  Check(not Self.M.IsValidFormat('256'),        'Format: "256"');
  Check(not Self.M.IsValidFormat('-1'),         'Format: "-1"');
  Check(not Self.M.IsValidFormat('text/plain'), 'Format: "text/plain"');
end;

procedure TestTIdSdpMediaDescription.TestIsValidFormatTcp;
begin
  Self.M.Protocol := Id_SDP_TCP;
  Check(not Self.M.IsValidFormat('0'),        'Format: "0"');
  Check(not Self.M.IsValidFormat('96'),       'Format: "96"');
  Check(not Self.M.IsValidFormat('256'),      'Format: "256"');
  Check(not Self.M.IsValidFormat('-1'),       'Format: "-1"');
  Check(Self.M.IsValidFormat('text/plain'),   'Format: "text/plain"');
  Check(Self.M.IsValidFormat('text/x-vcard'), 'Format: "text/x-vcard"');
  Check(Self.M.IsValidFormat('video/mpeg'),   'Format: "video/mpeg"');
end;

procedure TestTIdSdpMediaDescription.TestIsValidFormatUdp;
begin
  Self.M.Protocol := Id_SDP_udp;
  Check(not Self.M.IsValidFormat('0'),        'Format: "0"');
  Check(not Self.M.IsValidFormat('96'),       'Format: "96"');
  Check(not Self.M.IsValidFormat('256'),      'Format: "256"');
  Check(not Self.M.IsValidFormat('-1'),       'Format: "-1"');
  Check(Self.M.IsValidFormat('text/plain'),   'Format: "text/plain"');
  Check(Self.M.IsValidFormat('text/x-vcard'), 'Format: "text/x-vcard"');
  Check(Self.M.IsValidFormat('video/mpeg'),   'Format: "video/mpeg"');
end;

procedure TestTIdSdpMediaDescription.TestMediaTypeAsString;
begin
  Self.M.MediaType := mtText;
  CheckEquals(MediaTypeToStr(Self.M.MediaType), Self.M.MediaTypeAsString, 'Known media type');

  Self.M.MediaType := mtAudio;
  CheckEquals(MediaTypeToStr(Self.M.MediaType), Self.M.MediaTypeAsString, 'Changed media type');

  Self.M.MediaName := 'foo';
  CheckEquals(MediaTypeToStr(Self.M.MediaType), Self.M.MediaTypeAsString, 'Known media type, MediaName not empty');

  Self.M.MediaType := mtUnknown;
  CheckEquals(Self.M.MediaName, Self.M.MediaTypeAsString, 'Unknown media type, MediaName not empty');

  Self.M.MediaName := '';
  CheckEquals(Self.M.MediaName, Self.M.MediaTypeAsString, 'Unknown media type, MediaName empty (an invalid case)');
end;

procedure TestTIdSdpMediaDescription.TestPrintOnBasic;
var
  S: TStringStream;
begin
  Self.M.MediaType := mtAudio;
  Self.M.Port      := 49230;
  Self.M.Protocol := 'RTP/AVP';

  S := TStringStream.Create('');
  try
    Self.M.PrintOn(S);
    CheckEquals('m=audio 49230 RTP/AVP'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestPrintOnFull;
var
  Conn: TIdSdpConnection;
  S: TStringStream;
begin
  Self.M.AddAttribute(RTPMapAttribute, '98 L16/16000/2');

  Self.M.AddAttribute(RTPMapAttribute, '100 ' + T140Encoding);

  Self.M.Bandwidths.Add;
  Self.M.Bandwidths[0].Bandwidth := 666;
  Self.M.Bandwidths[0].BandwidthType := btRS;

  Self.M.Bandwidths.Add;
  Self.M.Bandwidths[1].Bandwidth := 42;
  Self.M.Bandwidths[1].BandwidthType := btConferenceTotal;

  Self.M.Connections.Add;
  Conn := Self.M.Connections[0];
  Conn.Address           := '127.0.0.1';
  Conn.AddressType       := Id_IPv4;
  Conn.NetType           := Id_SDP_IN;
  Conn.TTL               := 5;
  Conn.NumberOfAddresses := 5;
  Self.M.Info            := 'Cthulhu Speaks';
  Self.M.Key.KeyType     := ktBase64;
  Self.M.Key.Value       := 'DEADBEEF';
  Self.M.MediaType       := mtAudio;
  Self.M.Port            := 49230;
  Self.M.Protocol        := 'RTP/AVP';

  S := TStringStream.Create('');
  try
    Self.M.PrintOn(S);
    CheckEquals('m=audio 49230 RTP/AVP'#13#10
              + 'i=Cthulhu Speaks'#13#10
              + 'c=IN IP4 127.0.0.1/5/5'#13#10
              + 'b=RS:666'#13#10
              + 'b=CT:42'#13#10
              + 'k=base64:DEADBEEF'#13#10
              + 'a=rtpmap:98 L16/16000/2'#13#10
              + 'a=rtpmap:100 ' + T140Encoding + #13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestPrintOnWithPortCount;
var
  S: TStringStream;
begin
  Self.M.AddFormat('0');
  Self.M.MediaType := mtAudio;
  Self.M.Port      := 49230;
  Self.M.PortCount := 4;
  Self.M.Protocol  := 'RTP/AVP';

  S := TStringStream.Create('');
  try
    Self.M.PrintOn(S);

    CheckEquals('m=audio 49230/4 RTP/AVP 0'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestPrintOnWithUnknownMediaType;
var
  S: TStringStream;
begin
  Self.M.MediaType := mtUnknown;
  Self.M.MediaName := 'whiteboard';

  // Stuff not relevant to the test per se, but just to make the m header well-formed.
  Self.M.AddFormat('0');
  Self.M.Port      := 49230;
  Self.M.Protocol  := 'RTP/AVP';

  S := TStringStream.Create('');
  try
    Self.M.PrintOn(S);

    CheckEquals('m=whiteboard 49230 RTP/AVP 0'#13#10,
                S.DataString,
                'PrintOn with unknown media type');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestUsesBinding;
var
  Binding: TIdConnectionBindings;
begin
  Self.ConfigureComplicatedly(Self.M);

  Binding := TIdConnectionBindings.Create;
  try
    Binding.LocalIP   := Self.M.Connections[0].Address;
    Binding.LocalPort := Self.M.Port;
    Check(Self.M.UsesBinding(Binding), 'IPv4 binding');

    Binding.LocalIP   := Self.M.Connections[0].Address;
    Binding.LocalPort := Self.M.Port + 1;
    Check(not Self.M.UsesBinding(Binding), 'IPv4 binding with unused port');

    Binding.LocalIP   := TIdIPAddressParser.IncIPAddress(Self.M.Connections[0].Address);
    Binding.LocalPort := Self.M.Port;
    Check(not Self.M.UsesBinding(Binding), 'IPv4 binding with unused address');

    Binding.LocalIP   := Self.M.Connections[1].Address;
    Binding.LocalPort := Self.M.Port;
    Check(Self.M.UsesBinding(Binding), 'IPv6 binding');

    Binding.LocalIP   := Self.M.Connections[1].Address;
    Binding.LocalPort := Self.M.Port + 1;
    Check(not Self.M.UsesBinding(Binding), 'IPv6 binding with unused port');

    Binding.LocalIP   := TIdIPAddressParser.IncIPAddress(Self.M.Connections[1].Address);
    Binding.LocalPort := Self.M.Port;
    Check(not Self.M.UsesBinding(Binding), 'IPv6 binding with unused address');

    Self.M.Connections.Remove(Self.M.Connections[1]);
    Self.M.PortCount := 2;

    Binding.LocalIP   := Self.M.Connections[0].Address;
    Binding.LocalPort := Self.M.Port;
    Check(Self.M.UsesBinding(Binding), 'IPv4 binding (with port count)');

    Binding.LocalPort := Self.M.Port + 2;
    Check(Self.M.UsesBinding(Binding), 'IPv4 binding (with port count), 2nd port');
    Binding.LocalPort := Self.M.Port + 1;
    Check(not Self.M.UsesBinding(Binding), 'IPv4 binding (with port count), on RTCP port ');
    Binding.LocalPort := Self.M.Port + 3;
    Check(not Self.M.UsesBinding(Binding), 'IPv4 binding (with port count), on 2nd RTCP port');
    Binding.LocalPort := Self.M.Port + 4;
    Check(not Self.M.UsesBinding(Binding), 'IPv4 binding (with port count), unused port');
  finally
    Binding.Free;
  end;
end;

procedure TestTIdSdpMediaDescription.TestUsesRtpProtocol;
begin
  Self.M.Protocol := Id_SDP_RTPAVP;
  Check(Self.M.UsesRtpProtocol, Id_SDP_RTPAVP);

  Self.M.Protocol := Id_SDP_RTPSAVP;
  Check(Self.M.UsesRtpProtocol, Id_SDP_RTPSAVP);

  Self.M.Protocol := 'RTP/unknown';
  Check(Self.M.UsesRtpProtocol, 'RTP/unknown');

  Self.M.Protocol := Id_SDP_TCP;
  Check(not Self.M.UsesRtpProtocol, Id_SDP_TCP);
end;

//******************************************************************************
//* TestTIdSdpOrigin                                                           *
//******************************************************************************
//* TestTIdSdpOrigin Public methods ********************************************

procedure TestTIdSdpOrigin.SetUp;
begin
  inherited SetUp;

  Self.O := TIdSdpOrigin.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpOrigin.TearDown;
begin
  Self.S.Free;
  Self.O.Free;

  inherited TearDown;
end;

//* TestTIdSdpOrigin Published methods *****************************************

procedure TestTIdSdpOrigin.TestAssign;
var
  NewO: TIdSdpOrigin;
begin
  Self.O.Address := 'tessier-ashpool.co.luna';
  Self.O.AddressType := Id_IPv6;
  Self.O.NetType := 'IN';
  Self.O.SessionID := 'rock-the-casbah';
  Self.O.SessionVersion := '1';
  Self.O.Username := 'wintermute';

  NewO := TIdSdpOrigin.Create;
  try
    NewO.Assign(Self.O);
    CheckEquals(Self.O.Address,        NewO.Address,        'Address');
    Check(      Self.O.AddressType =   NewO.AddressType,    'AddressType');
    CheckEquals(Self.O.NetType,        NewO.NetType,        'NetType');
    CheckEquals(Self.O.SessionID,      NewO.SessionID,      'SessionID');
    CheckEquals(Self.O.SessionVersion, NewO.SessionVersion, 'SessionVersion');
    CheckEquals(Self.O.Username,       NewO.Username,       'Username');
  finally
    NewO.Free;
  end;
end;

procedure TestTIdSdpOrigin.TestPrintOn;
begin
  Self.O.Address        := 'www.example.com';
  Self.O.AddressType    := Id_IPv6;
  Self.O.NetType        := Id_SDP_IN;
  Self.O.SessionID      := 'side0f';
  Self.O.SessionVersion := 'beef';
  Self.O.Username       := 'Holy_Cow';

  Self.O.PrintOn(S);

  CheckEquals('o=Holy_Cow side0f beef IN IP6 www.example.com'#13#10,
              S.DataString,
              'PrintOn');
end;

procedure TestTIdSdpOrigin.TestUsernameEncode;
var
  NihongoSpace: WideString;
  UnicodeUsername: String;
begin
  // The kanji for what, romanised, reads "NihongoSpace".
  NihongoSpace := WideChar($65E5);
  NihongoSpace := NihongoSpace + WideChar($672C) + WideChar($8A9E) + WideChar(' ');

  UnicodeUsername := UTF16LEToUTF8(NihongoSpace);

  CheckEquals('one',     Self.O.UsernameEncode('one'),     'one');
  CheckEquals('one_two', Self.O.UsernameEncode('one_two'), 'one_two');

  CheckEquals(UTF16LEToUTF8(StringReplaceW(NihongoSpace, ' ', '_', true)),
              Self.O.UsernameEncode(UnicodeUsername), '"nihongo" + space');
end;

procedure TestTIdSdpOrigin.TestUsernameWithSpaces;
const
  SpacedUsername = 'Holy Cow';
begin
  Self.O.Address        := 'www.example.com';
  Self.O.AddressType    := Id_IPv6;
  Self.O.NetType        := Id_SDP_IN;
  Self.O.SessionID      := 'side0f';
  Self.O.SessionVersion := 'beef';
  Self.O.Username       := SpacedUsername;

  Self.O.PrintOn(S);

  CheckEquals('o=Holy_Cow side0f beef IN IP6 www.example.com'#13#10,
              S.DataString,
              'PrintOn didn''t "escape" spaces in username');

  CheckEquals(SpacedUsername, Self.O.Username, 'Username not encoded');
end;

//******************************************************************************
//* TestTIdSdpRepeat                                                           *
//******************************************************************************
//* TestTIdSdpRepeat Public methods ********************************************

procedure TestTIdSdpRepeat.SetUp;
begin
  inherited SetUp;

  Self.R := TIdSdpRepeat.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpRepeat.TearDown;
begin
  Self.S.Free;
  Self.R.Free;

  inherited TearDown;
end;

//* TestTIdSdpRepeat Published methods *****************************************

procedure TestTIdSdpRepeat.TestAssign;
var
  OrigValue: String;
  NewR:      TIdSdpRepeat;
begin
  OrigValue := '604800 3600 0 90000';
  Self.R.Value := OrigValue;

  NewR := TIdSdpRepeat.Create;
  try
    NewR.Assign(Self.R);
    CheckEquals(Self.R.Value,
                NewR.Value,
                'Assign');
  finally
    NewR.Free;
  end;
end;

procedure TestTIdSdpRepeat.TestPrintOn;
var
  OrigValue: String;
begin
  OrigValue := '604800 3600 0 90000';
  Self.R.Value := OrigValue;
  Self.R.PrintOn(Self.S);

  CheckEquals('r=' + OrigValue + #13#10,
              Self.S.DataString,
              'PrintOn');
end;

//******************************************************************************
//* TestTIdSdpTime                                                             *
//******************************************************************************
//* TestTIdSdpTime Public methods **********************************************

procedure TestTIdSdpTime.SetUp;
begin
  inherited SetUp;

  Self.T := TIdSdpTime.Create;
  Self.S := TStringStream.Create('');
end;

procedure TestTIdSdpTime.TearDown;
begin
  Self.S.Free;
  Self.T.Free;

  inherited TearDown;
end;

//* TestTIdSdpTime Published methods *******************************************

procedure TestTIdSdpTime.TestAssign;
var
  Other: TIdSdpTime;
begin
  Other := TIdSdpTime.Create;
  try
    Self.T.EndTime := $cafebabe;
    Self.T.StartTime := $deadbeef;
    Self.T.Repeats.Add.Value := '604800 3600 0 90000';
    Self.T.ZoneAdjustments.Add;

    Other.Assign(Self.T);

    CheckEquals(IntToHex(Self.T.EndTime, 16),
                IntToHex(Other.EndTime, 16),
                'EndTime');
    Check(Other.Repeats.Equals(Self.T.Repeats), 'Repeats');
    CheckEquals(IntToHex(Self.T.StartTime, 16),
                IntToHex(Other.StartTime, 16),
                'StartTime');
    Check(Other.ZoneAdjustments.Equals(Self.T.ZoneAdjustments),
          'ZoneAdjustments');
  finally

    Other.Free;
  end;
end;

procedure TestTIdSdpTime.TestPrintOn;
begin
  Self.T.EndTime   := $deadbeef;
  Self.T.StartTime := $cafebabe;

  Self.T.PrintOn(S);

  CheckEquals('t=3405691582 3735928559'#13#10,
              S.DataString,
              'PrintOn');
end;

procedure TestTIdSdpTime.TestPrintOnWithRepeats;
begin
  Self.T.EndTime   := $deadbeef;
  Self.T.StartTime := $cafebabe;
  Self.T.Repeats.Add;
  Self.T.Repeats[0].Value := '1d';

  Self.T.PrintOn(S);

  CheckEquals('t=3405691582 3735928559'#13#10
            + 'r=1d'#13#10,
              S.DataString,
              'PrintOn');
end;

procedure TestTIdSdpTime.TestPrintOnWithZoneAdjustments;
begin
  Self.T.EndTime   := $deadbeef;
  Self.T.StartTime := $cafebabe;
  Self.T.ZoneAdjustments.Add.Value := '3735928559 -2h';

  Self.T.PrintOn(S);

  CheckEquals('t=3405691582 3735928559'#13#10
            + 'z=3735928559 -2h'#13#10,
              S.DataString,
              'PrintOn');
end;

//******************************************************************************
//* TestTIdSdpAttributes                                                       *
//******************************************************************************
//* TestTIdSdpAttributes Public methods ****************************************

procedure TestTIdSdpAttributes.SetUp;
begin
  inherited SetUp;

  Self.A := TIdSdpAttributes.Create;
end;

procedure TestTIdSdpAttributes.TearDown;
begin
  Self.A.Free;

  inherited TearDown;
end;

//* TestTIdSdpAttributes Published methods *************************************

procedure TestTIdSdpAttributes.TestAddAndCount;
var
  Att: TIdSdpAttribute;
begin
  CheckEquals(0, Self.A.Count, 'Count on new list');
  Self.A.Add;
  CheckEquals(1, Self.A.Count, 'Count after Add');

  Att := TIdSdpAttribute.Create;
  try
    Self.A.Add(Att);
    CheckEquals(2, Self.A.Count, 'Count after Add(TIdSdpAttribute)');
  finally
    Att.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestAddMultipleAttributes;
var
  Atts: TIdSdpAttributes;
  I:    Integer;
begin
  Atts := TIdSdpAttributes.Create;
  try
    Atts.Add;
    Atts.Add;
    Atts.Add;

    for I := 0 to Atts.Count - 1 do
      Atts[I].Name := IntToStr(I);

    Self.A.Add(Atts);
    CheckEquals(Atts.Count,
                Self.A.Count,
                'Not all attributes added');

    for I := 0 to Self.A.Count - 1 do
      CheckEquals(IntToStr(I),
                  Self.A[I].Name,
                  'Name of attribute ' + IntToStr(I));
  finally
    Atts.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestAddUsingString;
begin
  Self.A.Add('foo:bar');
  CheckEquals(TIdSdpAttribute.ClassName,
              Self.A[0].ClassName,
              'Incorrect class added from ''foo:bar''');

  Self.A.Add('rtpmap:98 T140/1000');
  CheckEquals(TIdSdpRTPMapAttribute.ClassName,
              Self.A[1].ClassName,
              'Incorrect class added from ''rtpmap:98 T140/1000''');
end;

procedure TestTIdSdpAttributes.TestAssign;
var
  Other: TIdSdpAttributes;
begin
  Self.A.Add.Name := '1';

  Other := TIdSdpAttributes.Create;
  try
    Other.Add.Name := '4';
    Other.Add.Name := '5';
    Other.Add.Name := '6';

    Self.A.Assign(Other);

    CheckEquals(3,   Self.A.Count,   'Not all Attributes copied across');
    CheckEquals('4', Self.A[0].Name, 'First Attribute');
    CheckEquals('5', Self.A[1].Name, 'Second Attribute');
    CheckEquals('6', Self.A[2].Name, 'Third Attribute');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestClear;
begin
  Self.A.Add;
  Self.A.Add;
  Self.A.Add;

  Self.A.Clear;
  CheckEquals(0, Self.A.Count, 'Count after clear');
end;

procedure TestTIdSdpAttributes.TestDirection;
var
  Att: TIdSdpAttribute;
begin
  Check(sdSendRecv = Self.A.Direction,
       'default');

  Att := Self.A.Add;

  Att.Name := 'sendrecv';
  Check(sdSendRecv = Self.A.Direction,
       'sendrecv');

  Att.Name := 'recvonly';
  Check(sdrecvonly = Self.A.Direction,
       'recvonly');

  Att.Name := 'sendonly';
  Check(sdsendonly = Self.A.Direction,
       'sendonly');

  Att.Name := 'inactive';
  Check(sdinactive = Self.A.Direction,
       'inactive');
end;

procedure TestTIdSdpAttributes.TestEquals;
var
  Other: TIdSdpAttributes;
begin
  Other := TIdSdpAttributes.Create;
  try
    Check(Self.A.Equals(Other), 'Self.A = Other; empty list');
    Check(Self.A.Equals(Other), 'Other = Self.A; empty list');

    Self.A.Add.Name := '2';
    Self.A.Add.Name := '4';

    Other.Add(Self.A[1]);
    Check(not Self.A.Equals(Other), 'Self.A <> Other');
    Check(not Self.A.Equals(Other), 'Other <> Self.A');

    Other.Add(Self.A[0]);

    Check(Self.A.Equals(Other), 'Self.A = Other; non-empty list');
    Check(Self.A.Equals(Other), 'Other = Self.A; non-empty list');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestHasAttribute;
var
  Att: TIdSdpAttribute;
begin
  Att := TIdSdpAttribute.Create;
  try
    Att.Name  := 'foo';
    Att.Value := 'bar';

    Check(not Self.A.HasAttribute(Att), 'Empty list');

    Self.A.Add('foo=baz');
    Check(not Self.A.HasAttribute(Att), 'Non-empty list without Att');

    Self.A.Add(Att);
    Check(Self.A.HasAttribute(Att), 'Non-empty list with an equivalent attribute to Att');
  finally
    Att.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestHasAttributeNamed;
const
  AttributeName        = 'foo';
  AnotherAttributeName = 'bar';
begin
  Check(not Self.A.HasAttributeNamed(AttributeName), 'Empty list');

  Self.A.Add(AnotherAttributeName);
  Check(not Self.A.HasAttributeNamed(AttributeName), 'Non-empty list without the ' + AttributeName + ' attribute');

  Self.A.Add(AttributeName);
  Check(Self.A.HasAttributeNamed(AttributeName), 'Non-empty list with the ' + AttributeName + ' attribute');

  Self.A.Remove(Self.A[1]);
  Check(not Self.A.HasAttributeNamed(AttributeName), 'Non-empty list with the ' + AttributeName + ' attribute removed');
end;

procedure TestTIdSdpAttributes.TestPrintOn;
var
  S: TStringStream;
begin
  Self.A.Add;
  Self.A.Add;

  Self.A[0].Name  := 'rtpmap';
  Self.A[0].Value := '98 T140/1000';
  Self.A[1].Name  := 'dead';
  Self.A[1].Value := 'beef';

  S := TStringStream.Create('');
  try
    Self.A.PrintOn(S);
    CheckEquals('a=rtpmap:98 T140/1000'#13#10
              + 'a=dead:beef'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpAttributes.TestSetDirection;
begin
  Self.A.Direction := sdRecvOnly;
  Self.A.Direction := sdInactive;
  CheckEquals(DirectionToStr(sdInactive),
              DirectionToStr(Self.A.Direction),
              'Direction attribute not changed');
end;

procedure TestTIdSdpAttributes.TestSetSetupType;
begin
  Self.A.SetupType := stHoldConn;
  CheckEquals(SetupTypeToStr(stHoldConn),
              SetupTypeToStr(Self.A.SetupType),
              'Setup not set');

  Self.A.SetupType := stActive;
  CheckEquals(SetupTypeToStr(stActive),
              SetupTypeToStr(Self.A.SetupType),
              'Setup not set');

  Self.A.SetupType := stActPass;
  CheckEquals(SetupTypeToStr(stActPass),
              SetupTypeToStr(Self.A.SetupType),
              'Setup not set');
end;

procedure TestTIdSdpAttributes.TestSetupType;
begin
  CheckEquals(SetupTypeToStr(stHoldConn),
              SetupTypeToStr(Self.A.SetupType),
              'Default setting');

  Self.A.Add(SetupAttribute + ':foo');
  CheckEquals(SetupTypeToStr(stUnknown),
              SetupTypeToStr(Self.A.SetupType),
              'Unknown setup attribute');

  Self.A[0].Value := SetupTypeToStr(stActive);
  CheckEquals(SetupTypeToStr(stActive),
              SetupTypeToStr(Self.A.SetupType),
              'Known setup attribute');
end;

procedure TestTIdSdpAttributes.TestInitialSetDirection;
begin
  CheckEquals(0, Self.A.Count, 'New Attributes not empty');

  Self.A.Direction := sdInactive;
  CheckEquals(1, Self.A.Count, 'Direction attribute not added');

  CheckEquals(DirectionToStr(sdInactive),
              DirectionToStr(Self.A.Direction),
              'Direction attribute incorrect');
end;

procedure TestTIdSdpAttributes.TestValueFor;
const
  AttributeName        = 'foo';
  AttributeValue       = 'baz';
  AnotherAttributeName = 'bar';
begin
  CheckEquals('', Self.A.ValueFor(AttributeName), 'Empty list');

  Self.A.Add(AnotherAttributeName);
  CheckEquals('', Self.A.ValueFor(AttributeName), 'No such attribute');

  Self.A.Add(AttributeName + ':' + AttributeValue);
  CheckEquals(AttributeValue, Self.A.ValueFor(AttributeName), 'Attribute present');
end;

//******************************************************************************
//* TestTIdSdpRTPMapAttributes                                                 *
//******************************************************************************
//* TestTIdSdpRTPMapAttributes Public methods **********************************

procedure TestTIdSdpRTPMapAttributes.SetUp;
begin
  inherited SetUp;

  Self.A := TIdSdpRTPMapAttributes.Create;
end;

procedure TestTIdSdpRTPMapAttributes.TearDown;
begin
  Self.A.Free;

  inherited TearDown;
end;

//* TestTIdSdpRTPMapAttributes Published methods *******************************

procedure TestTIdSdpRTPMapAttributes.TestAddAndCount;
var
  Att: TIdSdpRTPMapAttribute;
begin
  CheckEquals(0, Self.A.Count, 'Count on new list');
  Self.A.Add;
  CheckEquals(1, Self.A.Count, 'Count after Add');

  Att := TIdSdpRTPMapAttribute.Create;
  try
    Self.A.Add(Att);
    CheckEquals(2, Self.A.Count, 'Count after Add(TIdSdpRTPMapAttribute)');
  finally
    Att.Free;
  end;
end;

procedure TestTIdSdpRTPMapAttributes.TestAddMultipleAttributes;
var
  Atts: TIdSdpRTPMapAttributes;
  I:    Integer;
begin
  Atts := TIdSdpRTPMapAttributes.Create;
  try
    Atts.Add;
    Atts.Add;
    Atts.Add;

    for I := 0 to Atts.Count - 1 do
      Atts[I].Format := IntToStr(I);

    Self.A.Add(Atts);
    CheckEquals(Atts.Count,
                Self.A.Count,
                'Not all RTPMapAttributes added');

    for I := 0 to Self.A.Count - 1 do
      CheckEquals(IntToStr(I),
                  Self.A[I].Format,
                  'PayloadType of RTPMapAttribute ' + IntToStr(I));
  finally
    Atts.Free;
  end;
end;

procedure TestTIdSdpRTPMapAttributes.TestAssign;
var
  Other: TIdSdpRTPMapAttributes;
begin
  Self.A.Add.Value := '1 PCMU/1';

  Other := TIdSdpRTPMapAttributes.Create;
  try
    Other.Add.Value := '4 PCMU/4';
    Other.Add.Value := '5 PCMU/5';
    Other.Add.Value := '6 PCMU/6';

    Self.A.Assign(Other);

    CheckEquals(3,          Self.A.Count,    'Not all rtpmap attributes copied across');
    CheckEquals('4 PCMU/4', Self.A[0].Value, 'First rtpmap attribute');
    CheckEquals('5 PCMU/5', Self.A[1].Value, 'Second rtpmap attribute');
    CheckEquals('6 PCMU/6', Self.A[2].Value, 'Third rtpmap attribute');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpRTPMapAttributes.TestClear;
begin
  Self.A.Add;
  Self.A.Add;
  Self.A.Add;

  Self.A.Clear;
  CheckEquals(0, Self.A.Count, 'Count after clear');
end;

procedure TestTIdSdpRTPMapAttributes.TestEncodingFor;
const
  T140                 = 't140/1000';
  T140Format           = '96';
  TelephoneEvent       = 'telephone-event/8000';
  TelephoneEventFormat = '97';
begin
  CheckEquals('', Self.A.EncodingFor(''), 'Blank encoding name, empty list');
  CheckEquals('', Self.A.EncodingFor(T140Format), 'Empty list');

  Self.A.Add(TelephoneEventFormat + ' ' + TelephoneEvent);
  CheckEquals('', Self.A.EncodingFor(T140Format), 'No such rtpmap');

  Self.A.Add(T140Format + ' ' + T140);
  CheckEquals(T140,           Self.A.EncodingFor(T140Format),           T140 + ' format');
  CheckEquals(TelephoneEvent, Self.A.EncodingFor(TelephoneEventFormat), TelephoneEvent + ' format');
end;

procedure TestTIdSdpRTPMapAttributes.TestEquals;
var
  Other: TIdSdpRTPMapAttributes;
begin
  Other := TIdSdpRTPMapAttributes.Create;
  try
    Check(Self.A.Equals(Other), 'Self.A = Other; empty list');
    Check(Self.A.Equals(Other), 'Other = Self.A; empty list');

    Self.A.Add.Format := '2';
    Self.A.Add.Format := '4';

    Other.Add(Self.A[1]);
    Check(not Self.A.Equals(Other), 'Self.A <> Other');
    Check(not Self.A.Equals(Other), 'Other <> Self.A');

    Other.Add(Self.A[0]);

    Check(Self.A.Equals(Other), 'Self.A = Other; non-empty list');
    Check(Self.A.Equals(Other), 'Other = Self.A; non-empty list');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpRTPMapAttributes.TestFormatFor;
const
  T140                 = 't140/1000';
  T140Format           = '96';
  TelephoneEvent       = 'telephone-event/8000';
  TelephoneEventFormat = '97';
begin
  CheckEquals('', Self.A.FormatFor(''), 'Blank encoding name, empty list');
  CheckEquals('', Self.A.FormatFor(T140), 'Empty list');

  Self.A.Add(TelephoneEventFormat + ' ' + TelephoneEvent);
  CheckEquals('', Self.A.FormatFor(T140), 'No such rtpmap');

  Self.A.Add(T140Format + ' ' + T140);
  CheckEquals(T140Format,           Self.A.FormatFor(T140),           T140 + ' format');
  CheckEquals(TelephoneEventFormat, Self.A.FormatFor(TelephoneEvent), TelephoneEvent + ' format');
end;

procedure TestTIdSdpRTPMapAttributes.TestPrintOn;
var
  S: TStringStream;
begin
  Self.A.Add;
  Self.A.Add;

  Self.A[0].Name  := 'rtpmap';
  Self.A[0].Value := '98 t140/1000';
  Self.A[1].Name  := 'rtpmap';
  Self.A[1].Value := '99 dead/8000';

  S := TStringStream.Create('');
  try
    Self.A.PrintOn(S);
    CheckEquals('a=rtpmap:98 t140/1000'#13#10
              + 'a=rtpmap:99 dead/8000'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpBandwidths                                                       *
//******************************************************************************
//* TestTIdSdpBandwidths Public methods ****************************************

procedure TestTIdSdpBandwidths.SetUp;
begin
  inherited SetUp;

  Self.B := TIdSdpBandwidths.Create;
end;

procedure TestTIdSdpBandwidths.TearDown;
begin
  Self.B.Free;

  inherited TearDown;
end;

//* TestTIdSdpBandwidths Published methods *************************************

procedure TestTIdSdpBandwidths.TestAddAndCount;
var
  BW: TIdSdpBandwidth;
begin
  CheckEquals(0, Self.B.Count, 'Count on new list');
  Self.B.Add;
  CheckEquals(1, Self.B.Count, 'Count after Add');

  BW := TIdSdpBandwidth.Create;
  try
    Self.B.Add(BW);
    CheckEquals(2, Self.B.Count, 'Count after Add(TIdSdpBandwidth)');
  finally
    BW.Free;
  end;
end;

procedure TestTIdSdpBandwidths.TestAddMultipleBandwidths;
var
  Bs: TIdSdpBandwidths;
  I:    Integer;
begin
  Bs := TIdSdpBandwidths.Create;
  try
    Bs.Add;
    Bs.Add;
    Bs.Add;

    for I := 0 to Bs.Count - 1 do
      Bs[I].Bandwidth := I;

    Self.B.Add(Bs);
    CheckEquals(Bs.Count,
                Self.B.Count,
                'Not all bandwidths added');

    for I := 0 to Self.B.Count - 1 do
      CheckEquals(I,
                  Self.B[I].Bandwidth,
                  'Bandwidth of Bandwidth ' + IntToStr(I));
  finally
    Bs.Free;
  end;
end;

procedure TestTIdSdpBandwidths.TestAssign;
var
  Other: TIdSdpBandwidths;
begin
  Self.B.Add.Bandwidth := 1;

  Other := TIdSdpBandwidths.Create;
  try
    Other.Add.Bandwidth := 4;
    Other.Add.Bandwidth := 5;
    Other.Add.Bandwidth := 6;

    Self.B.Assign(Other);

    CheckEquals(3, Self.B.Count,        'Not all bandwidths copied across');
    CheckEquals(4, Self.B[0].Bandwidth, 'First bandwidth');
    CheckEquals(5, Self.B[1].Bandwidth, 'Second bandwidth');
    CheckEquals(6, Self.B[2].Bandwidth, 'Third bandwidth');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpBandwidths.TestClear;
begin
  Self.B.Add;
  Self.B.Add;
  Self.B.Add;

  Self.B.Clear;
  CheckEquals(0, Self.B.Count, 'Count after clear');
end;

procedure TestTIdSdpBandwidths.TestPrintOn;
var
  S: TStringStream;
begin
  Self.B.Add;
  Self.B.Add;

  Self.B[0].Bandwidth  := 666;
  Self.B[0].BandwidthType := btConferenceTotal;
  Self.B[1].Bandwidth  := 13;
  Self.B[1].BandwidthType := btApplicationSpecific;

  S := TStringStream.Create('');
  try
    Self.B.PrintOn(S);
    CheckEquals('b=CT:666'#13#10
              + 'b=AS:13'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpConnections                                                      *
//******************************************************************************
//* TestTIdSdpConnections Public methods ***************************************

procedure TestTIdSdpConnections.SetUp;
begin
  inherited SetUp;

  Self.C := TIdSdpConnections.Create;
end;

procedure TestTIdSdpConnections.TearDown;
begin
  Self.C.Free;

  inherited TearDown;
end;

//* TestTIdSdpConnections Published methods ************************************

procedure TestTIdSdpConnections.TestAddAndCount;
var
  Conn: TIdSdpConnection;
begin
  CheckEquals(0, Self.C.Count, 'Count on new list');
  Self.C.Add;
  CheckEquals(1, Self.C.Count, 'Count after Add');

  Conn := TIdSdpConnection.Create;
  try
    Self.C.Add(Conn);
    CheckEquals(2, Self.C.Count, 'Count after Add(TIdSdpConnection)');
  finally
    Conn.Free;
  end;
end;

procedure TestTIdSdpConnections.TestAddConnection;
begin
  Self.C.AddConnection('IP', Id_IPv6, '2002:5156:4019:2::1', 69);
  CheckEquals(1, Self.C.Count, 'Count after AddConnection()');

  CheckEquals('IP',                  Self.C[0].NetType,     'NetType');
  Check(      Id_IPv6 =              Self.C[0].AddressType, 'AddressType');
  CheckEquals('2002:5156:4019:2::1', Self.C[0].Address,     'Address');
  CheckEquals(69,                    Self.C[0].TTL,         'TTL');
end;

procedure TestTIdSdpConnections.TestAddMultipleConnections;
var
  I:              Integer;
  NewConnections: TIdSdpConnections;
begin
  NewConnections := TIdSdpConnections.Create;
  try
    NewConnections.Add;
    NewConnections.Add;
    NewConnections.Add;

    for I := 0 to NewConnections.Count - 1 do
      NewConnections[I].TTL := I;

    Self.C.Add(NewConnections);

    CheckEquals(NewConnections.Count,
                Self.C.Count,
                'Not all connections added');

    for I := 0 to Self.C.Count - 1 do
      CheckEquals(NewConnections[I].TTL,
                  Self.C[I].TTL,
                  'TTL on connection ' + IntToStr(I));
  finally
    NewConnections.Free;
  end;
end;

procedure TestTIdSdpConnections.TestAssign;
var
  Other: TIdSdpConnections;
begin
  Self.C.Add;
  Self.C.Add;
  Self.C.Add;

  Other := TIdSdpConnections.Create;
  try
    Other.Add;
    Other[0].Address           := 'FF80::1';
    Other[0].AddressType       := Id_IPv6;
    Other[0].NetType           := Id_SDP_IN;
    Other[0].NumberOfAddresses := 2;
    Other[0].TTL               := 255;

    Self.C.Assign(Other);

    CheckEquals(1, Self.C.Count, 'Not all Connections copied across');

    CheckEquals(Other[0].Address,
                Self.C[0].Address,
                'Address');
    Check(      Other[0].AddressType = Self.C[0].AddressType,
                'AddressType');
    CheckEquals(Other[0].NetType,
                Self.C[0].NetType,
                'NetType');
    CheckEquals(Other[0].NumberOfAddresses,
                Self.C[0].NumberOfAddresses,
                'NumberOfAddresses');
    CheckEquals(Other[0].TTL,
                Self.C[0].TTL,
                'TTL');

  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpConnections.TestClear;
begin
  Self.C.Add;
  Self.C.Add;
  Self.C.Add;

  Self.C.Clear;
  CheckEquals(0, Self.C.Count, 'Count after clear');
end;

procedure TestTIdSdpConnections.TestPrintOn;
var
  S: TStringStream;
begin
  Self.C.Add;
  Self.C.Add;

  Self.C[0].Address := '127.0.0.1';
  Self.C[0].AddressType := Id_IPv4;
  Self.C[0].NetType := 'IN';
  Self.C[0].NumberOfAddresses := 1;
  Self.C[0].TTL := 0;
  Self.C[1].Address := '::1';
  Self.C[1].AddressType := Id_IPv6;
  Self.C[1].NetType := 'IN';
  Self.C[1].NumberOfAddresses := 2;
  Self.C[1].TTL := 1;

  S := TStringStream.Create('');
  try
    Self.C.PrintOn(S);
    CheckEquals('c=IN IP4 127.0.0.1'#13#10
              + 'c=IN IP6 ::1/1/2'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpMediaDescriptions                                                *
//******************************************************************************
//* TestTIdSdpMediaDescriptions Public methods *********************************

procedure TestTIdSdpMediaDescriptions.SetUp;
begin
  inherited SetUp;

  Self.M := TIdSdpMediaDescriptions.Create;
end;

procedure TestTIdSdpMediaDescriptions.TearDown;
begin
  Self.M.Free;

  inherited TearDown;
end;

//* TestTIdSdpMediaDescriptions Published methods ******************************

procedure TestTIdSdpMediaDescriptions.TestAddAndCount;
var
  Desc: TIdSdpMediaDescription;
begin
  CheckEquals(0, Self.M.Count, 'Count on new list');
  Self.M.Add;
  CheckEquals(1, Self.M.Count, 'Count after Add');

  Desc := TIdSdpMediaDescription.Create;
  try
    Self.M.Add(Desc);
    CheckEquals(2, Self.M.Count, 'Count after Add(TIdSdpMediaDescription)');
  finally
    Desc.Free;
  end;
end;

procedure TestTIdSdpMediaDescriptions.TestAssign;
var
  Other: TIdSdpMediaDescriptions;
begin
  Self.M.Add.Info := '1';

  Other := TIdSdpMediaDescriptions.Create;
  try
    Other.Add.Info := '4';
    Other.Add.Info := '5';
    Other.Add.Info := '6';

    Self.M.Assign(Other);

    CheckEquals(3,   Self.M.Count,   'Not all media descriptions copied across');
    CheckEquals('4', Self.M[0].Info, 'First media description');
    CheckEquals('5', Self.M[1].Info, 'Second media description');
    CheckEquals('6', Self.M[2].info, 'Third media description');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpMediaDescriptions.TestAllDescriptionsHaveConnections;
begin
  Check(Self.M.AllDescriptionsHaveConnections, 'Trivial case - empty list');

  Self.M.Add;
  Check(not Self.M.AllDescriptionsHaveConnections,
        'One item with no connection');

  Self.M[0].Connections.Add;
  Check(Self.M.AllDescriptionsHaveConnections,
        'One item now has a connection');

  Self.M.Add;
  Check(not Self.M.AllDescriptionsHaveConnections,
        'A second item, with no connection');

  Self.M[1].Connections.Add;
  Check(Self.M.AllDescriptionsHaveConnections,
        'Both items now have connections');
end;

procedure TestTIdSdpMediaDescriptions.TestClear;
begin
  Self.M.Add;
  Self.M.Add;
  Self.M.Add;

  Self.M.Clear;
  CheckEquals(0, Self.M.Count, 'Count after clear');
end;

procedure TestTIdSdpMediaDescriptions.TestPrintOn;
var
  S: TStringStream;
begin
  Self.M.Add;
  Self.M.Add;

  Self.M[0].MediaType := mtAudio;
  Self.M[0].AddFormat('0');
  Self.M[0].Port := 0;
  Self.M[0].Protocol := 'TCP';
  Self.M[1].MediaType := mtText;
  Self.M[1].Port := 1;
  Self.M[1].Protocol := 'RTP/AVP';
  Self.M[1].AddFormat('1');
  Self.M[1].AddFormat('2');
  Self.M[1].AddFormat('3');

  S := TStringStream.Create('');
  try
    Self.M.PrintOn(S);
    CheckEquals('m=audio 0 TCP 0'#13#10
              + 'm=text 1 RTP/AVP 1 2 3'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpRepeats                                                          *
//******************************************************************************
//* TestTIdSdpRepeats Public methods *******************************************

procedure TestTIdSdpRepeats.SetUp;
begin
  inherited SetUp;

  Self.R := TIdSdpRepeats.Create;
end;

procedure TestTIdSdpRepeats.TearDown;
begin
  Self.R.Free;

  inherited TearDown;
end;

//* TestTIdSdpRepeats Published methods ****************************************

procedure TestTIdSdpRepeats.TestAddAndCount;
var
  Rpt: TIdSdpRepeat;
begin
  CheckEquals(0, Self.R.Count, 'Count on new list');
  Self.R.Add;
  CheckEquals(1, Self.R.Count, 'Count after Add');

  Rpt := TIdSdpRepeat.Create;
  try
    Self.R.Add(Rpt);
    CheckEquals(2, Self.R.Count, 'Count after Add(TIdSdpBandwidth)');
  finally
    Rpt.Free;
  end;
end;

procedure TestTIdSdpRepeats.TestAssign;
var
  Other: TIdSdpRepeats;
begin
  Self.R.Add.Value := '1';

  Other := TIdSdpRepeats.Create;
  try
    Other.Add.Value := '4';
    Other.Add.Value := '5';
    Other.Add.Value := '6';

    Self.R.Assign(Other);

    CheckEquals(3,   Self.R.Count,    'Not all repeats copied across');
    CheckEquals('4', Self.R[0].Value, 'First repeat');
    CheckEquals('5', Self.R[1].Value, 'Second repeat');
    CheckEquals('6', Self.R[2].Value, 'Third repeat');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpRepeats.TestClear;
begin
  Self.R.Add;
  Self.R.Add;
  Self.R.Add;

  Self.R.Clear;
  CheckEquals(0, Self.R.Count, 'Count after clear');
end;

procedure TestTIdSdpRepeats.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.R.Add;
    Self.R.Add;

    Self.R[0].Value := '1w';
    Self.R[1].Value := '1h 1d 1w';

    Self.R.PrintOn(S);

    CheckEquals('r=1w'#13#10
              + 'r=1h 1d 1w'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpTimes                                                            *
//******************************************************************************
//* TestTIdSdpTimes Public methods *********************************************

procedure TestTIdSdpTimes.SetUp;
begin
  inherited SetUp;

  Self.T := TIdSdpTimes.Create;
end;

procedure TestTIdSdpTimes.TearDown;
begin
  Self.T.Free;

  inherited TearDown;
end;

//* TestTIdSdpTimes Published methods ******************************************

procedure TestTIdSdpTimes.TestAddAndCount;
var
  Time: TIdSdpTime;
begin
  CheckEquals(0, Self.T.Count, 'Count on new list');
  Self.T.Add;
  CheckEquals(1, Self.T.Count, 'Count after Add');

  Time := TIdSdpTime.Create;
  try
    Self.T.Add(Time);
    CheckEquals(2, Self.T.Count, 'Count after Add(TIdSdpTime)');
  finally
    Time.Free;
  end;
end;

procedure TestTIdSdpTimes.TestAssign;
var
  Other: TIdSdpTimes;
begin
  Self.T.Add.StartTime := 1;

  Other := TIdSdpTimes.Create;
  try
    Other.Add.StartTime := 4;
    Other.Add.StartTime := 5;
    Other.Add.StartTime := 6;

    Self.T.Assign(Other);

    CheckEquals(3, Self.T.Count, 'Not all times copied across');
    CheckEquals(IntToHex(4, Sizeof(Int64)*2),
                IntToHex(Self.T[0].StartTime, Sizeof(Int64)*2),
                'First time');
    CheckEquals(IntToHex(5, Sizeof(Int64)*2),
                IntToHex(Self.T[1].StartTime,
                Sizeof(Int64)*2), 'Second time');
    CheckEquals(IntToHex(6, Sizeof(Int64)*2),
                IntToHex(Self.T[2].StartTime,
                Sizeof(Int64)*2), 'Third time');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpTimes.TestClear;
begin
  Self.T.Add;
  Self.T.Add;
  Self.T.Add;

  Self.T.Clear;
  CheckEquals(0, Self.T.Count, 'Count after clear');
end;

procedure TestTIdSdpTimes.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.T.Add;
    Self.T.Add;
    Self.T[0].StartTime := $cafebabe;
    Self.T[0].EndTime   := $deadbeef;
    Self.T[1].StartTime := 1000000000;
    Self.T[1].EndTime   := 1000000001;

    Self.T.PrintOn(S);

    CheckEquals('t=3405691582 3735928559'#13#10
              + 't=1000000000 1000000001'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpZoneAdjustments                                                  *
//******************************************************************************
//* TestTIdSdpZoneAdjustments Public methods ***********************************

procedure TestTIdSdpZoneAdjustments.SetUp;
begin
  inherited SetUp;

  Self.Z := TIdSdpZoneAdjustments.Create;
end;

procedure TestTIdSdpZoneAdjustments.TearDown;
begin
  Self.Z.Free;

  inherited TearDown;
end;

//* TestTIdSdpZoneAdjustments Published methods ********************************

procedure TestTIdSdpZoneAdjustments.TestAddAndCount;
var
  Zone: TIdSdpZoneAdjustment;
begin
  CheckEquals(0, Self.Z.Count, 'Count on new list');
  Self.Z.Add;
  CheckEquals(1, Self.Z.Count, 'Count after Add');

  Zone := TIdSdpZoneAdjustment.Create;
  try
    Self.Z.Add(Zone);
    CheckEquals(2, Self.Z.Count, 'Count after Add(TIdSdpZoneAdjustment)');
  finally
    Zone.Free;
  end;
end;

procedure TestTIdSdpZoneAdjustments.TestAssign;
var
  Other: TIdSdpZoneAdjustments;
begin
  Self.Z.Add.Value := '1';

  Other := TIdSdpZoneAdjustments.Create;
  try
    Other.Add.Value := '4';
    Other.Add.Value := '5';
    Other.Add.Value := '6';

    Self.Z.Assign(Other);

    CheckEquals(3,   Self.Z.Count,    'Not all zone adjustments copied across');
    CheckEquals('4', Self.Z[0].Value, 'First zone adjustment');
    CheckEquals('5', Self.Z[1].Value, 'Second zone adjustment');
    CheckEquals('6', Self.Z[2].Value, 'Third zone adjustment');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpZoneAdjustments.TestClear;
begin
  Self.Z.Add;
  Self.Z.Add;
  Self.Z.Add;

  Self.Z.Clear;
  CheckEquals(0, Self.Z.Count, 'Count after clear');
end;

procedure TestTIdSdpZoneAdjustments.TestPrintOn;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Z.Add.Value := '3405691582 -2s';
    Self.Z.Add.Value := '3735928559 5d';

    Self.Z.PrintOn(S);

    CheckEquals('z=3405691582 -2s'#13#10
              + 'z=3735928559 5d'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpPayload                                                          *
//******************************************************************************
//* TestTIdSdpPayload Public methods *******************************************

procedure TestTIdSdpPayload.SetUp;
begin
  inherited SetUp;

  Self.Payload := TIdSdpPayload.Create;
end;

procedure TestTIdSdpPayload.TearDown;
begin
  Self.Payload.Free;

  inherited TearDown;
end;

//* TestTIdSdpPayload Private methods ******************************************

procedure TestTIdSdpPayload.SetToMinimumPayload(P: TIdSdpPayload);
var
  Parser: TIdSdpParser;
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload);
  try
    Parser := TIdSdpParser.Create;
    try
      Parser.Source := S;
      Parser.Parse(P);
    finally
      Parser.Free;
    end;
  finally
    S.Free;
  end;
end;

//* TestTIdSdpPayload Published methods ****************************************

procedure TestTIdSdpPayload.TestAddConnection;
var
  I: Integer;
begin
  Self.Payload.AddMediaDescription;
  Self.Payload.AddMediaDescription;

  Self.Payload.AddConnection;

  for I := 0 to Self.Payload.MediaDescriptionCount - 1 do
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(I).Connections.Count,
                'Media description(' + IntToStr(I)
              + ') didn''t get a connection');
end;

procedure TestTIdSdpPayload.TestAsString;
begin
  Self.SetToMinimumPayload(Self.Payload);

  CheckEquals(MinimumPayload, Self.Payload.AsString, 'AsString');
end;

procedure TestTIdSdpPayload.TestConnectionAt;
var
  I: Integer;
begin
  CheckNull(Self.Payload.ConnectionAt(-1),
            '-1 should never return a meaningful connection');
  CheckNull(Self.Payload.ConnectionAt(0),
            'First connection of empty set');

  Self.Payload.AddConnection.TTL := 0;
  Self.Payload.AddConnection.TTL := 1;

  for I := 0 to 1 do
    CheckEquals(I,
                Self.Payload.ConnectionAt(I).TTL,
                'TTL on ' + IntToStr(I) + 'th connection');

  CheckNull(Self.Payload.ConnectionAt(2),
            'Out-of-bounds index');
end;

procedure TestTIdSdpPayload.TestConnectionCount;
var
  I: Integer;
begin
  for I := 0 to 9 do begin
    CheckEquals(I,
                Self.Payload.ConnectionCount,
                'ConnectionCount ' + IntToStr(I));
    Self.Payload.AddConnection;
  end;
end;

procedure TestTIdSdpPayload.TestCreateFromStream;
var
  Dest:    TStringStream;
  Payload: TIdSdpPayload;
  Src:     TStringStream;
begin
  Src := TStringStream.Create(MinimumPayload);
  try
    Payload := TIdSdpPayload.CreateFrom(Src);
    try
      Dest := TStringStream.Create('');
      try
        Payload.PrintOn(Dest);

        CheckEquals(MinimumPayload, Dest.DataString, 'ReadFrom');
      finally
        Dest.Free;
      end;
    finally
      Payload.Free;
    end;
  finally
    Src.Free;
  end;
end;

procedure TestTIdSdpPayload.TestCreateFromStreamString;
var
  Dest:    TStringStream;
  Payload: TIdSdpPayload;
begin
  Payload := TIdSdpPayload.CreateFrom(MinimumPayload);
  try
    Dest := TStringStream.Create('');
    try
      Payload.PrintOn(Dest);

      CheckEquals(MinimumPayload, Dest.DataString, 'ReadFrom');
    finally
      Dest.Free;
    end;
  finally
    Payload.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEquals;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Check(Self.Payload.Equals(Other), 'Empty payloads, Payload = Other');
    Check(Other.Equals(Self.Payload), 'Empty payloads, Other = Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentAttributes;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Other.Attributes.Add;

    Check(not Self.Payload.Equals(Other), 'Different attributes, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different attributes, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentBandwidths;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Other.Bandwidths.Add;

    Check(not Self.Payload.Equals(Other), 'Different Bandwidths, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Bandwidths, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentEmailAddress;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.EmailAddress.Address := 'foo@bar';
    Other.EmailAddress.Address := Self.Payload.EmailAddress.Address + '1';

    Check(not Self.Payload.Equals(Other), 'Different EmailAddress, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different EmailAddress, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentInfo;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.Info := 'foo@bar';
    Other.Info := Self.Payload.Info + '1';

    Check(not Self.Payload.Equals(Other), 'Different Info, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Info, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentKey;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.Key.Value := 'foo@bar';
    Other.Key.Value := Self.Payload.Key.Value + '1';

    Check(not Self.Payload.Equals(Other), 'Different Key, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Key, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentOrigin;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.Origin.Address := 'foo@bar';
    Other.Origin.Address := Self.Payload.Origin.Address + '1';

    Check(not Self.Payload.Equals(Other), 'Different Origin, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Origin, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentPhoneNumber;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.PhoneNumber := 'foo@bar';
    Other.PhoneNumber := Self.Payload.PhoneNumber + '1';

    Check(not Self.Payload.Equals(Other), 'Different PhoneNumber, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different PhoneNumber, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentRTPMapAttributes;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Other.RTPMapAttributes.Add;

    Check(not Self.Payload.Equals(Other), 'Different RTPMapAttributes, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different RTPMapAttributes, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentSessionName;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.SessionName := 'foo@bar';
    Other.SessionName := Self.Payload.SessionName + '1';

    Check(not Self.Payload.Equals(Other), 'Different SessionName, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different SessionName, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentTimes;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Other.Times.Add;

    Check(not Self.Payload.Equals(Other), 'Different Times, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Times, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentUri;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.Uri := 'foo@bar';
    Other.Uri := Self.Payload.Uri + '1';

    Check(not Self.Payload.Equals(Other), 'Different Uri, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Uri, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestEqualsDifferentVersion;
var
  Other: TIdSdpPayload;
begin
  Other := TIdSdpPayload.Create;
  try
    Self.Payload.Version := 1;
    Other.Version := Self.Payload.Version + 1;

    Check(not Self.Payload.Equals(Other), 'Different Version, Payload <> Other');
    Check(not Other.Equals(Self.Payload), 'Different Version, Other <> Payload');
  finally
    Other.Free;
  end;
end;

procedure TestTIdSdpPayload.TestGetRtpMapAttributes;
var
  Attributes: TIdSdpRTPMapAttributes;
  P:          TIdSdpParser;
  S:          TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'a=rtpmap:96 t140/8000'#13#10
                          + 'a=fmtp:96 98/98'#13#10
                          + 'm=text 11000 RTP/AVP 98'#13#10
                          + 'a=rtpmap:98 t140/1000'#13#10
                          + 'm=text 12000 RTP/AVP 97 100'#13#10
                          + 'a=rtpmap:97 t140/1000'#13#10
                          + 'a=rtpmap:100 red/1000'#13#10
                          + 'a=fmtp:100 98/98'#13#10);
  try
    P := TIdSdpParser.Create;
    try
      P.Source := S;
      P.Parse(Self.Payload);

      Attributes := TIdSdpRTPMapAttributes.Create;
      try
        Self.Payload.GetRtpMapAttributes(Attributes);
        CheckEquals(4, Attributes.Count, 'Number of attributes');
        CheckEquals('96 t140/8000', Attributes[0].Value, '1st attribute value');
        CheckEquals('98 t140/1000', Attributes[1].Value, '2nd attribute value');
        CheckEquals('97 t140/1000', Attributes[2].Value, '3rd attribute value');
        CheckEquals('100 red/1000', Attributes[3].Value, '4th attribute value');
      finally
        Attributes.Free;
      end;
    finally
      P.Free
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestInitializeOnEmptySdpPayload;
var
  PT:      TIdRTPPayloadType;
  Profile: TIdRTPProfile;
begin
  Profile := TIdRTPProfile.Create;
  try
    Self.Payload.InitializeProfile(Profile);

    for PT := Low(TIdRTPPayloadType) to High(TIdRTPPayloadType) do
      CheckEquals(TIdNullPayload.ClassName,
                  Profile.EncodingFor(PT).ClassName,
                  'Encoding for payload type ' + IntToStr(PT));
  finally
    Profile.Free;
  end;
end;

procedure TestTIdSdpPayload.TestInitializeOnSingleMediaSdp;
var
  MD:      TIdSdpMediaDescription;
  Profile: TIdRTPProfile;
  PT:      TIdRTPPayloadType;
begin
  MD := Self.Payload.AddMediaDescription;
  MD.AddAttribute(RTPMapAttribute, '98 T140/1000');

  Profile := TIdRTPProfile.Create;
  try
    Self.Payload.InitializeProfile(Profile);

    for PT := Low(TIdRTPPayloadType) to 97 do
      CheckEquals(TIdNullPayload.ClassName,
                  Profile.EncodingFor(PT).ClassName,
                  'Encoding for payload type ' + IntToStr(PT));

    CheckNotEquals(TIdNullPayload.ClassName,
                   Profile.EncodingFor(98).ClassName,
                   'Encoding for payload type 98');

    for PT := 99 to High(TIdRTPPayloadType) do
      CheckEquals(TIdNullPayload.ClassName,
                  Profile.EncodingFor(PT).ClassName,
                  'Encoding for payload type ' + IntToStr(PT));
  finally
    Profile.Free;
  end;
end;

procedure TestTIdSdpPayload.TestMediaDescriptionAt;
var
  I: Integer;
begin
  CheckNull(Self.Payload.MediaDescriptionAt(-1),
            '-1 should never return a meaningful media description');
  CheckNull(Self.Payload.MediaDescriptionAt(0),
            'First media description of empty set');

  Self.Payload.AddMediaDescription.Port := 0;
  Self.Payload.AddMediaDescription.Port := 1;

  for I := 0 to 1 do
    CheckEquals(I,
                Self.Payload.MediaDescriptionAt(I).Port,
                'Port on ' + IntToStr(I) + 'th media description');

  CheckNull(Self.Payload.MediaDescriptionAt(2),
            'Out-of-bounds index');
end;

procedure TestTIdSdpPayload.TestMediaDescriptionCount;
var
  I: Integer;
begin
  for I := 0 to 9 do begin
    CheckEquals(I,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount ' + IntToStr(I));
    Self.Payload.AddMediaDescription;
  end;
end;

procedure TestTIdSdpPayload.TestMediaDescriptionGetsSessionConnections;
begin
  Self.Payload.AddConnection;
  Self.Payload.AddMediaDescription;

  CheckEquals(1,
              Self.Payload.MediaDescriptionAt(0).Connections.Count,
              'Media description didn''t "inherit" session connection');
end;

procedure TestTIdSdpPayload.TestMediaDescriptionGetsSetupTypeAttribute;
const
  ST = stActPass;
begin
  Self.Payload.Attributes.SetupType := ST;

  Self.Payload.AddMediaDescription;

  CheckEquals(SetupTypeToStr(ST),
              SetupTypeToStr(Self.Payload.MediaDescriptionAt(0).Attributes.SetupType),
              'Media description didn''t "inherit" session setup attribute');
end;

procedure TestTIdSdpPayload.TestMimeType;
begin
  CheckEquals(SdpMimeType, Self.Payload.MimeType, 'SDP MIME type');
end;

procedure TestTIdSdpPayload.TestNewSessionConnectionGetsCopiedToMediaDescriptions;
var
  MD: TIdSdpMediaDescription;
begin
  MD := Self.Payload.AddMediaDescription;
  Self.Payload.AddConnection;

  CheckEquals(1,
              MD.Connections.Count,
              'No connection added to an existing media description');
end;

procedure TestTIdSdpPayload.TestNoSessionNamePrintsDash;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.Payload.PrintOn(S);

    Check(Pos('s=-', S.DataString) > 0,
          '''s=-'' not present in payload');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnBasic;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);

    Self.Payload.PrintOn(S);

    CheckEquals(MinimumPayload, S.DataString, 'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithAttributes;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.Attributes.Add('dead:beef');

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 'a=dead:beef'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithBandwidth;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.Bandwidths.Add;
    Self.Payload.Bandwidths[0].Bandwidth := 13;
    Self.Payload.Bandwidths[0].BandwidthType := btApplicationSpecific;

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 'b=AS:13'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithEmail;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.EmailAddress.Address := 'azathoth@centre.of.chaos.net';

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'e=azathoth@centre.of.chaos.net'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithInfo;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.Info := 'Like a murder of ravens in fugue';

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'i=Like a murder of ravens in fugue'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithKey;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.Key.KeyType := ktBase64;
    Self.Payload.Key.Value   := '5ideofbeef';

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 'k=base64:5ideofbeef'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithMediaDescriptions;
var
  MD: TIdSdpMediaDescription;
  S:  TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    MD := Self.Payload.AddMediaDescription;
    MD.AddFormat('98');
    MD.AddAttribute(RTPMapAttribute, '98 t140/1000');

    MD.MediaType := mtText;
    MD.Protocol  := Id_SDP_RTPAVP;
    MD.Port      := 8000;

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'm=text 8000 RTP/AVP 98'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 'a=rtpmap:98 t140/1000'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithPhoneNumber;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.PhoneNumber := '+44 404 0000';

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'p=+44 404 0000'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithTimes;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.Times.Add;
    Self.Payload.Times[0].StartTime := 1000000000;
    Self.Payload.Times[0].EndTime   := 1000000001;

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10
              + 't=1000000000 1000000001'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestPrintOnWithUri;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.SetToMinimumPayload(Self.Payload);
    Self.Payload.URI := 'mailto:nyarlathotep@crawling.chaos.net';

    Self.Payload.PrintOn(S);

    CheckEquals('v=0'#13#10
              + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
              + 's=Minimum Session Info'#13#10
              + 'u=mailto:nyarlathotep@crawling.chaos.net'#13#10
              + 'c=IN IP4 224.2.17.12/127'#13#10,
                S.DataString,
                'PrintOn');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpPayload.TestReadFromString;
var
  Other: TIdSdpPayload;
  Src:   TStringStream;
begin
  Src := TStringStream.Create(MinimumPayload);
  try
    Other := TIdSdpPayload.CreateFrom(Src);
    try
      Self.Payload.ReadFrom(Src.DataString);
      Check(Self.Payload.Equals(Other),
            'ReadFrom(String) gives different result to ReadFrom(Stream)');
    finally
      Other.Free;
    end;
  finally
    Src.Free;
  end;
end;

procedure TestTIdSdpPayload.TestReadFromStringEmpty;
begin
  try
    Self.Payload.ReadFrom('');
    Fail('Failed to bail out on empty input stream');
  except
    on EParserError do;
  end;
end;

procedure TestTIdSdpPayload.TestRemoveLastMediaDescription;
var
  ExpectedFormat: String;
  OldStreamCount: Integer;
begin
  Self.Payload.ReadFrom('v=0'#13#10
                      + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                      + 's=Minimum Session Info'#13#10
                      + 'c=IN IP4 5.6.7.8'#13#10
                      + 'm=audio 8000 RTP/AVP 0'#13#10
                      + 'm=audio 8002 RTP/AVP 1'#13#10);

  ExpectedFormat := Self.Payload.MediaDescriptionAt(0).Formats[0];
  OldStreamCount := Self.Payload.MediaDescriptionCount;

  Self.Payload.RemoveLastMediaDescription;

  CheckEquals(OldStreamCount - 1, Self.Payload.MediaDescriptionCount,            'No stream removed');
  CheckEquals(ExpectedFormat,     Self.Payload.MediaDescriptionAt(0).Formats[0], 'Wrong stream removed');

  Self.Payload.RemoveLastMediaDescription;
  CheckEquals(OldStreamCount - 2, Self.Payload.MediaDescriptionCount, 'No stream removed (again)');


  Self.Payload.RemoveLastMediaDescription;
  CheckEquals(0, Self.Payload.MediaDescriptionCount, 'Can''t remove streams from a description with no streams');
end;

procedure TestTIdSdpPayload.TestSetConnectionAddress;
const
  Address = '1.2.3.4';
var
  I, J: Integer;
begin
  Self.Payload.ReadFrom('v=0'#13#10
                      + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                      + 's=Minimum Session Info'#13#10
                      + 'c=IN IP4 127.0.0.1'#13#10
                      + 'c=IN IP4 5.6.7.8'#13#10
                      + 'm=audio 8000 RTP/AVP 0'#13#10
                      + 'c=IN IP4 127.0.0.1'#13#10
                      + 'c=IN IP4 5.6.7.8'#13#10
                      + 'm=audio 8002 RTP/AVP 0'#13#10);

  Self.Payload.SetConnectionAddress(Address);

  for I := 0 to Self.Payload.ConnectionCount - 1 do
    CheckEquals(Address, Self.Payload.ConnectionAt(I).Address, Format('Connection #%d', [I]));

  for I := 0 to Self.Payload.MediaDescriptionCount - 1 do begin
    for J := 0 to Self.Payload.MediaDescriptionAt(I).Connections.Count - 1 do begin
      CheckEquals(Address, Self.Payload.MediaDescriptionAt(I).Connections[J].Address,
                  Format('Connection #%d of media description #%d', [J, I]));
    end;
  end;
end;

//******************************************************************************
//* TestTIdSdpParser                                                           *
//******************************************************************************
//* TestTIdSdpParser Public methods ********************************************

procedure TestTIdSdpParser.SetUp;
begin
  inherited SetUp;

  Self.P       := TIdSdpParser.Create;
  Self.Payload := TIdSdpPayload.Create;
end;

procedure TestTIdSdpParser.TearDown;
begin
  Self.Payload.Free;
  Self.P.Free;

  inherited TearDown;
end;

//* TestTIdSdpParser Private methods *******************************************

procedure TestTIdSdpParser.CheckMalformedAttribute(const Value: String);
begin
  Self.CheckMalformedOptionalSessionHeader(RSSDPAttributeName, Value);
end;

procedure TestTIdSdpParser.CheckMalformedConnection(const Value: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'c=' + Value);
  try
    try
      Self.P.Source := S;
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [RSSDPConnectionName, 'c=' + Value]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedOrigin(const OriginValue: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=' + OriginValue + #13#10
                          + 's=Minimum Session Info');
  try
    try
      Self.P.Source := S;
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [RSSDPOriginName, 'o=' + OriginValue]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedPhoneNumber(const Value: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'p=' + Value);
  try
    try
      Self.P.Source := S;
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [RSSDPPhoneName, 'p=' + Value]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedKey(const KeyValue: String);
begin
  Self.CheckMalformedOptionalSessionHeader(RSSDPKeyName, KeyValue);
end;

procedure TestTIdSdpParser.CheckMalformedMediaDescription(const Value: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=' + Value + #13#10
                          + 'i=Information');
  try
    Self.P.Source := S;

    try
    Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [RSSDPMediaDescriptionName, 'm=' + Value]),
                    E.Message,
                    'Unexpected exception');
    end;

  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedOptionalSessionHeader(const Name, Value: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + Name + '=' + Value + #13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [Name, Name + '=' + Value]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.CheckMalformedTimeWithRepeat(const RepeatValue: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'r=' + RepeatValue);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [RSSDPRepeatName, 'r=' + RepeatValue]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

//* TestTIdSdpParser Published methods *****************************************

procedure TestTIdSdpParser.TestIsAddressType;
begin
  Check(not TIdSdpParser.IsAddressType(''), '''''');
  Check(not TIdSdpParser.IsAddressType('ip4'), 'ip4');
  Check(not TIdSdpParser.IsAddressType(' IP4'), ' IP4');
  Check(    TIdSdpParser.IsAddressType('IP4'), 'IP4');
  Check(    TIdSdpParser.IsAddressType(Id_SDP_IP4), 'Id_SDP_IP4 constant');
  Check(    TIdSdpParser.IsAddressType('IP4'), 'IP6');
  Check(    TIdSdpParser.IsAddressType(Id_SDP_IP6), 'Id_SDP_IP6 constant');
end;

procedure TestTIdSdpParser.TestIsBandwidthType;
begin
  Check(not TIdSdpParser.IsBandwidthType(''),                         '''''');
  Check(    TIdSdpParser.IsBandwidthType('ct'),                       'ct');
  Check(    TIdSdpParser.IsBandwidthType('CT'),                       'CT');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_ConferenceTotal),     'Id_SDP_ConferenceTotal constant');
  Check(    TIdSdpParser.IsBandwidthType('AS'),                       'AS');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_ApplicationSpecific), 'Id_SDP_ApplicationSpecific constant');
  Check(    TIdSdpParser.IsBandwidthType('RS'),                       'RS');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_RS),                  'Id_SDP_RS constant');
  Check(    TIdSdpParser.IsBandwidthType('RR'),                       'RR');
  Check(    TIdSdpParser.IsBandwidthType(Id_SDP_RR),                  'Id_SDP_RR constant');
  Check(    TIdSdpParser.IsBandwidthType('1Unknown2BWtype'),          '1Unknown2BWtype');
end;

procedure TestTIdSdpParser.TestIsByteString;
var
  I: Integer;
  S: String;
begin
  Check(not TIdSdpParser.IsByteString(''),  '''''');
  Check(not TIdSdpParser.IsByteString(#13), #13);
  Check(not TIdSdpParser.IsByteString(#10), #10);
  Check(not TIdSdpParser.IsByteString(#0),  #0);

  Check(TIdSdpParser.IsByteString('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'),
        'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789');

  for I := 1 to 9 do
    Check(TIdSdpParser.IsByteString(Chr(I)), '#' + IntToHex(I, 2));

  Check(TIdSdpParser.IsByteString(#11), '#11');
  Check(TIdSdpParser.IsByteString(#12), '#12');

  for I := 14 to 255 do
    Check(TIdSdpParser.IsByteString(Chr(I)), '#' + IntToHex(I, 2));

  S := '';
  for I := 1 to 1000000 do
    S := S + 'a';
  Check(TIdSdpParser.IsByteString(S), '1 million a''s');
end;

procedure TestTIdSdpParser.TestIsDirection;
begin
  Check(TIdSdpParser.IsDirection('inactive'), 'inactive');
  Check(TIdSdpParser.IsDirection('recvonly'), 'recvonly');
  Check(TIdSdpParser.IsDirection('sendonly'), 'sendonly');
  Check(TIdSdpParser.IsDirection('sendrecv'), 'sendrecv');

  Check(not TIdSdpParser.IsDirection(''),    'Empty string');
  Check(not TIdSdpParser.IsDirection('foo'), 'foo');
end;

procedure TestTIdSdpParser.TestIsKeyData;
begin
  Check(not TIdSdpParser.IsKeyData(''), '''''');
  Check(not TIdSdpParser.IsKeyData('abc'#0), 'abc#0');
  Check(    TIdSdpParser.IsKeyData(' '), 'SP');
  Check(    TIdSdpParser.IsKeyData('~'), '~');
  Check(    TIdSdpParser.IsKeyData('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 '#9'''"-./:?"#$&*;=@[]^_`{|}+~'),
            '''abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 '#9'''"-./:?"#$&*;=@[]^_`{|}+~');
end;

procedure TestTIdSdpParser.TestIsKeyType;
begin
  Check(not TIdSdpParser.IsKeyType(''),            '''''');

  Check(TIdSdpParser.IsKeyType('clear'),       'clear');
  Check(TIdSdpParser.IsKeyType(Id_SDP_Clear),  'Id_SDP_Clear constant');
  Check(TIdSdpParser.IsKeyType('base64'),      'base64');
  Check(TIdSdpParser.IsKeyType(Id_SDP_Base64), 'Id_SDP_Base64 constant');
  Check(TIdSdpParser.IsKeyType('uri'),         'uri');
  Check(TIdSdpParser.IsKeyType(Id_SDP_URI),    'Id_SDP_URI constant');
  Check(TIdSdpParser.IsKeyType('prompt'),      'prompt');
  Check(TIdSdpParser.IsKeyType(Id_SDP_Prompt), 'Id_SDP_Prompt constant');

  Check(TIdSdpParser.IsKeyType(AllTokenChars), AllTokenChars);
end;

procedure TestTIdSdpParser.TestIsMediaType;
begin
  Check(not TIdSdpParser.IsMediaType(''), '''''');

  Check(TIdSdpParser.IsMediaType('audio'),                   'audio');
  Check(TIdSdpParser.IsMediaType(RSSDPMediaTypeAudio),       'RSSDPMediaTypeAudio constant');
  Check(TIdSdpParser.IsMediaType('video'),                   'video');
  Check(TIdSdpParser.IsMediaType(RSSDPMediaTypeVideo),       'RSSDPMediaTypeVideo constant');
  Check(TIdSdpParser.IsMediaType('application'),             'application');
  Check(TIdSdpParser.IsMediaType(RSSDPMediaTypeApplication), 'RSSDPMediaTypeApplication constant');
  Check(TIdSdpParser.IsMediaType('data'),                    'data');
  Check(TIdSdpParser.IsMediaType(RSSDPMediaTypeData),        'RSSDPMediaTypeData constant');
  Check(TIdSdpParser.IsMediaType('control'),                 'control');
  Check(TIdSdpParser.IsMediaType(RSSDPMediaTypeControl),     'RSSDPMediaTypeControl constant');

  Check(TIdSdpParser.IsMediaType(AllTokenChars), AllTokenChars);
end;

procedure TestTIdSdpParser.TestIsMimeType;
begin
  Check(not TIdSdpParser.IsMimeType(''), '''''');
  Check(TIdSdpParser.IsMimeType('text/plain'), 'text/plain');
  Check(TIdSdpParser.IsMimeType('text/plain;charset=UTF-8'), 'text/plain;charset=UTF-8');
  Check(TIdSdpParser.IsMimeType('x-y/x-y'), 'x-y/x-y');
  Check(TIdSdpParser.IsMimeType('x-''123''/x-#~%$&*!_+|{}^'), 'x-''123''/x-#~%$&*!_+|{}^');
  Check(TIdSdpParser.IsMimeType('-/-'), '-/-');
  Check(TIdSdpParser.IsMimeType('x-/x-'), 'x-/x-');

  Check(TIdSdpParser.IsMimeType('text/plain;charset=UTF-8'), 'text/plain;charset=UTF-8');

//  Check(not TIdSdpParser.IsMimeType('text/plain;charset'), 'text/plain;charset - parameters MUST have a value');
end;

procedure TestTIdSdpParser.TestIsMulticastAddress;
var
  I: Integer;
begin
  // ipv4
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, ''),                'IPv4: ''''');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '1'),               'IPv4: 1');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, 'abcd'),            'IPv4: abcd');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.'),            'IPv4: 224.');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '127.2.17.12'),     'IPv4: 127.2.17.12');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.2.17.12'),     'IPv4: 224.2.17.12');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.0.0.0'),       'IPv4: 224.0.0.0');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.255.255.255'), 'IPv4: 224.255.255.255');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv4, '224.255.255.256'), 'IPv4: 224.255.255.256');

  // 224.0.0.0 to 239.255.255.255 are all valid multicast addresses
  for I := 224 to 239 do
    Check(TIdSdpParser.IsMulticastAddress(Id_IPv4, IntToStr(I) + '.255.255.255'),
          'IPv4: ' + IntToStr(I) + '.255.255.255');

  // ipv6
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, ''), 'IPv6: ''''');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, '1'),                          'IPv6: 1');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, '224.'),                       'IPv6: 224.');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, '224.2.17.12'),                'IPv6: 224.2.17.12');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, '2002:c058:6301::'),           'IPv6: 2002:c058:6301::');
  Check(not TIdSdpParser.IsMulticastAddress(Id_IPv6, '1080:0:0:0:8:800:200C:417A'), 'IPv6: 1080:0:0:0:8:800:200C:417A');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv6, 'FF02:c058:6301::'),           'IPv6: FF02:c058:6301::');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv6, 'Ff00:0:0:0:0:0:0:101'),       'IPv6: Ff00:0:0:0:0:0:0:101');
  Check(    TIdSdpParser.IsMulticastAddress(Id_IPv6, 'ff01:0:0:0:0:0:0:101'),       'IPv6: ff01:0:0:0:0:0:0:101');
end;

procedure TestTIdSdpParser.TestIsNetType;
begin
  Check(not TIdSdpParser.IsNetType(''),        '''''');
  Check(not TIdSdpParser.IsNetType('in'),      'in');
  Check(    TIdSdpParser.IsNetType('IN'),      'IN');
  Check(    TIdSdpParser.IsNetType(Id_SDP_IN), 'Id_SDP_IN constant');

end;

procedure TestTIdSdpParser.TestIsPhone;
begin
  Check(not TIdSdpParser.IsPhone(''),                   '''''');
  Check(not TIdSdpParser.IsPhone('+-1'),                '+-1');
  Check(not TIdSdpParser.IsPhone('+1'),                 '+1');
  Check(    TIdSdpParser.IsPhone('+1 '),                '+1 ');
  Check(    TIdSdpParser.IsPhone('+1-'),                '+1-');
  Check(    TIdSdpParser.IsPhone('+1-2-3'),             '+1-2-3');
  Check(    TIdSdpParser.IsPhone('+1-2 3 '),            '+1-2 3 SP');
  Check(    TIdSdpParser.IsPhone('+44-870  154 0 154'), '+44-870  154 0 154');
end;

procedure TestTIdSdpParser.TestIsPhoneNumber;
begin
  Check(not TIdSdpParser.IsPhoneNumber(''),                   '''''');
  Check(not TIdSdpParser.IsPhoneNumber('+-1'),                '+-1');
  Check(not TIdSdpParser.IsPhoneNumber('+1'),                 '+1');
  Check(    TIdSdpParser.IsPhoneNumber('+1 '),                '+1 SP');
  Check(    TIdSdpParser.IsPhoneNumber('+1-'),                '+1-');
  Check(    TIdSdpParser.IsPhoneNumber('+1-2-3'),             '+1-2-3');
  Check(    TIdSdpParser.IsPhoneNumber('+1-2 3 '),            '+1-2 3 SP');
  Check(    TIdSdpParser.IsPhoneNumber('+44-870  154 0 154'), '+44-870  154 0 154');

  Check(TIdSdpParser.IsPhoneNumber('"Quoted name" <+44 870 154 0 154>'),
        '"Quoted name" <+44 870 154 0 154>');
  Check(TIdSdpParser.IsPhoneNumber('aheh "Quoted name" <+44 870 154 0 154>'),
        '"aheh Quoted name" <+44 870 154 0 154>');
  Check(TIdSdpParser.IsPhoneNumber('+44 870 154 0 154'),
        '+44 870 154 0 154');
  Check(TIdSdpParser.IsPhoneNumber('+44 870 154 0 154 (Quoted name)'),
        '+44 870 154 0 154 (Quoted name)');
  Check(not TIdSdpParser.IsPhoneNumber('+44 870 154 0 154 (Quoted name) fink'),
        '+44 870 154 0 154 (Quoted name) fink');
end;

procedure TestTIdSdpParser.TestIsPort;
begin
  Check(not TIdSdpParser.IsPort(''), '''''');
  Check(not TIdSdpParser.IsPort('a'), 'a');
  Check(not TIdSdpParser.IsPort('1a'), '1a');
  Check(not TIdSdpParser.IsPort(#0), '#0');
  Check(not TIdSdpParser.IsPort('1 '), '1 SP');
  Check(not TIdSdpParser.IsPort(''), '65536');
  Check(not TIdSdpParser.IsPort('1'#0), '1#0');
  Check(    TIdSdpParser.IsPort('1'), '1');
  Check(    TIdSdpParser.IsPort('65535'), '65535');
  Check(    TIdSdpParser.IsPort('666'), '666');
end;

procedure TestTIdSdpParser.TestIsProtocol;
begin
  Check(not TIdSdpParser.IsProtocol(''),            '''''');
  Check(not TIdSdpParser.IsProtocol('/'),           '/');
  Check(not TIdSdpParser.IsProtocol('/AVP'),        '/AVP');
  Check(not TIdSdpParser.IsProtocol('RTP/'),        'RTP/');
  Check(not TIdSdpParser.IsProtocol('RTP//AVP'),    'RTP//AVP');
  Check(    TIdSdpParser.IsProtocol('vAt'),         'vAt');
  Check(    TIdSdpParser.IsProtocol('rapunzel'),    'rapunzel');
  Check(    TIdSdpParser.IsProtocol('RTP/AVP'),     'RTP/AVP');
  Check(    TIdSdpParser.IsProtocol(Id_SDP_RTPAVP), 'Id_SDP_RTPAVP constant');
  Check(    TIdSdpParser.IsProtocol('vat'),         'vat');
  Check(    TIdSdpParser.IsProtocol('rtp'),         'rtp');
  Check(    TIdSdpParser.IsProtocol('UDPTL'),       'UDPTL');
  Check(    TIdSdpParser.IsProtocol('TCP'),         'TCP');
  Check(    TIdSdpParser.IsProtocol(Id_SDP_TCP),    'Id_SDP_TCP constant');
  Check(    TIdSdpParser.IsProtocol('1/2/3/4'),     '1/2/3/4');
end;

procedure TestTIdSdpParser.TestIsText;
var
  C: Char;
begin
  Check(not TIdSdpParser.IsText(''),         '''''');
  Check(    TIdSdpParser.IsText('abc'),      'abc');
  Check(not TIdSdpParser.IsText('hello'#0),  'hello#0');
  Check(not TIdSdpParser.IsText('hello'#10), 'hello#10');
  Check(not TIdSdpParser.IsText('hello'#13), 'hello#13');

  for C := Low(Char) to High(Char) do
    if not (C in [#0, #10, #13]) then
      Check(TIdSdpParser.IsText(C), 'Checking Ord(C) = ' + IntToStr(Ord(C)));
end;

procedure TestTIdSdpParser.TestIsTime;
begin
  Check(not TIdSdpParser.IsTime(''),                     '''''');
  Check(not TIdSdpParser.IsTime('a'),                    'a');
  Check(not TIdSdpParser.IsTime('d'),                    'd');
  Check(not TIdSdpParser.IsTime('99a'),                  '99a');
  Check(not TIdSdpParser.IsTime('1d1'),                  '1d1');
  Check(not TIdSdpParser.IsTime('1dd'),                  '1dd');
  Check(    TIdSdpParser.IsTime('98765432109876543210'), '98765432109876543210');
  Check(    TIdSdpParser.IsTime('1d'),                   '1d');
  Check(    TIdSdpParser.IsTime('1000h'),                '1000h');
  Check(    TIdSdpParser.IsTime('1000m'),                '1000m');
  Check(    TIdSdpParser.IsTime('1000s'),                '1000s');
end;

procedure TestTIdSdpParser.TestIsToken;
var
  C: Integer;
begin
  for C := 0 to 31 do
    Check(not TIdSdpParser.IsToken(Char(C)), 'Control character #$' + IntToHex(C, 2));

  Check(not TIdSdpParser.IsToken(''),                '''''');
  Check(not TIdSdpParser.IsToken('no spaces'),       'no spaces');
  Check(not TIdSdpParser.IsToken('(noparentheses)'), '(noparentheses)');
  Check(not TIdSdpParser.IsToken('no,comma'),        'no,comma');
  Check(not TIdSdpParser.IsToken('no/slash'),        'no/slash');
  Check(not TIdSdpParser.IsToken('no@at'),           'no@at');
  Check(not TIdSdpParser.IsToken('[nobrackets]'),    '[nobrackets]');
  Check(not TIdSdpParser.IsToken('no\backslash'),    'no\backslash');
  Check(not TIdSdpParser.IsToken('nodelete'#$7f),    'nodelete#$7f');

  Check(TIdSdpParser.IsToken(AllTokenChars), AllTokenChars);
end;

procedure TestTIdSdpParser.TestParseAttribute;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'a=recvonly'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,          Self.Payload.Attributes.Count,    'AttributeCount');
    CheckEquals('recvonly', Self.Payload.Attributes[0].Name,  'Attribute.Name');
    CheckEquals('',         Self.Payload.Attributes[0].Value, 'Attribute.Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseAttributeWithValue;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'a=orient:landscape'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,           Self.Payload.Attributes.Count,    'AttributeCount');
    CheckEquals('orient',    Self.Payload.Attributes[0].Name,  'Attribute.Name');
    CheckEquals('landscape', Self.Payload.Attributes[0].Value, 'Attribute.Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseAttributeMalformedName;
begin
  Self.CheckMalformedAttribute('a=real-dash');
end;

procedure TestTIdSdpParser.TestParseAttributeMalformedValue;
begin
  Self.CheckMalformedAttribute('a=bytestring:it is not#0one at all');
end;

procedure TestTIdSdpParser.TestParseBandwidth;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'b=RR:666'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,     Self.Payload.Bandwidths.Count,            'Bandwidths.Count');
    Check      (btRR = Self.Payload.Bandwidths[0].BandwidthType, 'BandwidthType');
    CheckEquals(666,   Self.Payload.Bandwidths[0].Bandwidth,     'Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseBandwidthMalformed;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'b=:'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on a malformed bandwidth');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [RSSDPBandwidthName, 'b=:']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseBandwidthMultipleHeaders;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'b=RR:666'#13#10
                          + 'b=CT:123'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(2,                  Self.Payload.Bandwidths.Count, 'Bandwidths.Count');
    Check      (btRR =              Self.Payload.Bandwidths[0].BandwidthType, '[0].BandwidthType');
    CheckEquals(666,                Self.Payload.Bandwidths[0].Bandwidth,     '[0].Bandwidth');
    Check      (btConferenceTotal = Self.Payload.Bandwidths[1].BandwidthType, '[1].BandwidthType');
    CheckEquals(123,                Self.Payload.Bandwidths[1].Bandwidth,     '[1].Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseBandwidthUnknown;
const
  TIAS = 'TIAS';
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'b=' + TIAS + ':666'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,          Self.Payload.Bandwidths.Count,            'Bandwidths.Count');
    Check      (btUnknown = Self.Payload.Bandwidths[0].BandwidthType, 'BandwidthType');
    CheckEquals(TIAS,       Self.Payload.Bandwidths[0].BandwidthName, 'BandwidthName');
    CheckEquals(666,        Self.Payload.Bandwidths[0].Bandwidth,     'Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionInSessionAndMediaDescription;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.11/127'#13#10); // note the difference compared with the session c.
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('IN',          Self.Payload.ConnectionAt(0).NetType,     'NetType');
    Check      (Id_IPv4 =      Self.Payload.ConnectionAt(0).AddressType, 'AddressType');
    CheckEquals('224.2.17.12', Self.Payload.ConnectionAt(0).Address,     'Address');
    CheckEquals(127,           Self.Payload.ConnectionAt(0).TTL,         'TTL');

    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');
    CheckEquals(2,
                Self.Payload.MediaDescriptionAt(0).Connections.Count,
                'MediaDescriptionAt(0).Connections.Count');
    CheckEquals('IN',
                Self.Payload.MediaDescriptionAt(0).Connections[0].NetType,
                'Connection.NetType');
    Check(Id_IPv4 = Self.Payload.MediaDescriptionAt(0).Connections[0].AddressType,
          'Connection.AddressType');
    CheckEquals('224.2.17.11',
                Self.Payload.MediaDescriptionAt(0).Connections[0].Address,
                'Connection.Address');
    CheckEquals(127,
                Self.Payload.MediaDescriptionAt(0).Connections[0].TTL,
                'Connection.TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionMalformed;
begin
  Self.CheckMalformedConnection('c=IN');
end;

procedure TestTIdSdpParser.TestParseConnectionMalformedMulticastAddress;
begin
  Self.CheckMalformedConnection('c=IN IP4 224.2.17.12/a');
  Self.CheckMalformedConnection('c=IN IP4 224.2.17.12');
  Self.CheckMalformedConnection('c=IN IP4 127.2.17.12/127');
  Self.CheckMalformedConnection('c=IN IP6 224.2.17.12/127');
end;

procedure TestTIdSdpParser.TestParseConnectionMalformedUnicastAddress;
begin
  Self.CheckMalformedConnection('c=IN IP4 127.0.0.1/127');
  Self.CheckMalformedConnection('c=IN IP4 127.0.0.1/127/2');
end;

procedure TestTIdSdpParser.TestParseConnectionMulticastAddress;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'c=IN IP4 224.2.17.12/127'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('IN',          Self.Payload.ConnectionAt(0).NetType,     'NetType');
    Check      (Id_IPv4 =      Self.Payload.ConnectionAt(0).AddressType, 'AddressType');
    CheckEquals('224.2.17.12', Self.Payload.ConnectionAt(0).Address,     'Address');
    CheckEquals(127,           Self.Payload.ConnectionAt(0).TTL,         'TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionMulticastAddressWithMultiple;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'c=IN IP4 224.2.17.12/127/3'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(3,
                Self.Payload.ConnectionCount,
                'Connections.Count');
    CheckEquals('IN',
                Self.Payload.ConnectionAt(0).NetType,
                'NetType');
    Check(Id_IPv4 = Self.Payload.ConnectionAt(0).AddressType,
          'AddressType');
    CheckEquals('224.2.17.12',
                Self.Payload.ConnectionAt(0).Address,
                'Address 1');
    CheckEquals('224.2.17.13',
                Self.Payload.ConnectionAt(1).Address,
                'Address 2');
    CheckEquals('224.2.17.14',
                Self.Payload.ConnectionAt(2).Address,
                'Address 3');
    CheckEquals(127,
                Self.Payload.ConnectionAt(0).TTL,
                'TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionMulticastIPv6AddressWithMultiple;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'c=IN IP6 FF02:5156:4019:2::FFFF/127/3'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(3,
                Self.Payload.ConnectionCount,
                'Connections.Count');
    CheckEquals('IN',
                Self.Payload.ConnectionAt(0).NetType,
                'NetType');
    Check(Id_IPv6 = Self.Payload.ConnectionAt(0).AddressType,
          'AddressType');
    CheckEquals('FF02:5156:4019:2::FFFF',
                Self.Payload.ConnectionAt(0).Address,
                'Address 1');
    CheckEquals('FF02:5156:4019:2::1:0',
                Self.Payload.ConnectionAt(1).Address,
                'Address 2');
    CheckEquals('FF02:5156:4019:2::1:1',
                Self.Payload.ConnectionAt(2).Address,
                'Address 3');
    CheckEquals(127,
                Self.Payload.ConnectionAt(0).TTL,
                'TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseConnectionUnknownAddrType;
begin
  Self.CheckMalformedConnection('c=IN IP5 224.2.17.12/127');
end;

procedure TestTIdSdpParser.TestParseConnectionUnknownNetType;
begin
  Self.CheckMalformedConnection('c=ON IP4 224.2.17.12/127');
end;

procedure TestTIdSdpParser.TestParseConnectionUnicastAddress;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'c=IN IP4 127.2.17.12'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('IN',          Self.Payload.ConnectionAt(0).NetType,     'NetType');
    Check      (Id_IPv4 =      Self.Payload.ConnectionAt(0).AddressType, 'AddressType');
    CheckEquals('127.2.17.12', Self.Payload.ConnectionAt(0).Address,     'Address');
    CheckEquals(0,             Self.Payload.ConnectionAt(0).TTL,         'TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmail;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'e=nihilistic@mystics.net'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('nihilistic@mystics.net', Self.Payload.EmailAddress.Text, 'EmailAddress');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmailBracketedName;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'e=nihilistic@mystics.net (Apostolic alcoholics)'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    // note that "Apostolic alcoholics" is a COMMENT, not a display name.
    CheckEquals('nihilistic@mystics.net', Self.Payload.EmailAddress.Text, 'EmailAddress.Text');
    CheckEquals('',                       Self.Payload.EmailAddress.Name, 'EmailAddress.Name');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmailRfc822;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'e="Apostolic alcoholics" <nihilistic@mystics.net>'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals('Apostolic alcoholics <nihilistic@mystics.net>',
                Self.Payload.EmailAddress.Text,
                'EmailAddress.Text');

    CheckEquals('Apostolic alcoholics',
                Self.Payload.EmailAddress.Name,
                'EmailAddress.Name');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseEmptyStream;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with empty input stream');
    except
      on E: EParserError do
        CheckEquals(EmptyInputStream, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseHeaderMissingName;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + '=Missing name'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(Format(UnknownOptionalHeader, ['=Missing name']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseHeaderMissingValue;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'b='#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, ['b', 'b=']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseHeadersInvalidOrder;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info'#13#10
                          + 'u=http://127.0.0.1/'#13#10
                          + 'i=My u & i headers are swopped round'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out: vosui');
    except
      on E: EParserError do
        CheckEquals(Format(BadHeaderOrder, [RSSDPInformationName, RSSDPUriName]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;

  S := TStringStream.Create(MinimumPayload
                          + 'b=RR:666'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'b=CT:666'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out: vosbcb');
    except
      on E: EParserError do
        CheckEquals(Format(BadHeaderOrder, [RSSDPConnectionName, RSSDPBandwidthName]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseInfo;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'i=An optional header'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('An optional header', Self.Payload.Info, 'Info');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseInfoIllegalCharacters;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=f00'#13#10
                          + 'i=haha'#0'haha');
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on malformed information-field');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [RSSDPInformationName, 'i=haha'#0'haha']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseKey;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'k=uri:sip:wintermute@tessier-ashpool.co.luna'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    Check(ktUri =                                       Self.Payload.Key.KeyType, 'KeyType');
    CheckEquals('sip:wintermute@tessier-ashpool.co.luna', Self.Payload.Key.Value,   'Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseKeyMalformedPrompt;
begin
  Self.CheckMalformedKey('prompt:sip:wintermute@tessier-ashpool.co.luna');
end;

procedure TestTIdSdpParser.TestParseKeyMalformedBase64;
begin
  Self.CheckMalformedKey('base64');
end;

procedure TestTIdSdpParser.TestParseKeyMalformedClear;
begin
  Self.CheckMalformedKey('clear');
end;

procedure TestTIdSdpParser.TestParseKeyMalformedUri;
begin
  Self.CheckMalformedKey('uri');
end;

procedure TestTIdSdpParser.TestParseKeyUnknownKeyType;
const
  UnknownKeyType = 'rot26';
var
  Key: TIdSdpKey;
  S:   TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'k=' + UnknownKeyType + #13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    Key := Self.Payload.Key;

    Check(ktUnknown = Key.KeyType, 'KeyType');
    CheckEquals(UnknownKeyType, Key.KeyName, 'KeyName');
    CheckEquals('', Key.Value, 'Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseKeyUnknownKeyTypeWithValue;
const
  UnknownKeyType = 'rot26';
  Value          = 'foo';
var
  Key: TIdSdpKey;
  S:   TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'k=' + UnknownKeyType + ':' + Value + #13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    Key := Self.Payload.Key;

    Check(ktUnknown = Key.KeyType, 'KeyType');
    CheckEquals(UnknownKeyType, Key.KeyName, 'KeyName');
    CheckEquals(Value, Key.Value, 'Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseLinphoneSessionDescription;
var
  S: TStringStream;
begin
  // Note the space in the bandwidth header. We just rip out the first token
  // after the colon and ignore the rest.
  S := TStringStream.Create('v=0'#13#10
                          + 'o=frank 123456 654321 IN IP4 192.168.0.2'#13#10
                          + 's=A conversation'#13#10
                          + 'c=IN IP4 192.168.0.2'#13#10
                          + 't=0 0'#13#10
                          + 'm=audio 7078 RTP/AVP 110 115 101'#13#10
                          + 'b=AS:110 8'#13#10
                          + 'a=rtpmap:110 speex/8000/1'#13#10
                          + 'a=rtpmap:115 1015/8000/1'#13#10
                          + 'a=rtpmap:101 telephone-event/8000'#13#10
                          + 'a=fmtp:101 0-11'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    Check(Self.Payload.MediaDescriptionCount > 0,
          'Insuffient media descriptions');
    Check(Self.Payload.MediaDescriptionAt(0).Bandwidths.Count > 0,
          'Insuffient bandwidths');
    CheckEquals(110,
                Self.Payload.MediaDescriptionAt(0).Bandwidths[0].Bandwidth,
                'Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescription;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=data 6666 RTP/AVP 1'#13#10
                          + 'i=Information'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');

    Check(mtData = Self.Payload.MediaDescriptionAt(0).MediaType,
          'MediaDescriptionAt(0).MediaType');
    CheckEquals(6666,
                Self.Payload.MediaDescriptionAt(0).Port,
                'MediaDescriptionAt(0).Port');
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(0).PortCount,
                'MediaDescriptionAt(0).PortCount');
    CheckEquals('RTP/AVP',
                Self.Payload.MediaDescriptionAt(0).Protocol,
                'MediaDescriptionAt(0).Protocol');
    CheckEquals('Information',
                Self.Payload.MediaDescriptionAt(0).Info,
                'MediaDescriptionAt(0).Info');
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(0).FormatCount,
                'MediaDescriptionAt(0).FormatCount');
    CheckEquals('1',
                Self.Payload.MediaDescriptionAt(0).Formats[0],
                'MediaDescriptionAt(0).Formats[0]');
    Check(Self.Payload.MediaDescriptionAt(0).HasConnection,
         'MediaDescriptionAt(0).HasConnection');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithSessionAttributes;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'c=IN IP4 127.0.0.1'#13#10
                          + 'a=recvonly'#13#10
                          + 'a=tool:RNID SDP'#13#10
                          + 'm=data 6666 RTP/AVP 1'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals(Self.Payload.Attributes.Count,
                Self.Payload.MediaDescriptionAt(0).Attributes.Count,
                'Media description attribute count');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithSessionConnections;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'c=IN IP4 127.0.0.1'#13#10
                          + 'm=data 6666 RTP/AVP 1'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals(Self.Payload.ConnectionCount,
                Self.Payload.MediaDescriptionAt(0).Connections.Count,
                'Media description connection count');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMalformedFormatList;
begin
  Self.CheckMalformedMediaDescription('data 65536 RTP/AVP 1 2 3 -1');
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionNonRtpFormatList;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=data 65536 TCP text/plain;charset=UTF-8 text/html'#13#10);
  try
    Self.P.Source := S;
    Self.P.Parse(Self.Payload);

    CheckEquals(1, Self.Payload.MediaDescriptionCount, 'Incorrect number of media descriptions');
    CheckEquals(2, Self.Payload.MediaDescriptionAt(0).FormatCount, 'Incorrect number of formats');
    CheckEquals('text/plain;charset=UTF-8', Self.Payload.MediaDescriptionAt(0).Formats[0],  'First format');
    CheckEquals('text/html',                Self.Payload.MediaDescriptionAt(0).Formats[1],  'Second format');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMissingInformation;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10);
  try
    Self.P.Source := S;
    Self.P.Parse(Self.Payload);
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMissingKey;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10);
  try
    Self.P.Source := S;
    Self.P.Parse(Self.Payload);

    Check(Self.Payload.MediaDescriptionCount > 0,
          'No media descriptions');
    Check(not Self.Payload.MediaDescriptionAt(0).HasKey, 'HasKey');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMalformedPort;
var
  S: TStringStream;
begin
  // A port number of 65536 makes no sense for UDP or TCP, but SDP doesn't
  // care - a port consists of a sequence of digits.
  S := TStringStream.Create(MinimumPayload
                          + 'm=data 65536 RTP/AVP 1'#13#10 // Note the port number
                          + 'i=Information'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(65536,
                Self.Payload.MediaDescriptionAt(0).Port,
                'High port (portnum > 65535)');

  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMalformedTransport;
begin
  Self.CheckMalformedMediaDescription('data 6666 RtP//AVP 1');
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMissingFormatList;
begin
  Self.CheckMalformedMediaDescription('data 65535 RTP/AVP');
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionMissingPort;
begin
  Self.CheckMalformedMediaDescription('data RTP/AVP 1');
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionsMissingSessionConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'b=RR:666'#13#10
                          + 'a=recvonly'#13#10
                          + 'a=T38FaxTranscodingJBIG'#13#10
                          + 'a=type:broadcast'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1, Self.Payload.MediaDescriptionCount, 'MediaDescriptionCount');
    Check(Self.Payload.MediaDescriptionAt(0).HasConnection, 'MediaDescriptionAt(0) connection');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptions;
var
  I: Integer;
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'm=video 49170/1 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'm=video 49171/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'm=video 49172/3 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10
                          + 'm=video 49173/4 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.12/127'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(4,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');

    for I := 0 to 3 do begin
      Check(Self.Payload.MediaDescriptionAt(I).HasConnection,
            'MediaDescriptions[' + IntToStr(I) + '] connection');

      Check(mtVideo = Self.Payload.MediaDescriptionAt(I).MediaType,
            'MediaDescriptions[' + IntToStr(I) + '].MediaType');



    CheckEquals(49170 + I,
                Self.Payload.MediaDescriptionAt(I).Port,
                'MediaDescriptions[' + IntToStr(I) + '].Port');
    CheckEquals(1 + I,
                Self.Payload.MediaDescriptionAt(I).PortCount,
                'MediaDescriptions[' + IntToStr(I) + '].PortCount');
    CheckEquals('RTP/AVP',
                Self.Payload.MediaDescriptionAt(I).Protocol,
                'MediaDescriptions[' + IntToStr(I) + '].Protocol');
    CheckEquals('More information than you can shake a stick at',
                Self.Payload.MediaDescriptionAt(I).Info,
                'MediaDescriptions[' + IntToStr(I) + '].Info');
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(I).FormatCount,
                'MediaDescriptions[' + IntToStr(I) + '].FormatCount');
    CheckEquals('31',
                Self.Payload.MediaDescriptionAt(I).Formats[0],
                'MediaDescriptions[' + IntToStr(I) + '].Formats[0]');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionUnknownBandwidthType;
const
  UnknownBandwidthType = 'UnknownBandwidthType';
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=audio 65535 RTP/AVP 0'#13#10
                          + 'i=Information'#13#10
                          + 'b=' + UnknownBandwidthType + ':35200'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    Check(Self.Payload.MediaDescriptionCount > 0, 'No media descriptions');
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(0).Bandwidths.Count,
                'Number of bandwidth attributes');
    CheckEquals(UnknownBandwidthType,
                Self.Payload.MediaDescriptionAt(0).Bandwidths[0].BandwidthName,
                'Bandwidth header not properly parsed');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionUnknownMediaType;
const
  UnknownMediaType = 'unknown-media-type';
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=' + UnknownMediaType + ' 65535 RTP/AVP 0'#13#10
                          + 'i=Information'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    Check(Self.Payload.MediaDescriptionCount > 0, 'Insufficient media descriptions');
    Check(mtUnknown = Self.Payload.MediaDescriptionAt(0).MediaType, 'MediaType');
    CheckEquals(UnknownMediaType, Self.Payload.MediaDescriptionAt(0).MediaName, 'MediaName');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithAttributes;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'b=RR:666'#13#10
                          + 'a=recvonly'#13#10
                          + 'a=T38FaxTranscodingJBIG'#13#10
                          + 'a=type:broadcast'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(0, Self.Payload.Attributes.Count,                       'Session attributes');
    CheckEquals(1, Self.Payload.MediaDescriptionCount,                  'MediaDescriptionCount');
    CheckEquals(3, Self.Payload.MediaDescriptionAt(0).Attributes.Count, 'AttributeCount');

    CheckEquals('recvonly',
                Self.Payload.MediaDescriptionAt(0).Attributes[0].Name,
                'Attributes[0].Name');
    CheckEquals('',
                Self.Payload.MediaDescriptionAt(0).Attributes[0].Value,
                'Attributes[0].Value');
    CheckEquals('T38FaxTranscodingJBIG',
                Self.Payload.MediaDescriptionAt(0).Attributes[1].Name,
                'Attributes[1].Name');
    CheckEquals('',
                Self.Payload.MediaDescriptionAt(0).Attributes[1].Value,
                'Attributes[1].Value');
    CheckEquals('type',
                Self.Payload.MediaDescriptionAt(0).Attributes[2].Name,
                'Attributes[2].Name');
    CheckEquals('broadcast',
                Self.Payload.MediaDescriptionAt(0).Attributes[2].Value,
                'Attributes[2].Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithBandwidth;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'b=RR:666'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(0,
                Self.Payload.Bandwidths.Count,
                'Bandwidths.Count');
    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');
    CheckEquals(1,
                Self.Payload.MediaDescriptionAt(0).Bandwidths.Count,
                'MediaDescriptions.Bandwidths.Count');
    Check(btRR = Self.Payload.MediaDescriptionAt(0).Bandwidths[0].BandwidthType,
                'BandwidthType');
    CheckEquals(666,
                Self.Payload.MediaDescriptionAt(0).Bandwidths[0].Bandwidth,
                'Bandwidth');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.14/127/2'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');
    Check(Self.Payload.MediaDescriptionAt(0).HasConnection,
          'HasConnection');
    CheckEquals(3,
                Self.Payload.MediaDescriptionAt(0).Connections.Count,
                'MediaDescriptionAt(0).Connections.Count');
    CheckEquals('IN',
                Self.Payload.MediaDescriptionAt(0).Connections[1].NetType,
                'Connection[1].NetType');
    Check(Id_IPv4 = Self.Payload.MediaDescriptionAt(0).Connections[1].AddressType,
          'Connection[1].AddressType');
    CheckEquals('224.2.17.14',
                Self.Payload.MediaDescriptionAt(0).Connections[0].Address,
                'Connection[0].Address');
    CheckEquals('224.2.17.15',
                Self.Payload.MediaDescriptionAt(0).Connections[1].Address,
                'Connection[1].Address');
    CheckEquals('224.2.17.12',
                Self.Payload.MediaDescriptionAt(0).Connections[2].Address,
                'Connection[2].Address');
    CheckEquals(127,
                Self.Payload.MediaDescriptionAt(0).Connections[1].TTL,
                'Connection[1].TTL');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithKey;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'k=uri:sip:wintermute@tessier-ashpool.co.luna'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');
    Check(Self.Payload.MediaDescriptionAt(0).HasKey,
          'MediaDescriptions.HasKey');
    Check(ktUri = Self.Payload.MediaDescriptionAt(0).Key.KeyType,
          'KeyType');

    CheckEquals('sip:wintermute@tessier-ashpool.co.luna',
                Self.Payload.MediaDescriptionAt(0).Key.Value,
                'Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithMultipleFormats;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 31 23'#13#10
                          + 'i=Information'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,
                Self.Payload.MediaDescriptionCount,
                'MediaDescriptionCount');
    CheckEquals(2,
                Self.Payload.MediaDescriptionAt(0).FormatCount,
                'MediaDescriptionAt(0).FormatCount');
    CheckEquals('31',
                Self.Payload.MediaDescriptionAt(0).Formats[0],
                'MediaDescriptionAt(0).Formats[0]');
    CheckEquals('23',
                Self.Payload.MediaDescriptionAt(0).Formats[1],
                'MediaDescriptionAt(0).Formats[1]');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMediaDescriptionWithPortCount;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'm=video 49170/2 RTP/AVP 2 2'#13#10
                          + 'i=Information'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1, Self.Payload.MediaDescriptionCount, 'MediaDescriptionCount');

    Check(mtVideo = Self.Payload.MediaDescriptionAt(0).MediaType,
                'MediaDescriptionAt(0).MediaType');
    CheckEquals(49170,
                Self.Payload.MediaDescriptionAt(0).Port,
                'MediaDescriptionAt(0).Port');
    CheckEquals(2,
                Self.Payload.MediaDescriptionAt(0).PortCount,
                'MediaDescriptionAt(0).PortCount');
    CheckEquals('RTP/AVP',
                Self.Payload.MediaDescriptionAt(0).Protocol,
                'MediaDescriptionAt(0).Protocol');
    CheckEquals('Information',
                Self.Payload.MediaDescriptionAt(0).Info,
                'MediaDescriptionAt(0).Info');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMinimumPayload;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(0,                      Self.Payload.Version,     'Version');
    CheckEquals('Minimum Session Info', Self.Payload.SessionName, 'SessionName');

    CheckEquals('mhandley',    Self.Payload.Origin.Username,       'Origin.Username');
    CheckEquals('2890844526',  Self.Payload.Origin.SessionID,      'Origin.SessionID');
    CheckEquals('2890842807',  Self.Payload.Origin.SessionVersion, 'Origin.SessionVersion');
    CheckEquals('IN',          Self.Payload.Origin.NetType,        'Origin.NetType');
    Check      (Id_IPV4 =      Self.Payload.Origin.AddressType,    'Origin.AddressType');
    CheckEquals('126.16.64.4', Self.Payload.Origin.Address,        'Origin.Address');

    CheckEquals('IN',          Self.Payload.ConnectionAt(0).NetType,     'Connection.NetType');
    Check      (Id_IPv4      = Self.Payload.ConnectionAt(0).AddressType, 'Connection.AddressType');
    CheckEquals('224.2.17.12', Self.Payload.ConnectionAt(0).Address,     'Connection.Address');
    CheckEquals(127,           Self.Payload.ConnectionAt(0).TTL,         'Connection.TTL');

    CheckEquals('', Self.Payload.Info, 'Info');
    CheckEquals(0, Self.Payload.Times.Count, 'Times.Count');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingOrigin;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=1'#13#10
                          + 's=Missing Origin. Like We Bad Syntax'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing origin-field');
    except
      on E: EParserError do
        CheckEquals(MissingOrigin, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingSession;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=1'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 'i=Session Information'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing session-name-field');
    except
      on E: EParserError do
        CheckEquals(MissingSessionName, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingSessionConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(MissingConnection,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseMissingVersion;
var
  S: TStringStream;
begin
  S := TStringStream.Create('o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with missing proto-version');
    except
      on E: EParserError do
        CheckEquals(MissingVersion, E.Message, 'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseOriginIPv6Address;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP6 2002:5156:4019:1::1'#13#10
                          + 's=Minimum Session Info'#13#10
                          + 'c=IN IP6 2002:5156:4019:1::1'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('2002:5156:4019:1::1',
                Self.Payload.Origin.Address,
                'Address');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseOriginMalformed;
begin
  Self.CheckMalformedOrigin('mhandley');
end;

procedure TestTIdSdpParser.TestParseOriginMalformedUsername;
begin
  Self.CheckMalformedOrigin('mhand ley 2890844526 2890842807 IN IP4 126.16.64.4');
end;

procedure TestTIdSdpParser.TestParseOriginMalformedSessionID;
begin
  Self.CheckMalformedOrigin('mhandley 28a90844526 2890842807 IN IP4 126.16.64.4');
end;

procedure TestTIdSdpParser.TestParseOriginMalformedSessionVersion;
begin
  Self.CheckMalformedOrigin('mhandley 2890844526 28a90842807 IN IP4 126.16.64.4');
end;

procedure TestTIdSdpParser.TestParseOriginMalformedNetType;
begin
  Self.CheckMalformedOrigin('mhandley 2890844526 2890842807 in IP4 126.16.64.4');
end;

procedure TestTIdSdpParser.TestParsePhoneNumber;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'p=Ziggy Stardust <+99 666 0942-3>'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('Ziggy Stardust <+99 666 0942-3>',
                Self.Payload.PhoneNumber,
                'PhoneNumber');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParsePhoneNumberMultipleHeaders;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'p=Ziggy Stardust <+99 666 0942-3>'#13#10
                          + 'p=Ziggy Stardust <+99 666 0942-3>'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with multiple Phone headers');
    except
      on E: EParserError do
        CheckEquals(Format(TooManyHeaders, [RSSDPPhoneName]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParsePhoneNumberWithAngleBracketsButNoName;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'p=<+99 666 0942-3>'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('<+99 666 0942-3>',
                Self.Payload.PhoneNumber,
                'PhoneNumber');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParsePhoneNumberWithStuffOutsideComment;
begin
  Self.CheckMalformedPhoneNumber('+99 666 0942-3 (Ziggy Stardust) oh no! it''s garbage!');
end;

procedure TestTIdSdpParser.TestParsePhoneNumberWithUnsafeChars;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'p=+99 666 0942-3'#0#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [RSSDPPhoneName, 'p=+99 666 0942-3'#0]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseRedefinedPayloadType;
var
  Profile: TIdRTPProfile;
  S:       TStringStream;
begin
  // RFC 3388 says
  //   An SDP session description typically contains one or more media lines
  //   - they are commonly known as "m" lines.  When a session description
  //   contains more than one "m" line, SDP does not provide any means to
  //   express a particular relationship between two or more of them.  When
  //   an application receives an SDP session description with more than one
  //   "m" line, it is up to the application what to do with them.  SDP does
  //   not carry any information about grouping media streams.
  //
  // We assume therefore that the application (us) can treat the SDP below
  // as we see fit. Note that the payload type is being defined and then
  // redefined. We see several interpretations - (a) format type 98
  // gets its first definition overwritten and becomes a T140/1000, or (b)
  // each media description uses a separate RTP profile (and hence needs a
  // separate RTP server), or (c) ignore the second definition. We choose
  // interpretation (c) because it seems simplest.
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + DefaultConnection
                          + 'm=audio 4000 RTP/AVP 98'#13#10
                          + 'a=rtpmap:98 pcm/8000'#13#10
                          + 'm=text 8000 RTP/AVP 98'#13#10
                          + 'a=rtpmap:98 T140/1000'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);

    Profile := TIdRTPProfile.Create;
    try
      Self.Payload.InitializeProfile(Profile);
      CheckEquals('pcm',
                  Profile.EncodingFor(98).Name,
                  'Format type wasn''t redefined');
    finally
      Profile.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseSessionIllegalCharacters;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=0'#13#10
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's='#0#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out on Session with #0');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [RSSDPSessionName, 's='#0]),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseSomeMediaDescriptionsLackConnectionAndNoSessionConnection;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'm=video 49170/2 RTP/AVP 31'#13#10
                          + 'i=More information than you can shake a stick at'#13#10
                          + 'c=IN IP4 224.2.17.11/127'#13#10
                          + 'm=audio 5060/2 RTP/AVP 31'#13#10
                          + 'i=Oh no, we have no media-level connection!'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(MissingConnection,
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeMultipleHeaders;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 't=0 0'#13#10
                          + 't=3034423619 0'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(3,            Self.Payload.Times.Count,                    'Times.Count');
    CheckEquals('3042462419', IntToStr(Self.Payload.Times[0].EndTime),     'Times[0].EndTime');
    CheckEquals('3034423619', IntToStr(Self.Payload.Times[0].StartTime),   'Times[0].StartTime');
    CheckEquals(0,            Self.Payload.Times[0].Repeats.Count,         'Times[0].Repeats.Count');
    CheckEquals(0,            Self.Payload.Times[0].ZoneAdjustments.Count, 'Times[0].ZoneAdjustments.Count');
    CheckEquals('0',          IntToStr(Self.Payload.Times[1].EndTime),     'Times[1].EndTime');
    CheckEquals('0',          IntToStr(Self.Payload.Times[1].StartTime),   'Times[1].StartTime');
    CheckEquals(0,            Self.Payload.Times[1].Repeats.Count,         'Times[1].Repeats.Count');
    CheckEquals(0,            Self.Payload.Times[1].ZoneAdjustments.Count, 'Times[1].ZoneAdjustments.Count');
    CheckEquals('0',          IntToStr(Self.Payload.Times[2].EndTime),     'Times[2].EndTime');
    CheckEquals('3034423619', IntToStr(Self.Payload.Times[2].StartTime),   'Times[2].StartTime');
    CheckEquals(0,            Self.Payload.Times[2].Repeats.Count,         'Times[2].Repeats.Count');
    CheckEquals(0,            Self.Payload.Times[2].ZoneAdjustments.Count, 'Times[2].ZoneAdjustments.Count');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeSingleBounded;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,            Self.Payload.Times.Count,                   'Times.Count');
    // Yes, it looks lame, but CheckEquals takes two integers and the compiler
    // sees that 3042462419 is too big for an integer. This way we still get
    // to test, and see <expected> <actual> values instead of just a pass/fail
    CheckEquals('3042462419', IntToStr(Self.Payload.Times[0].EndTime),   'Times[0].EndTime');
    CheckEquals('3034423619', IntToStr(Self.Payload.Times[0].StartTime), 'Times[0].StartTime');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeSingleUnbounded;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 't=0 0'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1, Self.Payload.Times.Count,        'Times.Count');
    CheckEquals(0, Self.Payload.Times[0].EndTime,   'Times[0].EndTime');
    CheckEquals(0, Self.Payload.Times[0].StartTime, 'Times[0].StartTime');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeSingleUnboundedEndTime;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 0'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,            Self.Payload.Times.Count,                  'Times.Count');
    CheckEquals('0',          IntToStr(Self.Payload.Times[0].EndTime),   'Times[0].EndTime');
    CheckEquals('3034423619', IntToStr(Self.Payload.Times[0].StartTime), 'Times[0].StartTime');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeWithMalformedMultipleRepeat;
begin
  Self.CheckMalformedTimeWithRepeat('1 1 1 1 1x');
end;

procedure TestTIdSdpParser.TestParseTimeWithMalformedSingleRepeat;
begin
  Self.CheckMalformedTimeWithRepeat('1x');
end;

procedure TestTIdSdpParser.TestParseTimeWithRepeatAndZoneAdjustment;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'z=1 -2 3 4'#13#10
                          + 'r=1'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,   Self.Payload.Times.Count,                    'Times.Count');
    CheckEquals(1,   Self.Payload.Times[0].Repeats.Count,         'Times[0].Repeats.Count');
    CheckEquals(1,   Self.Payload.Times[0].ZoneAdjustments.Count, 'Times[0].ZoneAdjustments.Count');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeWithRepeatBeforeZoneAdjustment;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'r=1'#13#10
                          + 'z=1 -2 3 4'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out');
    except
      on E: EParserError do
        CheckEquals(Format(UnknownOptionalHeader, ['z=1 -2 3 4']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeWithSingleRepeat;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'r=1'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,   Self.Payload.Times.Count,              'Times.Count');
    CheckEquals(1,   Self.Payload.Times[0].Repeats.Count,   'Times[0].Repeats.Count');
    CheckEquals('1', Self.Payload.Times[0].Repeats[0].Value, 'Repeats[0].Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeWithSingleTypedRepeat;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'r=1d'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,    Self.Payload.Times.Count,               'Times.Count');
    CheckEquals(1,    Self.Payload.Times[0].Repeats.Count,    'Times[0].Repeats.Count');
    CheckEquals('1d', Self.Payload.Times[0].Repeats[0].Value, 'Repeats[0].Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseTimeWithSingleZoneAdjustment;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 't=3034423619 3042462419'#13#10
                          + 'z=0 -1'#13#10);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals(1,      Self.Payload.Times.Count,                       'Times.Count');
    CheckEquals(0,      Self.Payload.Times[0].Repeats.Count,            'Times[0].Repeats.Count');
    CheckEquals(1,      Self.Payload.Times[0].ZoneAdjustments.Count,    'Times[0].ZoneAdjustments.Count');
    CheckEquals('0 -1', Self.Payload.Times[0].ZoneAdjustments[0].Value, 'ZoneAdjustments[0].Value');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseUri;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayloadSansConnection
                          + 'u=http://127.0.0.1/'#13#10
                          + DefaultConnection);
  try
    Self.P.Source := S;

    Self.P.Parse(Self.Payload);
    CheckEquals('http://127.0.0.1/', Self.Payload.URI, 'URI');
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseVersionMalformed;
var
  S: TStringStream;
begin
  S := TStringStream.Create('v=a'#13#10 // v must be a number
                          + 'o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4'#13#10
                          + 's=Minimum Session Info'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out malformed version');
    except
      on E: EParserError do
        CheckEquals(Format(MalformedToken, [RSSDPVersionName, 'v=a']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpParser.TestParseVersionMultipleHeaders;
var
  S: TStringStream;
begin
  S := TStringStream.Create(MinimumPayload
                          + 'v=1'#13#10);
  try
    Self.P.Source := S;

    try
      Self.P.Parse(Self.Payload);
      Fail('Failed to bail out with a duplicate version header');
    except
      on E: EParserError do
        CheckEquals(Format(UnknownOptionalHeader, ['v=1']),
                    E.Message,
                    'Unexpected exception');
    end;
  finally
    S.Free;
  end;
end;

//******************************************************************************
//* TIdSdpTestCase                                                             *
//******************************************************************************
//* TIdSdpTestCase Public methods **********************************************

procedure TIdSdpTestCase.SetUp;
begin
  inherited SetUp;

  Self.ReceivedData := false;
end;

procedure TIdSdpTestCase.CheckPortActive(Address: String;
                                         Port: Cardinal;
                                         Msg: String);
var
  Server: TIdBaseRTPAbstractPeer;
begin
  Server := TIdRTPPeerRegistry.ServerOn(Address, Port);

  if Assigned(Server) then begin
    Check(Server.Active, Msg);
  end
  else
    Fail(Msg);
end;

procedure TIdSdpTestCase.CheckPortFree(Address: String;
                                       Port: Cardinal;
                                       Msg: String);
var
  Server: TIdBaseRTPAbstractPeer;
begin
  Server := TIdRTPPeerRegistry.ServerOn(Address, Port);

  if Assigned(Server) then begin
    Check(not Server.Active, Msg);
  end;
end;

//* TIdSdpTestCase Protected methods *******************************************

procedure TIdSdpTestCase.CheckDataSent(Msg: String);
begin
  Check(Self.SentData, Msg);
end;

procedure TIdSdpTestCase.CheckNoDataSent(Msg: String);
begin
  Check(not Self.SentData, Msg);
end;

procedure TIdSdpTestCase.OnData(Stream: TIdSdpBaseMediaStream;
                                Chunk: TStream;
                                Format: String;
                                Binding: TIdConnectionBindings);
begin
  Self.ReceivedData := true;
end;

procedure TIdSdpTestCase.OnSentData(Stream: TIdSdpBaseMediaStream;
                                    Chunk: TStream;
                                    Format: String;
                                    LayerID: Integer);
begin
  Self.SentData := true;
end;

//******************************************************************************
//* TestTIdSdpBaseMediaStream                                                  *
//******************************************************************************
//* TestTIdSdpBaseMediaStream Public methods ***********************************

procedure TestTIdSdpBaseMediaStream.SetUp;
begin
  inherited SetUp;

  Self.Desc := TIdSdpPayload.CreateFrom('v=0'#13#10
                                      + 'o=foo 1 2 IN IP4 127.0.0.1'#13#10
                                      + 's=-'#13#10
                                      + 'c=IN IP4 127.0.0.1'#13#10
                                      + Self.BasicMediaDesc(8000)
                                      + Self.BasicMediaDesc(9000));
  Self.SendingBinding := TIdConnectionBindings.Create;
  Self.Timer := TIdDebugTimerQueue.Create(false);
end;

procedure TestTIdSdpBaseMediaStream.TearDown;
begin
  Self.Timer.Terminate;
  Self.SendingBinding.Free;
  Self.Desc.Free;

  inherited TearDown;
end;

//* TestTIdSdpBaseMediaStream Protected methods ********************************

procedure TestTIdSdpBaseMediaStream.AddSendingChecking(Stream: TIdSdpBaseMediaStream);
begin
  // Subclasses put code here such that they can check that Stream actually
  // sends data, for instance by adding Listeners to the Stream.
  Stream.AddDataSendListener(Self);
end;

procedure TestTIdSdpBaseMediaStream.Activate(Stream: TIdSdpBaseMediaStream);
begin
  Stream.StartListening;
end;

function TestTIdSdpBaseMediaStream.BasicMediaDesc(Port: Cardinal = 8000; PortCount: Cardinal = 1): String;
begin
  Result := '';
  Fail(Self.ClassName + ' MUST override BasicMediaDesc');
end;

procedure TestTIdSdpBaseMediaStream.CheckPortAdjusted(Expected, Received: Cardinal; IsNullStream: Boolean; StreamClass: String);
begin
  if IsNullStream then
    CheckEquals(0, Received,
                StreamClass + ': Stream not refused')
  else
    CheckEquals(Expected, Received,
                StreamClass + ': Port not reset to lower limit');
end;

function TestTIdSdpBaseMediaStream.CreateStream: TIdSdpBaseMediaStream;
begin
  Result := nil;
  Fail(Self.ClassName + ' MUST override CreateStream');
end;

function TestTIdSdpBaseMediaStream.InactiveMediaDesc: String;
begin
  Result := Self.BasicMediaDesc
          + 'a=' + RSSDPDirectionInactive + #13#10;
end;

function TestTIdSdpBaseMediaStream.LocalDescription: TIdSdpMediaDescription;
begin
  CheckEquals(2, Self.Desc.MediaDescriptionCount,
              Self.ClassName + 'LocalDescription: Someone''s damaged Self.Desc: it needs two media descriptions');

  Result := Self.Desc.MediaDescriptionAt(0);
end;

procedure TestTIdSdpBaseMediaStream.ReceiveDataOn(S: TIdSdpBaseMediaStream);
begin
  Fail(Self.ClassName + ' MUST override ReceiveDataOn');
end;

function TestTIdSdpBaseMediaStream.RefusedMediaDesc: String;
begin
  Result := Self.BasicMediaDesc(RefusedPort);
end;

function TestTIdSdpBaseMediaStream.RemoteDescription: TIdSdpMediaDescription;
begin
  CheckEquals(2, Self.Desc.MediaDescriptionCount,
              Self.ClassName + 'RemoteDescription: Someone''s damaged Self.Desc: it needs two media descriptions');

  Result := Self.Desc.MediaDescriptionAt(1);
end;

procedure TestTIdSdpBaseMediaStream.RemoveSendingChecking(Stream: TIdSdpBaseMediaStream);
begin
  // Subclasses put code here to clean up any resources allocated in
  // AddSendingChecking.
  Stream.RemoveDataSendListener(Self);
end;

procedure TestTIdSdpBaseMediaStream.SendData(Stream: TIdSdpBaseMediaStream);
begin
  Fail(Self.ClassName + ' MUST override SendData');
end;

procedure TestTIdSdpBaseMediaStream.SetLocalMediaDesc(Stream: TIdSdpBaseMediaStream;
                                                      const MediaDesc: String);
var
  SDP: TIdSdpPayload;
begin
  SDP := TIdSdpPayload.CreateFrom(MinimumPayloadWithConnection
                                + MediaDesc);
  try
    Stream.LocalDescription := SDP.MediaDescriptionAt(0);
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.SetRemoteMediaDesc(Stream: TIdSdpBaseMediaStream;
                                                       const MediaDesc: String);
var
  SDP: TIdSdpPayload;
begin
  SDP := TIdSdpPayload.CreateFrom(MinimumPayloadWithConnection
                                + MediaDesc);
  try
    Stream.RemoteDescription := SDP.MediaDescriptionAt(0);
  finally
    SDP.Free;
  end;
end;

//* TestTIdSdpBaseMediaStream Published methods ********************************

procedure TestTIdSdpBaseMediaStream.TestAddDataListener;
var
  L1:     TIdSdpTestMediaListener;
  L2:     TIdSdpTestMediaListener;
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Self.Activate(Stream);
    L1 := TIdSdpTestMediaListener.Create;
    try
      L2 := TIdSdpTestMediaListener.Create;
      try
        Stream.AddDataListener(L1);
        Stream.AddDataListener(L2);

        Self.ReceiveDataOn(Stream);

        // Only non-null streams can receive data
        CheckEquals(not Stream.IsNull, L1.ReceivedData, Self.ClassName + ': L1 notified?');
        CheckEquals(not Stream.IsNull, L2.ReceivedData, Self.ClassName + ': L2 notified?');
      finally
        L2.Free;
      end;
    finally
      L1.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestAddDataSendListener;
var
  L1:     TIdSdpTestMediaListener;
  L2:     TIdSdpTestMediaListener;
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    L1 := TIdSdpTestMediaListener.Create;
    try
      L2 := TIdSdpTestMediaListener.Create;
      try
        Stream.AddDataSendListener(L1);
        Stream.AddDataSendListener(L2);

        Self.SendData(Stream);

        // Only non-null streams can send data
        CheckEquals(not Stream.IsNull, L1.SentData, Self.ClassName + ': L1 notified?');
        CheckEquals(not Stream.IsNull, L2.SentData, Self.ClassName + ': L2 notified?');
      finally
        L2.Free;
      end;
    finally
      L1.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestIsReceiver;
var
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc);
    Check(Stream.IsReceiver,
          'Not Receiver by default');

    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc
                         + 'a=recvonly');
    Check(Stream.IsReceiver,
          'Not Receiver when recvonly');

    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc
                         + 'a=sendrecv');
    Check(Stream.IsReceiver,
          'Not Receiver when sendrecv');

    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc
                         + 'a=sendonly');
    Check(not Stream.IsReceiver,
          'Receiver when sendonly');

    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc
                         + 'a=inactive');
    Check(not Stream.IsReceiver,
          'Receiver when inactive');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestIsSender;
var
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc);
    Check(Stream.IsSender,
          'Not Sender by default');

    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc
                         + 'a=sendrecv');
    Check(Stream.IsSender,
          'Not Sender when sendrecv');

    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc
                         + 'a=sendonly');
    Check(Stream.IsSender,
          'Not Sender when sendonly');

    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc
                         + 'a=recvonly');
    Check(not Stream.IsSender,
          'Sender when recvonly');

    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc
                         + 'a=inactive');
    Check(not Stream.IsSender,
          'Sender when inactive');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestPutOnHoldRecvOnly;
var
  LocalMediaType: TIdSdpMediaType;
  Stream:         TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Check(not Stream.OnHold,
          'OnHold set before PutOnHold');

    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc
                         + 'a=recvonly');
    LocalMediaType := Stream.LocalDescription.MediaType;
    Stream.PutOnHold;

    CheckEquals(DirectionToStr(sdInactive),
                DirectionToStr(Stream.Direction),
                'Stream not put on hold');
    Check(Stream.OnHold,
          'OnHold not set');
    CheckEquals(MediaTypeToStr(LocalMediaType),
                MediaTypeToStr(Stream.LocalDescription.MediaType),
          'Stream''s MediaType changed');
  finally
    Stream.Free
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestPutOnHoldSendRecv;
var
  LocalMediaType: TIdSdpMediaType;
  Stream:         TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Check(not Stream.OnHold,
          'OnHold set before PutOnHold');

    Self.SetLocalMediaDesc(Stream,
                           'm=text 8000 RTP/AVP 96'#13#10
                         + 'a=rtpmap:96 t140/1000'#13#10
                         + 'a=sendrecv');
    LocalMediaType := Stream.LocalDescription.MediaType;
    Stream.PutOnHold;

    CheckEquals(DirectionToStr(sdSendOnly),
                DirectionToStr(Stream.Direction),
                'Stream not put on hold');
    Check(Stream.OnHold,
          'OnHold not set');
    CheckEquals(MediaTypeToStr(LocalMediaType),
                MediaTypeToStr(Stream.LocalDescription.MediaType),
          'Stream''s MediaType changed');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestPutOnHoldWhileOnHold;
var
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Stream.PutOnHold;
    Stream.PutOnHold;
    Check(Stream.OnHold,
          'OnHold not set');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestSendData;
var
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Self.AddSendingChecking(Stream);
    try
      Self.SendData(Stream);

      CheckDataSent(Stream.ClassName + ': No data sent');
    finally
      Self.RemoveSendingChecking(Stream);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestSendDataWhenNotSender;
var
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Self.AddSendingChecking(Stream);
    try
      Self.AddSendingChecking(Stream);
      Self.SetLocalMediaDesc(Stream,
                             Self.InactiveMediaDesc);

      Self.SendData(Stream);

      CheckNoDataSent('Data sent');
    finally
      Self.RemoveSendingChecking(Stream);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestSetTimer;
begin
  // Setting the Timer property must set the Timer property of each Connection.
  Fail(Self.ClassName + ' MUST override TestSetTimer');
end;

procedure TestTIdSdpBaseMediaStream.TestStartListeningPortAboveAllowedRange;
const
  LowerLimit  = 15000;
  HigherLimit = 16000;
  TooHighPort = HigherLimit + 1000;
var
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Stream.LowestAllowedPort  := LowerLimit;
    Stream.HighestAllowedPort := HigherLimit;

    Stream.IsOffer := true;
    Self.SetLocalMediaDesc(Stream, Self.BasicMediaDesc(TooHighPort));
    CheckPortFree(Stream.LocalDescription.Connections[0].Address, Stream.LocalDescription.Port, 'Required port is not free');

    Stream.StartListening;

    CheckPortAdjusted(LowerLimit, Stream.LocalDescription.Port, Stream.IsNull, Stream.ClassName);
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestStartListeningPortBelowAllowedRange;
const
  LowerLimit  = 15000;
  HigherLimit = 16000;
  TooLowPort  = LowerLimit - 1000;
var
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Stream.LowestAllowedPort  := LowerLimit;
    Stream.HighestAllowedPort := HigherLimit;

    Self.SetLocalMediaDesc(Stream, Self.BasicMediaDesc(TooLowPort));
    CheckPortFree(Stream.LocalDescription.Connections[0].Address, Stream.LocalDescription.Port, 'Required port is not free');

    Stream.StartListening;

    CheckPortAdjusted(LowerLimit, Stream.LocalDescription.Port, Stream.IsNull, Stream.ClassName);
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestStartListeningRefusedStream;
var
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Self.SetLocalMediaDesc(Stream,
                           Self.RefusedMediaDesc);

    Stream.StartListening;
    Check(not Stream.IsListening, 'Stream listening, but has been refused');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestTakeOffHold;
var
  PreHoldDirection: TIdSdpDirection;
  Stream:           TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    PreHoldDirection := sdRecvOnly;
    Self.SetLocalMediaDesc(Stream,
                           Self.BasicMediaDesc
                         + 'a=' + DirectionToStr(PreHoldDirection));
    Stream.PutOnHold;
    Stream.TakeOffHold;

    CheckEquals(DirectionToStr(PreHoldDirection),
                DirectionToStr(Stream.Direction),
                'Stream not taken off hold');
    Check(not Stream.OnHold,
          'OnHold not unset');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpBaseMediaStream.TestUnusedPortsSwitchOff;
begin
  Check(true, 'By default do nothing, but override me if the media stream has a way of being refused.');
end;

//******************************************************************************
//* TestTIdSDPMediaStream                                                      *
//******************************************************************************
//* TestTIdSDPMediaStream Public methods ***************************************

procedure TestTIdSDPMediaStream.SetUp;
var
  T140: TIdRTPT140Payload;
begin
  // This is a dynamically assigned payload type in RTP/AVP.
  Self.DataFormat := '96';

  inherited SetUp;

  Self.AVP := TIdRTPProfile.Create;
  Self.AVP.AddEncoding(T140EncodingName, T140ClockRate, '', Self.T140PT);

  Self.Media  := Self.CreateStream as TIdSDPMediaStream;
  Self.Sender := Self.CreateStream as TIdSDPMediaStream;

  Self.Media.LocalDescription  := Self.LocalDescription;
  Self.Media.RemoteDescription := Self.RemoteDescription;

  Self.Sender.LocalDescription  := Self.Media.RemoteDescription;
  Self.Sender.RemoteDescription := Self.Media.LocalDescription;

  Self.Text := TStringStream.Create('');

  T140 := TIdRTPT140Payload.Create;
  try
    T140.Block := '1234';
    T140.PrintOn(Self.Text);
    Self.Text.Seek(0, soFromBeginning);
  finally
    T140.Free;
  end;

  Self.Media.StartListening;
  Self.Sender.StartListening;

  Self.SentBye      := false;
  Self.SentControl  := false;
  Self.SentData     := false;
end;

procedure TestTIdSDPMediaStream.TearDown;
begin
  Self.Text.Free;
  Self.Sender.Free;
  Self.Media.Free;
  Self.AVP.Free;

  inherited TearDown;
end;

//* TestTIdSDPMediaStream Protected methods ************************************

function TestTIdSDPMediaStream.BasicMediaDesc(Port: Cardinal = 8000; PortCount: Cardinal = 1): String;
begin
  if (PortCount > 1) then
    Result := Format('m=text %d/%d RTP/AVP %d'#13#10
                   + 'a=rtpmap:%d T140/8000'#13#10,
                     [Port, PortCount, Self.T140PT, Self.T140PT])
  else
    Result := Format('m=text %d RTP/AVP %d'#13#10
                   + 'a=rtpmap:%d T140/8000'#13#10,
                     [Port, Self.T140PT, Self.T140PT]);
end;

function TestTIdSDPMediaStream.CreateStream: TIdSdpBaseMediaStream;
var
  S: TIdSDPMediaStream;
begin
  S := TIdSDPMediaStream.Create(TIdMockRTPPeer);
  S.LocalDescription  := Self.LocalDescription;
  S.LocalProfile      := Self.AVP;
  S.RemoteDescription := Self.RemoteDescription;
  S.RemoteProfile     := Self.AVP;
  S.Timer             := Self.Timer;

  Result := S;
end;

procedure TestTIdSDPMediaStream.ReceiveDataOn(S: TIdSdpBaseMediaStream);
var
  Binding: TIdConnectionBindings;
  Host:    String;
  Port:    Cardinal;
  RTP:     TIdRTPPacket;
  Server:  TIdBaseRTPAbstractPeer;
  Text:    TIdRTPT140Payload;
begin
  Binding := TIdConnectionBindings.Create;
  try
    Host := S.LocalDescription.Connections[0].Address;
    Port := S.LocalDescription.Port;

    Server := TIdRTPPeerRegistry.ServerOn(Host, Port);

    Check(Assigned(Server), 'RTP peer not found running on ' + Host + ':' + IntToStr(Port));

    Binding.PeerIP   := Host;
    Binding.PeerPort := Port;

    RTP := TIdRTPPacket.Create(Server.LocalProfile);
    try
      Text := TIdRTPT140Payload.Create;
      try
        Text.Block := '1234';
        RTP.Payload := Text;

        Server.ReceivePacket(RTP, Binding);
      finally
        Text.Free;
      end;
    finally
      RTP.Free;
    end;
  finally
    Binding.Free;
  end;
end;

procedure TestTIdSDPMediaStream.SendData(Stream: TIdSdpBaseMediaStream);
begin
  Stream.SendData(Self.Text, IntToStr(Self.T140PT));
  Self.Timer.TriggerAllEventsUpToFirst(TIdRTPSendDataWait);
end;

//* TestTIdSDPMediaStream Private methods **************************************

procedure TestTIdSDPMediaStream.OnSendRTCP(Packet: TIdRTCPPacket;
                                           Binding: TIdConnectionBindings);
begin
  Self.SendingBinding.LocalIP   := Binding.LocalIP;
  Self.SendingBinding.LocalPort := Binding.LocalPort;
  Self.SendingBinding.PeerIP    := Binding.PeerIP;
  Self.SendingBinding.PeerPort  := Binding.PeerPort;

  Self.SentBye := Packet.IsBye;
  Self.SentControl := true;
end;

procedure TestTIdSDPMediaStream.OnSendRTP(Packet: TIdRTPPacket;
                                          Binding: TIdConnectionBindings);
begin
  Self.SendingBinding.LocalIP   := Binding.LocalIP;
  Self.SendingBinding.LocalPort := Binding.LocalPort;
  Self.SendingBinding.PeerIP    := Binding.PeerIP;
  Self.SendingBinding.PeerPort  := Binding.PeerPort;

  Self.SentData := true;
end;

procedure TestTIdSDPMediaStream.ReceiveControlOn(S: TIdSDPMediaStream);
var
  Binding: TIdConnectionBindings;
  RTCP:    TIdRTCPPacket;
  Server:  TIdBaseRTPAbstractPeer;
begin
  Binding := TIdConnectionBindings.Create;
  try
    Binding.LocalIP   := S.LocalDescription.Connections[0].Address;
    Binding.LocalPort := S.LocalDescription.Port;
    Binding.PeerIP    := Self.Sender.LocalDescription.Connections[0].Address;
    Binding.PeerPort  := Self.Sender.LocalDescription.Port;

    Server := TIdRTPPeerRegistry.ServerOn(Binding.LocalIP, Binding.LocalPort);

    Check(Assigned(Server), 'RTP peer not found running on ' + Binding.LocalIP + ':' + IntToStr(Binding.LocalPort));

    RTCP := TIdRTCPSenderReport.Create;
    try
      RTCP.SyncSrcID := TIdRTPPeerRegistry.ServerOn(Binding.PeerIP, Binding.PeerPort).Session.SyncSrcID;

      Server.ReceivePacket(RTCP, Binding);
    finally
      RTCP.Free;
    end;
  finally
    Binding.Free;
  end;
end;

function TestTIdSDPMediaStream.T140PT: TIdRTPPayloadType;
begin
  Result := StrToInt(Self.DataFormat);
end;

//* TestTIdSDPMediaStream Published methods ************************************

procedure TestTIdSDPMediaStream.TestAddRTPListener;
var
  L1: TIdRTPTestRTPListener;
  L2: TIdRTPTestRTPListener;
begin
  L1 := TIdRTPTestRTPListener.Create;
  try
    L2 := TIdRTPTestRTPListener.Create;
    try
      Self.Media.AddRTPListener(L1);
      Self.Media.AddRTPListener(L2);

      Self.ReceiveDataOn(Self.Media);

      Check(L1.ReceivedRTP, 'L1 not notified');
      Check(L2.ReceivedRTP, 'L2 not notified');
    finally
      Self.Media.RemoveRTPListener(L2);
      L2.Free;
    end;
  finally
    Self.Media.RemoveRTPListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestAddRTPSendListener;
var
  L1: TIdRTPTestRTPSendListener;
  L2: TIdRTPTestRTPSendListener;
begin
  L1 := TIdRTPTestRTPSendListener.Create;
  try
    L2 := TIdRTPTestRTPSendListener.Create;
    try
      Self.Media.AddRTPSendListener(L1);
      Self.Media.AddRTPSendListener(L2);

      Self.Media.SendData(Self.Text, IntToStr(Self.T140PT));
      Self.Timer.TriggerAllEventsUpToFirst(TIdRTPSendDataWait);

      Check(L1.SentRTP, 'L1 not notified of RTP');
      Check(L2.SentRTP, 'L2 not notified of RTP');

      Self.Media.JoinSession;
      Self.Timer.TriggerAllEventsUpToFirst(TIdRTPTransmissionTimeExpire);

      Check(L1.SentRTCP, 'L1 not notified of RTCP');
      Check(L2.SentRTCP, 'L2 not notified of RTCP');
    finally
      Self.Media.RemoveRTPSendListener(L2);
      L2.Free;
    end;
  finally
    Self.Media.RemoveRTPSendListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestHierarchicallyEncodedStream;
const
  PortCount = 5;
var
  Offset:          Cardinal;
  ReceiverLayerID: Cardinal;
  SenderLayerID:   Cardinal;
begin
  // Both ends must have the same number of ports open.
  Self.SetLocalMediaDesc(Self.Sender,
                         'm=audio 9000/' + IntToStr(PortCount) + ' RTP/AVP 0'#13#10
                       + 'a=rtpmap:98 t140/1000'#13#10);
  Self.SetLocalMediaDesc(Self.Media,
                         'm=audio 8000/' + IntToStr(PortCount) + ' RTP/AVP 0'#13#10
                       + 'a=rtpmap:98 t140/1000'#13#10);
  Self.Media.RemoteDescription  := Self.Sender.LocalDescription;

  Self.Sender.StartListening;
  Self.Media.StartListening;

  Self.Media.AddRTPSendListener(Self);

  for Offset := 0 to PortCount - 1 do begin
    ReceiverLayerID := Self.Media.RemoteDescription.Port + 2*Offset;
    SenderLayerID   := Self.Media.LocalDescription.Port + 2*Offset;
    CheckPortActive(Self.Media.LocalDescription.Connections[0].Address,
                    ReceiverLayerID,
                    IntToStr(Offset + 1) + 'th port not active on the receiver');
    CheckPortActive(Self.Media.LocalDescription.Connections[0].Address,
                    SenderLayerID,
                    IntToStr(Offset + 1) + 'th port not active on the sender');

    Self.Media.SendData(Self.Text, IntToStr(Self.T140PT), SenderLayerID);

    Self.SentData := false;
    Self.Timer.TriggerAllEventsUpToFirst(TIdRTPSendDataWait);
    Check(Self.SentData, IntToStr(Offset + 1) + 'th port of sender didn''t send data');

    CheckEquals(SenderLayerID,
                Self.SendingBinding.LocalPort,
                'Sender used wrong port (hence layer) to send data');
    CheckEquals(ReceiverLayerID,
                Self.SendingBinding.PeerPort,
                'Wrong layer will receive the data');
  end;
end;

procedure TestTIdSDPMediaStream.TestLayeredCodecAddressesAndPorts;
var
  FirstPort:               Cardinal;
  SecondExpectedLocalPort: Cardinal;
  SecondExpectedPeerPort:  Cardinal;
begin
  // We check that the SDP sets the RTCP address/ports of the remote party's
  // various layers by "joining a session" - that will send a Receiver Report to
  // the control ports of the remote party's different layers.

  Self.Media.StopListening;
  Self.Sender.StopListening;

  Self.SetLocalMediaDesc(Self.Media,
                         'm=text 8000/2 RTP/AVP 96'#13#10
                       + 'a=rtpmap:96 t140/1000'#13#10);
  Self.SetLocalMediaDesc(Self.Sender,
                         'm=text 9000/2 RTP/AVP 96'#13#10
                       + 'a=rtpmap:96 t140/1000'#13#10);
  Self.Media.RemoteDescription := Self.Sender.LocalDescription;
  Self.Sender.RemoteDescription := Self.Media.LocalDescription;

  Self.Media.AddRTPSendListener(Self);

  // Self.Media sends off two RTCP packets (one per layer).
  Self.Media.StartListening;
  Self.Media.JoinSession;

  // We use a debug timer, so we trigger the event manually.
  Self.Timer.TriggerEarliestEvent;

  // Now we check that we sent two RTCP packets. We don't necessarily know
  // the order in which the packets will be sent.

  // Check the first: it could come from one of two ports, and could arrive at
  // one of two ports.
  Check((Self.SendingBinding.LocalPort = Self.Media.LocalDescription.Port + 1)
     or (Self.SendingBinding.LocalPort = Self.Media.LocalDescription.Port + 3),
        'The RTCP came from an unexpected port: ' + IntToStr(Self.SendingBinding.LocalPort));
  Check((Self.SendingBinding.PeerPort = Self.Media.RemoteDescription.Port + 1)
     or (Self.SendingBinding.PeerPort = Self.Media.RemoteDescription.Port + 3),
        'The RTCP arrived at an unexpected port: ' + IntToStr(Self.SendingBinding.PeerPort));

  // Obviously, the second packet must come from the OTHER source and arrive at
  // the OTHER destination.
  FirstPort := Self.SendingBinding.PeerPort;
  if (FirstPort = Self.Sender.LocalDescription.Port + 1) then begin
    SecondExpectedLocalPort := Self.Media.LocalDescription.Port + 3;
    SecondExpectedPeerPort  := Self.Media.RemoteDescription.Port + 3;
  end
  else begin
    SecondExpectedLocalPort := Self.Media.LocalDescription.Port + 1;
    SecondExpectedPeerPort  := Self.Media.RemoteDescription.Port + 1;
  end;

  Self.Timer.TriggerEarliestEvent;

  CheckEquals(SecondExpectedPeerPort,
              Self.SendingBinding.PeerPort,
              'The RTCP came from an unexpected port');
  CheckEquals(SecondExpectedLocalPort,
              Self.SendingBinding.LocalPort,
              'The RTCP arrived at an unexpected port');
end;

procedure TestTIdSDPMediaStream.TestMatchPort;
const
  Port      = 8000;
  PortCount = 5;
var
  ActualPort: Cardinal;
  I:          Integer;
begin
  Self.SetLocalMediaDesc(Self.Media,
                         Self.BasicMediaDesc(Port));
  Check(Self.Media.MatchPort(8000), 'Single stream');
  Check(not Self.Media.MatchPort(8002), 'Single stream, wrong port');

  Self.SetLocalMediaDesc(Self.Media,
                         'm=text ' + IntToStr(Port) + '/' + IntToStr(PortCount) + ' TCP text/plain'#13#10);
  for I := 0 to PortCount - 1 do begin
    ActualPort := Port + I*2;
    Check(Self.Media.MatchPort(ActualPort), 'Single hierarchically encoded stream; port = ' + IntToStr(ActualPort));
  end;
  ActualPort := Port + PortCount*2 + 2;
  Check(not Self.Media.MatchPort(ActualPort), 'Single hierarchically encoded stream, wrong port (' + IntToStr(ActualPort) + ')');
end;

procedure TestTIdSDPMediaStream.TestPortsAndPortCount;
const
  PortCount = 5;
var
  I: Integer;
begin
  Self.SetLocalMediaDesc(Self.Sender,
                         'm=audio 9000/' + IntToStr(PortCount) + ' RTP/AVP 0'#13#10);
  Self.Sender.StartListening;

  CheckEquals(5, Self.Sender.PortCount, 'Port count');

  for I := 0 to PortCount - 1 do
   CheckEquals(Self.Sender.LocalDescription.Port + Cardinal(I)*2,
               Self.Sender.Ports[I],
               Format('Ports[%d]', [I]));
end;

procedure TestTIdSDPMediaStream.TestReceiveDataWhenNotReceiver;
begin
  Self.Media.AddDataListener(Self);
  Self.SetLocalMediaDesc(Self.Media,
                         'm=text 8000 RTP/AVP 96'#13#10
                       + 'a=rtpmap:96 t140/1000'#13#10
                       + 'a=inactive');

  Self.ReceiveDataOn(Self.Media);

  Check(not Self.ReceivedData, 'Received data when not a receiver');
end;

procedure TestTIdSDPMediaStream.TestRemoteMembersControlAddressAndPortSet;
begin
  // We check that the SDP sets the RTCP address/port of the remote party
  // by "joining a session" - that will send a Receiver Report to the
  // control port of the remote party.

  // This schedules the joining of the RTP session.
  Self.Media.JoinSession;
  Self.Media.AddRTPSendListener(Self);

  Self.Timer.TriggerAllEventsUpToFirst(TIdRTPTransmissionTimeExpire);

  Check(Self.SentControl, 'No RTCP sent');

  CheckEquals(Self.Media.LocalDescription.Port + 1, Self.SendingBinding.LocalPort, 'The RTCP came from an unexpected port');
  CheckEquals(Self.Sender.LocalDescription.Port + 1, Self.SendingBinding.PeerPort, 'The RTCP arrived at an unexpected port');
end;

procedure TestTIdSDPMediaStream.TestRemoveDataListener;
var
  L1: TIdSdpTestMediaListener;
  L2: TIdSdpTestMediaListener;
begin
  L1 := TIdSdpTestMediaListener.Create;
  try
    L2 := TIdSdpTestMediaListener.Create;
    try
      Self.Media.AddDataListener(L1);
      Self.Media.AddDataListener(L2);
      Self.Media.RemoveDataListener(L1);

      Self.ReceiveDataOn(Self.Media);

      Check(not L1.ReceivedData, 'L1 notified, hence not removed');
      Check(    L2.ReceivedData, 'L2 not notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestRemoveDataSendListener;
var
  L1: TIdSdpTestMediaListener;
  L2: TIdSdpTestMediaListener;
begin
  L1 := TIdSdpTestMediaListener.Create;
  try
    L2 := TIdSdpTestMediaListener.Create;
    try
      Self.Media.AddDataSendListener(L1);
      Self.Media.AddDataSendListener(L2);
      Self.Media.RemoveDataSendListener(L1);

      Self.SendData(Self.Media);

      Check(not L1.SentData, 'L1 notified, hence not removed');
      Check(    L2.SentData, 'L2 not notified');
    finally
      L2.Free;
    end;
  finally
    L1.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestRemoveRTPSendListener;
var
  L: TIdRTPTestRTPSendListener;
begin
  L := TIdRTPTestRTPSendListener.Create;
  try
    Self.Media.AddRTPSendListener(L);
    Self.Media.RemoveRTPSendListener(L);
    Self.Media.SendData(Self.Text, IntToStr(Self.T140PT));

    Check(not L.SentRTP, 'L notified, thus not removed');
  finally
    L.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestRTPListenersGetRTCP;
var
  L1: TIdRTPTestRTPListener;
  L2: TIdRTPTestRTPListener;
begin
  L1 := TIdRTPTestRTPListener.Create;
  try
    L2 := TIdRTPTestRTPListener.Create;
    try
      Self.Media.AddRTPListener(L1);
      Self.Media.AddRTPListener(L2);

      Self.ReceiveControlOn(Self.Media);

      Check(L1.ReceivedRTCP, 'L1 not notified');
      Check(L2.ReceivedRTCP, 'L2 not notified');
    finally
      Self.Media.RemoveRTPListener(L2);
      L2.Free;
    end;
  finally
    Self.Media.RemoveRTPListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestRTPListenersGetRTP;
var
  L1: TIdRTPTestRTPListener;
  L2: TIdRTPTestRTPListener;
begin
  L1 := TIdRTPTestRTPListener.Create;
  try
    L2 := TIdRTPTestRTPListener.Create;
    try
      Self.Media.AddRTPListener(L1);
      Self.Media.AddRTPListener(L2);

      Self.ReceiveDataOn(Self.Media);

      Check(L1.ReceivedRTP, 'L1 not notified');
      Check(L2.ReceivedRTP, 'L2 not notified');
    finally
      Self.Media.RemoveRTPListener(L2);
      L2.Free;
    end;
  finally
    Self.Media.RemoveRTPListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestSetRemoteDescriptionSendsNoPackets;
var
  SameDesc: TIdSdpMediaDescription;
begin
  Self.Media.AddRTPSendListener(Self);

  SameDesc := TIdSdpMediaDescription.Create;
  try
    SameDesc.Assign(Self.Media.RemoteDescription);

    Self.Media.RemoteDescription := SameDesc;
    
    Check(not Self.SentControl, 'SetRemoteDescription sent RTCP packets');
  finally
    SameDesc.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestSetRemoteDescriptionRegistersRemoteRtpMaps;
const
  TEEncodingName = TelephoneEventEncoding;
  TEPayloadType  = 97;
var
  P:   TIdSdpPayload;
  SDP: String;
begin
  SDP :=  'v=0'#13#10
        + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
        + 's=-'#13#10
        + 'c=IN IP4 127.0.0.1'#13#10
        + 'm=audio 8000 RTP/AVP ' + IntToStr(TEPayloadType) + #13#10
        + 'a=rtpmap:' + IntToStr(TEPayloadType) + ' ' + TEEncodingName + #13#10;

  Check(Self.Media.RemoteProfile.HasPayloadType(Self.T140PT),
        'Sanity check: profile already knows about ' + T140EncodingName + '!');
  Check(not Self.Media.RemoteProfile.HasPayloadType(TEPayloadType),
        'Sanity check: profile doesn''t already know about ' + TEEncodingName + '!');

  P := TIdSdpPayload.CreateFrom(SDP);
  try
    Self.Media.RemoteDescription := P.MediaDescriptionAt(0);
  finally
    P.Free;
  end;

  Check(Self.Media.RemoteProfile.HasPayloadType(Self.T140PT),
        T140EncodingName + ' not unregistered');
  Check(Self.Media.RemoteProfile.HasPayloadType(TEPayloadType),
        TEEncodingName + ' not registered');
end;

procedure TestTIdSDPMediaStream.TestSetTimer;
var
  NewTimer: TIdDebugTimerQueue;
  OldTimer: TIdDebugTimerQueue;
begin
  OldTimer := Self.Media.Timer as TIdDebugTimerQueue;

  NewTimer := TIdDebugTimerQueue.Create(false);
  try
    Self.Media.Timer := NewTimer;

    Self.Media.JoinSession;
    Check(nil =  OldTimer.LastEventScheduled(TIdRTPTransmissionTimeExpire), 'Connection used the old timer');
    Check(nil <> NewTimer.LastEventScheduled(TIdRTPTransmissionTimeExpire), 'Connection didn''t use the new timer');
  finally
    Self.Media.Timer := OldTimer;
    NewTimer.Terminate;
  end;
end;

procedure TestTIdSDPMediaStream.TestStartListening;
begin
  Self.Media.StopListening;
  Self.Sender.StopListening;

  Self.SetLocalMediaDesc(Self.Sender,
                         'm=audio 9000 RTP/AVP 98'#13#10
                       + 'a=rtpmap:98 t140/1000'#13#10);
  Self.SetLocalMediaDesc(Self.Media,
                         'm=audio 8000 RTP/AVP 0'#13#10
                       + 'a=rtpmap:98 t140/1000'#13#10);
  Self.Sender.RemoteDescription := Self.Media.LocalDescription;
  Self.Media.RemoteDescription  := Self.Sender.LocalDescription;

  CheckPortFree(Self.Media.LocalDescription.Connections[0].Address,
                Self.Media.LocalDescription.Port,
                'Local port active before Initialize');

  Self.Media.AddRTPSendListener(Self);

  Self.Media.StartListening;

  CheckPortActive(Self.Media.LocalDescription.Connections[0].Address,
                  Self.Media.LocalDescription.Port,
                  'Local port not active after StartListening');

  Self.Media.JoinSession;
  Self.Timer.TriggerAllEventsUpToFirst(TIdRTPTransmissionTimeExpire);

  Check(Self.SentControl, 'No RTCP sent');

  Self.Media.SendData(Self.Text, IntToStr(Self.T140PT));
  Self.Timer.TriggerAllEventsUpToFirst(TIdRTPSendDataWait);

  Check(Self.SentData, 'No RTP sent');
end;

procedure TestTIdSDPMediaStream.TestStartListeningRegistersLocalRtpMaps;
const
  TEEncodingName = TelephoneEventEncoding;
  TEPayloadType  = 97;
var
  P:   TIdSdpPayload;
  SDP: String;
begin
  SDP :=  'v=0'#13#10
        + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
        + 's=-'#13#10
        + 'c=IN IP4 127.0.0.1'#13#10
        + 'm=text 8000 RTP/AVP ' + IntToStr(TEPayloadType)+ #13#10
        + 'a=rtpmap:' + IntToStr(TEPayloadType) + ' ' + TEEncodingName + #13#10;

  Check(Self.Media.RemoteProfile.HasPayloadType(Self.T140PT),
        'Sanity check: profile already knows about ' + T140EncodingName + '!');
  Check(not Self.Media.LocalProfile.HasPayloadType(TEPayloadType),
        'Sanity check: profile doesn''t already know about ' + TEEncodingName + '!');

  P := TIdSdpPayload.CreateFrom(SDP);
  try
    Self.Media.LocalDescription := P.MediaDescriptionAt(0);
  finally
    P.Free;
  end;

  Self.Media.StartListening;

  Check(not Self.Media.LocalProfile.HasPayloadType(Self.T140PT),
        T140EncodingName + ' not unregistered');
  Check(Self.Media.LocalProfile.HasPayloadType(TEPayloadType),
        TEEncodingName + ' not registered');
end;

procedure TestTIdSDPMediaStream.TestStartListeningTriesConsecutivePorts;
const
  BlockedPort = 10000;
var
  P:           TIdSdpPayload;
  PortBlocker: TIdMockRTPPeer;
  SDP:         String;
begin
  PortBlocker := TIdMockRTPPeer.Create;
  try
    PortBlocker.Address := '127.0.0.1';
    PortBlocker.RTPPort := BlockedPort;
    PortBlocker.Active  := true;

    CheckPortActive('127.0.0.1', BlockedPort, 'The PortBlocker isn''t blocking the port');

    SDP := 'v=0'#13#10
         + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
         + 's=-'#13#10
         + 'c=IN IP4 127.0.0.1'#13#10
         + 'm=text ' + IntToStr(BlockedPort) + ' RTP/AVP 0'#13#10;

    P := TIdSdpPayload.CreateFrom(SDP);
    try
      Self.Media.LocalDescription := P.MediaDescriptionAt(0);
    finally
      P.Free;
    end;

    Self.Media.StartListening;
    CheckPortActive('127.0.0.1', BlockedPort + 2, 'Next available RTP port not used');

    CheckEquals(BlockedPort + 2, Self.Media.LocalDescription.Port, 'Actual port used');

    Self.Media.StopListening;

    CheckPortFree('127.0.0.1', BlockedPort + 2, 'Port not closed');
  finally
    PortBlocker.Free;
  end;
end;

procedure TestTIdSDPMediaStream.TestStopListeningStopsListening;
begin
  Self.Media.AddDataListener(Self);
  Self.Media.AddRTPSendListener(Self);
  Self.Media.StopListening;

  Self.ReceiveDataOn(Self.Media);

  Check(not Self.ReceivedData, 'Server didn''t stop listening');
  Check(Self.SentBye, 'Server didn''t send an RTCP BYE');
end;

procedure TestTIdSDPMediaStream.TestUnusedPortsSwitchOff;
var
  Address: String;
  Port:    Cardinal;
begin
  Address := Localhost(Id_IPv4);
  Port    := 8000;

  Self.SetLocalMediaDesc(Self.Media,
                         'm=audio 8000 RTP/AVP 0'#13#10);
  Self.Media.StartListening;
  CheckPortActive(Address, Port, 'RTP/AVP port not listening');

  Self.SetRemoteMediaDesc(Self.Media,
                          'm=audio 0 RTP/AVP 0'#13#10);
  CheckPortFree(Address, Port, 'Unused RTP/AVP port not freed');
end;

//******************************************************************************
//* TestTIdSdpNullMediaStream                                                  *
//******************************************************************************
//* TestTIdSdpNullMediaStream Protected methods ********************************

function TestTIdSdpNullMediaStream.BasicMediaDesc(Port: Cardinal = 8000; PortCount: Cardinal = 1): String;
begin
  if (PortCount > 1) then
    Result := Format('m=audio %d/%d unknown 0'#13#10, [Port])
  else
    Result := Format('m=audio %d unknown 0'#13#10, [Port]);
end;

function TestTIdSdpNullMediaStream.CreateStream: TIdSdpBaseMediaStream;
begin
  Result := TIdSdpNullMediaStream.Create;
end;

procedure TestTIdSdpNullMediaStream.ReceiveDataOn(S: TIdSdpBaseMediaStream);
begin
  // Null streams don't even instantiate servers - there's nothing that
  // can receive data!
end;

procedure TestTIdSdpNullMediaStream.SendData(Stream: TIdSdpBaseMediaStream);
var
  S: TStream;
begin
  S := TStringStream.Create('foo');
  try
    Stream.SendData(S, Self.DataFormat);
    Self.Timer.TriggerAllEventsUpToFirst(TIdRTPSendDataWait);
  finally
    S.Free;
  end;
end;

//* TestTIdSdpNullMediaStream Published methods ********************************

procedure TestTIdSdpNullMediaStream.TestSendData;
var
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Self.AddSendingChecking(Stream);
    try
      Self.SendData(Stream);

      CheckNoDataSent('Data sent on a null stream');
    finally
      Self.RemoveSendingChecking(Stream);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpNullMediaStream.TestSetTimer;
begin
  Check(true, 'Null timers don''t do anything.');
end;

//******************************************************************************
//* TestTIdSdpTcpConnectionRegistry                                            *
//******************************************************************************
//* TestTIdSdpTcpConnectionRegistry Public methods *****************************

procedure TestTIdSdpTcpConnectionRegistry.SetUp;
begin
  inherited SetUp;

  Self.Agent := TIdSdpMockTcpConnection.Create;
end;

procedure TestTIdSdpTcpConnectionRegistry.TearDown;
begin
  Self.Agent.Free;

  inherited TearDown;
end;

//* TestTIdSdpTcpConnectionRegistry Private methods ****************************

procedure TestTIdSdpTcpConnectionRegistry.TestClientConnectedTo;
const
  ArbitraryAddress = '1.2.3.4';
  ArbitraryPort    = 8000;
  AnotherPort      = 9000;
var
  Client: TIdSdpMockTcpClientConnection;
begin
  Client := TIdSdpMockTcpClientConnection.Create;
  try
    Client.ConnectTo(ArbitraryAddress, ArbitraryPort);

    Check(Client = TIdSdpTcpConnectionRegistry.ClientConnectedTo(ArbitraryAddress, ArbitraryPort),
          'Client not found in registry');

    Check(nil = TIdSdpTcpConnectionRegistry.ClientConnectedTo(TIdIPAddressParser.IncIPAddress(ArbitraryAddress), ArbitraryPort),
          'Client found in registry (no such host');
    Check(nil = TIdSdpTcpConnectionRegistry.ClientConnectedTo(ArbitraryAddress, ArbitraryPort + 1),
          'Client found in registry (no such port');

    Client.ConnectTo(ArbitraryAddress, AnotherPort);
    Check(nil = TIdSdpTcpConnectionRegistry.ClientConnectedTo(ArbitraryAddress, ArbitraryPort),
          'Client found in registry after moving');
    Check(Client = TIdSdpTcpConnectionRegistry.ClientConnectedTo(ArbitraryAddress, AnotherPort),
          'Client not found in registry after moving');
  finally
    Client.Free;
  end;
end;

procedure TestTIdSdpTcpConnectionRegistry.TestFindConnection;
begin
  Check(Self.Agent = TIdSdpTcpConnectionRegistry.FindConnection(Self.Agent.ID),
        'FindConnection: registered TcpConnection');
end;

procedure TestTIdSdpTcpConnectionRegistry.TestFindConnectionNoSuchRegisteredObject;
const
  NotARegisteredObject = 'Registered objects don''t have an ID like this';
begin
  Self.ExpectedException := ERegistry;
  TIdSdpTcpConnectionRegistry.FindConnection(NotARegisteredObject);
end;

procedure TestTIdSdpTcpConnectionRegistry.TestFindConnectionNotATcpConnection;
var
  RandomRegisteredObject: TIdRegisteredObject;
begin
  RandomRegisteredObject := TIdRegisteredObject.Create;
  try
    Self.ExpectedException := ERegistry;
    TIdSdpTcpConnectionRegistry.FindConnection(RandomRegisteredObject.ID);
  finally
    RandomRegisteredObject.Free;
  end;
end;

procedure TestTIdSdpTcpConnectionRegistry.TestServerOn;
const
  ArbitraryAddress = '1.2.3.4';
  ArbitraryPort    = 8000;
  AnotherPort      = 9000;
var
  Server: TIdSdpMockTcpServerConnection;
begin
  Server := TIdSdpMockTcpServerConnection.Create;
  try
    Server.ListenOn(ArbitraryAddress, ArbitraryPort);

    Check(Server = TIdSdpTcpConnectionRegistry.ServerOn(ArbitraryAddress, ArbitraryPort),
          'Server not found in registry');

    Check(nil = TIdSdpTcpConnectionRegistry.ServerOn(TIdIPAddressParser.IncIPAddress(ArbitraryAddress), ArbitraryPort),
          'Server found in registry (no such host');
    Check(nil = TIdSdpTcpConnectionRegistry.ServerOn(ArbitraryAddress, ArbitraryPort + 1),
          'Server found in registry (no such port');

    Server.ListenOn(ArbitraryAddress, AnotherPort);
    Check(nil = TIdSdpTcpConnectionRegistry.ServerOn(ArbitraryAddress, ArbitraryPort),
          'Server found in registry after moving');
    Check(Server = TIdSdpTcpConnectionRegistry.ServerOn(ArbitraryAddress, AnotherPort),
          'Server not found in registry after moving');
  finally
    Server.Free;
  end;
end;

procedure TestTIdSdpTcpConnectionRegistry.TestServerRunningOn;
const
  Address = '127.0.0.1';
  Port    = 8000;
var
  Server: TIdSdpMockTcpServerConnection;
begin
  Server := TIdSdpMockTcpServerConnection.Create;
  try
    Check(not TIdSdpTcpConnectionRegistry.ServerRunningOn(Address, Port),
          'There''s a server running where there should be no server');

    Server.ListenOn(Address, Port);
    Check(Server.IsActive, 'Agent isn''t active');

    Check(TIdSdpTcpConnectionRegistry.ServerRunningOn(Address, Port),
          'There''s no running server');
  finally
    Server.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpMockTcpNullConnection                                            *
//******************************************************************************
//* TestTIdSdpMockTcpNullConnection Protected methods **************************

procedure TestTIdSdpMockTcpNullConnection.Activate(Connection: TIdSdpMockTcpConnection);
begin
  Connection.ConnectTo('1.2.3.4', 1234);
end;

function TestTIdSdpMockTcpNullConnection.CreateConnection: TIdSdpMockTcpConnection;
begin
  Result := TIdSdpMockTcpNullConnection.Create;
end;

//* TestTIdSdpMockTcpNullConnection Published methods **************************

procedure TestTIdSdpMockTcpNullConnection.TestAddDataListener;
var
  L1, L2: TIdSdpTestConnectionListener;
begin
  L1 := TIdSdpTestConnectionListener.Create;
  try
    L2 := TIdSdpTestConnectionListener.Create;
    try
      Self.Conn.AddDataListener(L1);
      Self.Conn.AddDataListener(L2);

      Self.Conn.ReceiveData(Self.Data, Self.ReceivedOn);

      Check(not L1.OnDataCalled, 'L1 notified (null connections don''t receive data!)');
      Check(not L2.OnDataCalled, 'L2 notified (null connections don''t receive data!)');
    finally
      Self.Conn.RemoveDataListener(L2);
      L2.Free;
    end;
  finally
    Self.Conn.RemoveDataListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSdpMockTcpNullConnection.TestConnectTo;
begin
  Self.Conn.ConnectTo('1.2.3.4', 1234);
  Check(not Self.Conn.IsActive,    'Null connections can''t be active');
  Check(not Self.Conn.IsConnected, 'Null connections can''t be connected');
end;

procedure TestTIdSdpMockTcpNullConnection.TestDisconnect;
begin
  Self.Conn.ConnectTo('1.2.3.4', 1234);
  Self.Conn.Disconnect;
  Check(not Self.Conn.IsActive,    'Null connections can''t be active');
  Check(not Self.Conn.IsConnected, 'Null connections can''t be connected');
end;

procedure TestTIdSdpMockTcpNullConnection.TestForceDisconnect;
var
  L: TIdSdpTestConnectionListener;
begin
  L := TIdSdpTestConnectionListener.Create;
  try
    Self.Conn.AddDataListener(L);

    Self.Conn.ConnectTo('1.2.3.4', 1234);
    Self.Conn.ForceDisconnect;

    Check(not Self.Conn.IsActive,    'Null connections can''t be active');
    Check(not Self.Conn.IsConnected, 'Null connections can''t be connected');
    Check(not L.OnDisconnectCalled,  'Null connections can''t be disconnected');
  finally
    Self.Conn.RemoveDataListener(L);
    L.Free;
  end;
end;

procedure TestTIdSdpMockTcpNullConnection.TestListenOn;
begin
  Self.Conn.ListenOn('1.2.3.4', 1234);
  Check(not Self.Conn.IsActive, 'Null connections can''t be active');
end;

procedure TestTIdSdpMockTcpNullConnection.TestRemoveDataListener;
var
  L1, L2: TIdSdpTestConnectionListener;
begin
  L1 := TIdSdpTestConnectionListener.Create;
  try
    L2 := TIdSdpTestConnectionListener.Create;
    try
      Self.Conn.AddDataListener(L1);
      Self.Conn.AddDataListener(L2);
      Self.Conn.RemoveDataListener(L2);

      Self.Conn.ReceiveData(Self.Data, Self.ReceivedOn);

      Check(not L1.OnDataCalled, 'L1 notified (null connections don''t receive data!)');
      Check(not L2.OnDataCalled, 'L2 notified (null connections don''t receive data!)');
    finally
      L2.Free;
    end;
  finally
    Self.Conn.RemoveDataListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSdpMockTcpNullConnection.TestRemotePartyAccepts;
var
  L: TIdSdpTestConnectionListener;
begin
  L := TIdSdpTestConnectionListener.Create;
  try
    Self.Conn.AddDataListener(L);

    Check(not Self.Conn.IsConnected, 'New connection can''t be connected');

    Self.Conn.ConnectTo('1.2.3.4', 1234);
    Check(not Self.Conn.IsConnected, 'Remote party has yet to establish their end');

    Self.Conn.RemotePartyAccepts;
    Check(not Self.Conn.IsConnected, 'Remote party established their end, but null connections can''t be connected');
    Check(not L.OnConnectCalled, 'L notified: null connections can''t connect');
  finally
    Self.Conn.RemoveDataListener(L);
    L.Free;
  end;
end;

procedure TestTIdSdpMockTcpNullConnection.TestSendData;
var
  Data: TStringStream;
begin
  CheckEquals('', Self.Conn.SentData, 'New connection can''t have sent data');

  Data := TStringStream.Create('foo');
  try
    Self.Conn.SendData(Data);

    CheckEquals('', Self.Conn.SentData, 'Cannot send data when not active');

    Self.Conn.ConnectTo('127.0.0.1', 8000);

    Self.Conn.SendData(Data);
    CheckEquals('', Self.Conn.SentData, 'Null connections can''t send data');
  finally
    Data.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpMockTcpClientConnection                                          *
//******************************************************************************
//* TestTIdSdpMockTcpClientConnection Protected methods ************************

procedure TestTIdSdpMockTcpClientConnection.Activate(Connection: TIdSdpMockTcpConnection);
begin
  (Connection as TIdSdpMockTcpClientConnection).ConnectTo('1.2.3.4', 1234);
end;

function TestTIdSdpMockTcpClientConnection.CreateConnection: TIdSdpMockTcpConnection;
begin
  Result := TIdSdpMockTcpClientConnection.Create;
end;

//* TestTIdSdpMockTcpClientConnection Published methods ************************

procedure TestTIdSdpMockTcpClientConnection.TestConnectTo;
const
  Address = '1.2.3.4';
  Port    = 1234;
begin
  Self.Conn.ConnectTo(Address, Port);

  Check(Self.Conn.ConnectToCalled, 'ConnectTo flag not set');
  Check(Self.Conn.IsActive,        'Connection not marked as active');
  Check(not Self.Conn.IsServer,    'Connection thinks it''s a server');
  CheckEquals(Address, Self.Conn.PeerAddress, 'Tried to connect to wrong address');
  CheckEquals(Port,    Self.Conn.PeerPort,    'Tried to connect to wrong port');
end;

procedure TestTIdSdpMockTcpClientConnection.TestDisconnect;
begin
  Self.Conn.ConnectTo('1.2.3.4', 1234);
  Self.Conn.Disconnect;
  Check(not Self.Conn.IsActive,    'Connection still active after Disconnect');
  Check(not Self.Conn.IsConnected, 'Connection still connected after Disconnect');
end;

procedure TestTIdSdpMockTcpClientConnection.TestForceDisconnect;
var
  L: TIdSdpTestConnectionListener;
begin
  L := TIdSdpTestConnectionListener.Create;
  try
    Self.Conn.AddDataListener(L);

    Self.Conn.ConnectTo('1.2.3.4', 1234);
    Self.Conn.RemotePartyAccepts;

    Check(Self.Conn.IsConnected, 'Connection not marked as connected');

    Self.Conn.ForceDisconnect;
    Check(not Self.Conn.IsActive, 'Connection not marked as inactive');
    Check(L.OnDisconnectCalled, 'Listener not notified of disconnection');
    Check(Self.Conn = L.ConnectionParam, 'Connection param');
  finally
    Self.Conn.RemoveDataListener(L);
    L.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpMockTcpServerConnection                                          *
//******************************************************************************
//* TestTIdSdpMockTcpServerConnection Protected methods ************************

procedure TestTIdSdpMockTcpServerConnection.Activate(Connection: TIdSdpMockTcpConnection);
begin
  (Connection as TIdSdpMockTcpServerConnection).ListenOn('1.2.3.4', 1234);
end;

function TestTIdSdpMockTcpServerConnection.CreateConnection: TIdSdpMockTcpConnection;
begin
  Result := TIdSdpMockTcpServerConnection.Create;
end;

//* TestTIdSdpMockTcpServerConnection Published methods ************************

procedure TestTIdSdpMockTcpServerConnection.TestDisconnect;
begin
  Self.Conn.ListenOn('1.2.3.4', 1234);
  Self.Conn.RemotePartyAccepts;
  Check(Self.Conn.IsConnected, 'Connection not connected');

  Self.Conn.Disconnect;
  
  Check(not Self.Conn.IsActive,    'Connection still active after Disconnect');
  Check(not Self.Conn.IsConnected, 'Connection still connected after Disconnect');
end;

procedure TestTIdSdpMockTcpServerConnection.TestForceDisconnect;
var
  L: TIdSdpTestConnectionListener;
begin
  L := TIdSdpTestConnectionListener.Create;
  try
    Self.Conn.AddDataListener(L);

    Self.Conn.ListenOn('1.2.3.4', 1234);
    Self.Conn.RemotePartyAccepts;

    Check(Self.Conn.IsActive, 'Connection not marked as active');

    Self.Conn.ForceDisconnect;
    Check(not Self.Conn.IsActive, 'Connection not marked as inactive');
    Check(L.OnDisconnectCalled, 'Listener not notified of disconnection');
    Check(Self.Conn = L.ConnectionParam, 'Connection param');
  finally
    Self.Conn.RemoveDataListener(L);
    L.Free;
  end;
end;

procedure TestTIdSdpMockTcpServerConnection.TestListenOn;
const
  Address = '1.2.3.4';
  Port    = 1234;
begin
  Self.Conn.ListenOn(Address, Port);

  Check(Self.Conn.ListenOnCalled, 'ListenOn flag not set');
  Check(Self.Conn.IsActive,       'Connection not marked as active');
  Check(Self.Conn.IsServer,       'Connection thinks it''s a client');

  CheckEquals(Address, Self.Conn.Address, 'Listening on wrong address');
  CheckEquals(Port,    Self.Conn.Port,    'Listening on wrong port');
end;

procedure TestTIdSdpMockTcpServerConnection.TestRemotePartyAccepts;
begin
  Check(not Self.Conn.IsConnected, 'New connection can''t be connected');

  Self.Conn.ListenOn('1.2.3.4', 1234);
  Check(not Self.Conn.IsConnected, 'Remote party has yet to establish their end');

  Self.Conn.RemotePartyAccepts;
  Check(Self.Conn.IsConnected, 'Remote party established their end, but no connection');
end;

//******************************************************************************
//* TestTIdSdpTcpClient                                                        *
//******************************************************************************
//* TestTIdSdpTcpClient Public methods *****************************************

procedure TestTIdSdpTcpClient.SetUp;
begin
  inherited SetUp;

  Self.Lock        := TCriticalSection.Create;
  Self.RecvBinding := TIdConnectionBindings.Create;

  Self.Server := TIdTCPServer.Create(nil);
  Self.Server.OnExecute := Self.SendData;
  Self.Server.Bindings.Add;
  Self.Server.Bindings[0].IP   := Localhost(Id_IPv4);
  Self.Server.Bindings[0].Port := 8000;

  try
    Self.Server.Active := true;
  except
    Fail(Format('Shut down the service using %s:%d/tcp', [Self.Server.Bindings[0].IP, Self.Server.Bindings[0].Port]));
  end;

  Self.Client := TIdSdpTcpClient.Create(nil);
  Self.Client.Host             := Self.Server.Bindings[0].IP;
  Self.Client.OnReceiveMessage := Self.ReceiveMessage;
  Self.Client.Port             := Self.Server.Bindings[0].Port;

  Self.FireOnceFlag := false;
end;

procedure TestTIdSdpTcpClient.TearDown;
begin
  Self.Lock.Acquire;
  try
    Self.Client.Free;
    Self.Server.Free;
    Self.RecvBinding.Free;
  finally
    Self.Lock.Release;
  end;

  inherited TearDown;
end;

//* TestTIdSdpTcpClient Private methods ****************************************

procedure TestTIdSdpTcpClient.ReceiveMessage(Sender: TObject; Msg: String; ReceivedOn: TIdConnectionBindings);
begin
  Self.Lock.Acquire;
  try
    Self.RecvBinding.Assign(ReceivedOn);
    Self.ThreadEvent.SetEvent;
  finally
    Self.Lock.Release;
  end;
end;

procedure TestTIdSdpTcpClient.SendData(Thread: TIdPeerThread);
begin
  if not FireOnceFlag then begin
    Thread.Connection.Write('foo');
    Thread.Connection.DisconnectSocket;

    FireOnceFlag := true;
  end;
end;

//* TestTIdSdpTcpClient Published methods **************************************

procedure TestTIdSdpTcpClient.TestReceiveMessageRecordsBinding;
begin
  Self.Client.Connect(Self.DefaultTimeout);
  Self.Client.ReceiveMessages;

  Self.WaitForSignaled('No messages received');

  Self.Lock.Acquire;
  try
    CheckEquals(Self.Client.Socket.Binding.IP,       Self.RecvBinding.LocalIP,   'Wait''s LocalIP');
    CheckEquals(Self.Client.Socket.Binding.Port,     Self.RecvBinding.LocalPort, 'Wait''s LocalPort');
    CheckEquals(Self.Client.Socket.Binding.PeerIP,   Self.RecvBinding.PeerIP,    'Wait''s PeerIP');
    CheckEquals(Self.Client.Socket.Binding.PeerPort, Self.RecvBinding.PeerPort,  'Wait''s PeerPort');
    CheckEquals(TcpTransport,                        Self.RecvBinding.Transport, 'Wait''s Transport');
  finally
    Self.Lock.Release;
  end;
end;

//******************************************************************************
//* TestTIdSdpTcpClientConnection                                              *
//******************************************************************************
//* TestTIdSdpTcpClientConnection Public methods *******************************

procedure TestTIdSdpTcpClientConnection.SetUp;
var
  Binding: TIdSocketHandle;
begin
  inherited SetUp;

  Self.Timer := TIdThreadedTimerQueue.Create(false);

  Self.Connected         := false;
  Self.ConnectionAddress := '';
  Self.ConnectionPort    := 0;
  Self.FirstExecute      := true;

  Self.Connection := TIdSdpTcpClientConnection.Create;
  Self.Connection.Timeout := Self.DefaultTimeout div 2;
  Self.Connection.Timer   := Self.Timer;

  Self.Listener := TIdSdpTestConnectionListener.Create;

  Self.Connection.AddDataListener(Self.Listener);
  Self.Connection.AddDataListener(Self);

  Self.DisconnectEvent := TSimpleEvent.Create;
  Self.ServerEvent     := TSimpleEvent.Create;

  Self.Server := TIdTCPServer.Create(nil);
  Binding := Self.Server.Bindings.Add;
  Binding.IP   := '127.0.0.1';
  Binding.Port := 9000;
  Self.Server.OnConnect := Self.RegisterConnectionAttempt;
  Self.Server.OnExecute := Self.DoNothing;
  Self.Server.Active    := true;

  Self.TestData := TStringStream.Create('In R''lyeh dead Cthulhu lies dreaming');
end;

procedure TestTIdSdpTcpClientConnection.TearDown;
begin
  Self.Connection.RemoveDataListener(Self);
  Self.Connection.RemoveDataListener(Self.Listener);

  Self.TestData.Free;
  Self.Server.Free;
  Self.ServerEvent.Free;
  Self.DisconnectEvent.Free;
  Self.Listener.Free;
  Self.Connection.Free;

  Self.Timer.Terminate;

  inherited TearDown;
end;

//* TestTIdSdpTcpClientConnection Published methods ****************************

procedure TestTIdSdpTcpClientConnection.DisconnectClient;
var
  Connections: TList;
  I:           Integer;
begin
  Connections := Self.Server.Threads.LockList;
  try
    for I := 0 to Connections.Count - 1 do
      TIdPeerThread(Connections[I]).Connection.Disconnect;
  finally
    Self.Server.Threads.UnlockList;
  end;
end;

procedure TestTIdSdpTcpClientConnection.DoNothing(Thread: TIdPeerThread);
begin
end;

procedure TestTIdSdpTcpClientConnection.OnConnect(Connection: TIdSdpBaseTcpConnection);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSdpTcpClientConnection.OnData(Connection: TIdSdpBaseTcpConnection; Data: TStream);
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    S.CopyFrom(Data, 0);

    Self.ReceivedData := S.DataString;

    Self.CollectedReceivedData := Self.CollectedReceivedData + Self.ReceivedData;
  finally
    S.Free;
  end;

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSdpTcpClientConnection.OnDisconnect(Connection: TIdSdpBaseTcpConnection);
begin
  Self.ThreadEvent.SetEvent;
  Self.DisconnectEvent.SetEvent;
end;

procedure TestTIdSdpTcpClientConnection.OnException(Connection: TIdSdpBaseTcpConnection;
                                                    ExceptionType: ExceptClass;
                                                    ExceptionMessage: String);
begin
  Self.ExceptionMessage := ExceptionMessage;
  Self.ExceptionType    := ExceptionType;
end;

procedure TestTIdSdpTcpClientConnection.ReadTestData(Thread: TIdPeerThread);
begin
  Self.ReceivedData := Thread.Connection.ReadString(Length(Self.TestData.DataString));

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSdpTcpClientConnection.RegisterConnectionAttempt(Thread: TIdPeerThread);
begin
  Self.Connected := true;
  Self.ConnectionAddress := Thread.Connection.Socket.Binding.PeerIP;
  Self.ConnectionPort    := Thread.Connection.Socket.Binding.PeerPort;

  Self.ServerEvent.SetEvent;
end;

function TestTIdSdpTcpClientConnection.ServerIP: String;
begin
  Result := Self.Server.Bindings[0].IP;
end;

function TestTIdSdpTcpClientConnection.ServerPort: Integer;
begin
  Result := Self.Server.Bindings[0].Port;
end;

procedure TestTIdSdpTcpClientConnection.WriteTestData;
begin
  Self.WriteTestData(Self.TestData);
end;

procedure TestTIdSdpTcpClientConnection.WriteTestData(Chunk: TStream);
var
  Connections: TList;
  I:           Integer;
begin
  Connections := Self.Server.Threads.LockList;
  try
    for I := 0 to Connections.Count - 1 do
      TIdPeerThread(Connections[I]).Connection.WriteStream(Chunk);
  finally
    Self.Server.Threads.UnlockList;
  end;
end;

procedure TestTIdSdpTcpClientConnection.WriteTestData(Thread: TIdPeerThread);
begin
  // This executes in the context of the server thread.
  Thread.Connection.ReadString(Length(Self.TestData.DataString));
  if Self.FirstExecute then
    Thread.Connection.WriteStream(Self.TestData);

  Self.FirstExecute := false;
end;

//* TestTIdSdpTcpClientConnection Private methods ******************************

procedure TestTIdSdpTcpClientConnection.TestAddressAndPortWithNoConnection;
begin
  CheckEquals(IPv4ZeroAddress, Self.Connection.Address,     'Address');
  CheckEquals(TcpDiscardPort,  Self.Connection.Port,        'Port');
  CheckEquals(IPv4ZeroAddress, Self.Connection.PeerAddress, 'PeerAddress');
  CheckEquals(TcpDiscardPort,  Self.Connection.PeerPort,    'PeerPort');
end;

procedure TestTIdSdpTcpClientConnection.TestConnectTo;
begin
  Check(not Self.Connection.IsActive,    'Connection active before connection attempt');
  Check(not Self.Connection.IsConnected, 'Connection connected before connection attempt');
  Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);

  Self.WaitForSignaled(Self.ServerEvent, 'No connection made');

  Check(Self.Connected,                'No connection made: flag not set');
  Check(Self.Listener.OnConnectCalled, 'Listener not notified of connection');

  Check(Self.Connection.IsActive,    'Connection not active');
  Check(Self.Connection.IsConnected, 'Connection not connected');
  CheckNotEquals('',                   Self.Connection.Address,     'Connection address not set');
  CheckNotEquals(IPv4ZeroAddress,      Self.Connection.Address,     'Connection address not set');
  CheckNotEquals(0,                    Self.Connection.Port,        'Connection port not set');
  CheckNotEquals(TcpDiscardPort,       Self.Connection.Port,        'Connection port not set');
  CheckEquals(Self.Connection.Address, Self.ConnectionAddress,      'Connection address');
  CheckEquals(Self.Connection.Port,    Self.ConnectionPort,         'Port discrepancy');
  CheckEquals(ServerIP,                Self.Connection.PeerAddress, 'Connection peer address not set');
  CheckEquals(ServerPort,              Self.Connection.PeerPort,    'Connection peer port not set');
end;

procedure TestTIdSdpTcpClientConnection.TestConnectToNonexistentPeer;
begin
  Self.Server.Active := false;

  Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);

  CheckEquals(EIdConnectException, Self.ExceptionType, 'Unexpected exception type');
end;

procedure TestTIdSdpTcpClientConnection.TestConnectToTwice;
begin
  Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);

  Self.ServerEvent.ResetEvent;
  Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);

  Self.WaitForSignaled(Self.DisconnectEvent, 'No disconnection made');
  Self.WaitForSignaled(Self.ServerEvent,     'No re-connection made');
end;

procedure TestTIdSdpTcpClientConnection.TestDisconnect;
begin
  Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);

  Self.WaitForSignaled(Self.ServerEvent, 'No connection made');

  Self.Connection.Disconnect;

  Check(not Self.Connection.IsActive,    'Connection still thinks it''s active');
  Check(not Self.Connection.IsConnected, 'Connection still thinks it''s connected');
end;

procedure TestTIdSdpTcpClientConnection.TestDisconnectTwice;
begin
  Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);

  Self.WaitForSignaled(Self.ServerEvent, 'No connection made');

  Self.Connection.Disconnect;

  Self.DefaultTimeout := OneSecond;
  Self.ServerEvent.ResetEvent;
  Self.Connection.Disconnect;
  Self.WaitForTimeout(Self.ServerEvent, 'Disconnection notification received');
end;

procedure TestTIdSdpTcpClientConnection.TestLocalDisconnectNotifiesListeners;
begin
  Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);

  Self.WaitForSignaled(Self.ServerEvent, 'No connection made');

  Self.ThreadEvent.ResetEvent;
  Self.Connection.Disconnect;

  Self.WaitForSignaled('No disconnection notification');
  Check(Self.Listener.OnDisconnectCalled, 'Listener not notified of disconnection');
  Check(not Self.Connection.IsActive,     'Connection still thinks it''s active');
  Check(not Self.Connection.IsConnected,  'Connection still thinks it''s connected');
end;

procedure TestTIdSdpTcpClientConnection.TestReceiveData;
begin
  Self.Server.OnExecute := Self.WriteTestData;

  Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);
  Self.WaitForSignaled(Self.ServerEvent, 'No connection made');

  Self.ThreadEvent.ResetEvent;
  Self.WriteTestData;
  Self.WaitForSignaled('Data not received');

  Self.DisconnectClient;
  Self.WaitForSignaled(Self.DisconnectEvent, 'No disconnection');

  CheckEquals(Self.TestData.DataString, Self.CollectedReceivedData, 'Data not received correctly');
end;

procedure TestTIdSdpTcpClientConnection.TestReceiveDataMultipleTimes;
var
  ChunkOne, ChunkTwo: TStringStream;
begin
  Self.Server.OnExecute := Self.WriteTestData;

  ChunkOne := TStringStream.Create('1');
  try
    ChunkTwo := TStringStream.Create('2');
    try
      Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);
      Self.WaitForSignaled(Self.ServerEvent, 'No connection made');

      Self.ThreadEvent.ResetEvent;
      Self.WriteTestData(ChunkOne);
      Self.WaitForSignaled('ChunkOne not received');
      CheckEquals(ChunkOne.DataString, Self.ReceivedData, 'First chunk received');

      Self.ThreadEvent.ResetEvent;
      Self.WriteTestData(ChunkTwo);
      Self.WaitForSignaled('ChunkTwo not received');
      CheckEquals(ChunkTwo.DataString, Self.ReceivedData, 'Second chunk received');
    finally
      ChunkTwo.Free;
    end;
  finally
    ChunkOne.Free;
  end;
end;

procedure TestTIdSdpTcpClientConnection.TestRemoteDisconnectionNotifiesListeners;
begin
  Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);
  Self.WaitForSignaled(Self.ServerEvent, 'No connection made');

  Self.DisconnectClient;
  Self.WaitForSignaled(Self.DisconnectEvent, 'No disconnection notification for server');

  Check(Self.Listener.OnDisconnectCalled, 'Listener not notified of disconnection');
  Check(not Self.Connection.IsActive,     'Connection still thinks it''s active');
  Check(not Self.Connection.IsConnected,  'Connection still thinks it''s connected');
end;

procedure TestTIdSdpTcpClientConnection.TestSendData;
begin
  Self.Server.OnExecute := Self.ReadTestData;

  Self.Connection.ConnectTo(Self.ServerIP, Self.ServerPort);
  Self.WaitForSignaled(Self.ServerEvent, 'No connection made');

  Self.ThreadEvent.ResetEvent;
  Self.Connection.SendData(Self.TestData);

  Self.WaitForSignaled('No connection made');
  CheckEquals(Self.TestData.DataString, Self.ReceivedData, 'Data not sent');
end;

//******************************************************************************
//* TestTIdSdpTcpServerConnection                                              *
//******************************************************************************
//* TestTIdSdpTcpServerConnection Public methods *******************************

procedure TestTIdSdpTcpServerConnection.SetUp;
begin
  inherited SetUp;

  Self.Connected := false;
  Self.LocalIP   := '127.0.0.1';
  Self.LocalPort := 8000;

  Self.Timer := TIdThreadedTimerQueue.Create(false);

  Self.Client := TIdTcpClient.Create(nil);
  Self.Client.Host        := Self.LocalIP;
  Self.Client.Port        := Self.LocalPort;
  Self.Client.ReadTimeout := Self.DefaultTimeout;

  Self.Connection := TIdSdpTcpServerConnection.Create;
  Self.Connection.AddDataListener(Self);
  Self.Connection.Timer := Self.Timer;
  Self.Connection.AddDataListener(Self);

  Self.TestData := TStringStream.Create('In R''lyeh dead Cthulhu lies dreaming');
end;

procedure TestTIdSdpTcpServerConnection.TearDown;
begin
  Self.Connection.RemoveDataListener(Self);

  Self.TestData.Free;
  Self.Connection.Free;
  Self.Client.Free;
  Self.Timer.Terminate;

  inherited TearDown;
end;

//* TestTIdSdpTcpServerConnection Private methods ******************************

procedure TestTIdSdpTcpServerConnection.CheckPortFree(Address: String; Port: Integer);
var
  B: TIdSocketHandle;
  S: TIdTCPServer;
begin
  S := TIdTCPServer.Create(nil);
  try
    B := S.Bindings.Add;
    B.IP   := Address;
    B.Port := Port;

    try
      S.Active := true;
    except
      Fail('Something is blocking TCP/' + Address + '/' + IntToStr(Port));
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpTcpServerConnection.CheckPortUsed(Address: String; Port: Integer);
var
  B: TIdSocketHandle;
  S: TIdTCPServer;
begin
  S := TIdTCPServer.Create(nil);
  try
    B := S.Bindings.Add;
    B.IP   := Address;
    B.Port := Port;

    try
      S.Active := true;
      Fail('Nothing is running on TCP/' + Address + '/' + IntToStr(Port));
    except
      on EIdCouldNotBindSocket do;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpTcpServerConnection.OnConnect(Connection: TIdSdpBaseTcpConnection);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSdpTcpServerConnection.OnData(Connection: TIdSdpBaseTcpConnection; Data: TStream);
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    S.CopyFrom(Data, 0);

    Self.ReceivedData := S.DataString;
  finally
    S.Free;
  end;

  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSdpTcpServerConnection.OnDisconnect(Connection: TIdSdpBaseTcpConnection);
begin
  Self.ThreadEvent.SetEvent;
end;

procedure TestTIdSdpTcpServerConnection.OnException(Connection: TIdSdpBaseTcpConnection;
                                                    ExceptionType: ExceptClass;
                                                    ExceptionMessage: String);
begin
end;

//* TestTIdSdpTcpServerConnection Published methods ****************************

procedure TestTIdSdpTcpServerConnection.TestAddressAndPort;
begin
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);
  Self.Client.Connect(Self.DefaultTimeout);
  Self.WaitForSignaled('No connection');

  CheckEquals(Self.LocalIP,   Self.Connection.Address, 'Address');
  CheckEquals(Self.LocalPort, Self.Connection.Port,    'Port');
end;

procedure TestTIdSdpTcpServerConnection.TestAddressAndPortWithNoConnection;
begin
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);

  CheckEquals(Self.LocalIP,   Self.Connection.Address, 'Address');
  CheckEquals(Self.LocalPort, Self.Connection.Port,    'Port');
end;

procedure TestTIdSdpTcpServerConnection.TestAddressAndPortWhenNotListening;
begin
  CheckEquals(IPv4ZeroAddress, Self.Connection.Address, 'Address');
  CheckEquals(TcpDiscardPort , Self.Connection.Port,    'Port');
end;

procedure TestTIdSdpTcpServerConnection.TestConnectionNotifiesListeners;
begin
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);
  Self.Client.Connect(Self.DefaultTimeout);
  Self.WaitForSignaled('No connection');
end;

procedure TestTIdSdpTcpServerConnection.TestDisconnect;
begin
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);
  Self.Client.Connect(Self.DefaultTimeout);
  Self.WaitForSignaled('No connection');

  Self.Connection.Disconnect;

  Check(not Self.Connection.IsActive,    'Disconnecting means the connection stop listening');
  Check(not Self.Connection.IsConnected, 'Connection still connected');
  CheckPortFree(Self.LocalIP, Self.LocalPort);
end;

procedure TestTIdSdpTcpServerConnection.TestLocalDisconnectionNotifiesListeners;
begin
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);
  Self.Client.Connect(Self.DefaultTimeout);
  Self.WaitForSignaled('No connection');

  Self.ThreadEvent.ResetEvent;
  Self.Connection.Disconnect;

  Self.WaitForSignaled('No disconnection notification');
end;

procedure TestTIdSdpTcpServerConnection.TestListenOn;
begin
  Check(not Self.Connection.IsActive,    'Connection marked as active before listening');
  Check(not Self.Connection.IsConnected, 'Connection marked as connected before listening');
  CheckPortFree(Self.LocalIP, Self.LocalPort);
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);
  CheckPortUsed(Self.LocalIP, Self.LocalPort);

  Check(Self.Connection.IsActive,        'Connection not marked as active after listening');
  Check(not Self.Connection.IsConnected, 'Connection marked as connected before a connect');

  Self.ThreadEvent.ResetEvent;
  Self.Client.Connect(Self.DefaultTimeout);
  Self.WaitForSignaled('No connection notification');

  Check(Self.Connection.IsActive,    'Connection not marked as active after a connect');
  Check(Self.Connection.IsConnected, 'Connection not marked as connected after a connect');
end;

procedure TestTIdSdpTcpServerConnection.TestPeerAddressAndPort;
begin
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);
  Self.Client.Connect(Self.DefaultTimeout);
  Self.WaitForSignaled('No connection');

  CheckEquals(Self.Client.Socket.Binding.IP,   Self.Connection.PeerAddress, 'PeerAddress');
  CheckEquals(Self.Client.Socket.Binding.Port, Self.Connection.PeerPort,    'PeerPort');
end;

procedure TestTIdSdpTcpServerConnection.TestPeerAddressAndPortNoConnection;
begin
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);

  CheckEquals(IPv4ZeroAddress, Self.Connection.PeerAddress, 'PeerAddress');
  CheckEquals(TcpDiscardPort,  Self.Connection.PeerPort,    'PeerPort');
end;

procedure TestTIdSdpTcpServerConnection.TestReceiveData;
begin
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);
  Self.Client.Connect(Self.DefaultTimeout);
  Self.WaitForSignaled('No connection');

  Self.ThreadEvent.ResetEvent;
  Self.Client.WriteStream(Self.TestData);

  Self.WaitForSignaled('No data arrived');
end;

procedure TestTIdSdpTcpServerConnection.TestRemoteDisconnectionNotifiesListeners;
begin
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);
  Self.Client.Connect(Self.DefaultTimeout);
  Self.WaitForSignaled('No connection');

  Self.ThreadEvent.ResetEvent;
  Self.Client.Disconnect;

  Self.WaitForSignaled('No disconnection');
end;

procedure TestTIdSdpTcpServerConnection.TestSendData;
var
  Received: String;
begin
  Self.Connection.ListenOn(Self.LocalIP, Self.LocalPort);
  Self.Client.Connect(Self.DefaultTimeout);

  Self.Connection.SendData(Self.TestData);

  try
    Received := Self.Client.ReadString(Length(Self.TestData.DataString));
  except
    on EIdConnClosedGracefully do;
  end;

  CheckEquals(Self.TestData.DataString, Received, 'Data not sent');
end;

//******************************************************************************
//* TestTIdSdpTcpNullConnection                                                *
//******************************************************************************
//* TestTIdSdpTcpNullConnection Public methods *********************************

procedure TestTIdSdpTcpNullConnection.SetUp;
begin
  inherited SetUp;

  Self.Connection := TIdSdpTcpNullConnection.Create;
end;

procedure TestTIdSdpTcpNullConnection.TearDown;
begin
  Self.Connection.Free;

  inherited TearDown;
end;

//* TestTIdSdpTcpNullConnection Published methods ******************************

procedure TestTIdSdpTcpNullConnection.TestAddressAndPort;
begin
  CheckEquals(IPv4ZeroAddress, Self.Connection.Address, 'Address');
  CheckEquals(TcpDiscardPort,  Self.Connection.Port,    'Port');
end;

procedure TestTIdSdpTcpNullConnection.TestPeerAddressAndPort;
begin
  CheckEquals(IPv4ZeroAddress, Self.Connection.PeerAddress, 'PeerAddress');
  CheckEquals(TcpDiscardPort,  Self.Connection.PeerPort,    'PeerPort');
end;

//******************************************************************************
//* TestTIdSdpMockTcpConnection                                                *
//******************************************************************************
//* TestTIdSdpMockTcpConnection Public methods *********************************

procedure TestTIdSdpMockTcpConnection.SetUp;
begin
  inherited SetUp;

  Self.Conn       := Self.CreateConnection;
  Self.Data       := TStringStream.Create('');
  Self.ReceivedOn := TIdConnectionBindings.Create;
end;

procedure TestTIdSdpMockTcpConnection.TearDown;
begin
  Self.ReceivedOn.Free;
  Self.Data.Free;
  Self.Conn.Free;

  inherited TearDown;
end;

//* TestTIdSdpMockTcpConnection Protected methods ******************************

procedure TestTIdSdpMockTcpConnection.Activate(Connection: TIdSdpMockTcpConnection);
begin
  Fail(Self.ClassName + ' MUST override Activate');
end;

function TestTIdSdpMockTcpConnection.CreateConnection: TIdSdpMockTcpConnection;
begin
  Result := nil;
  Fail(Self.ClassName + ' MUST override CreateConnection');
end;

//* TestTIdSdpMockTcpConnection Published methods ******************************

procedure TestTIdSdpMockTcpConnection.TestAddDataListener;
var
  L1, L2: TIdSdpTestConnectionListener;
begin
  L1 := TIdSdpTestConnectionListener.Create;
  try
    L2 := TIdSdpTestConnectionListener.Create;
    try
      Self.Conn.AddDataListener(L1);
      Self.Conn.AddDataListener(L2);

      Self.Activate(Self.Conn);
      Self.Conn.ReceiveData(Self.Data, Self.ReceivedOn);

      Check(L1.OnDataCalled, 'L1 not notified');
      Check(L2.OnDataCalled, 'L2 not notified');

      Check(Self.Conn = L1.ConnectionParam, 'Connection parameter');
      CheckEquals(Self.Data.DataString, StreamToStr(L1.DataParam), 'Data parameter');
    finally
      Self.Conn.RemoveDataListener(L2);
      L2.Free;
    end;
  finally
    Self.Conn.RemoveDataListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSdpMockTcpConnection.TestDisconnect;
begin
  Fail(Self.ClassName + ' MUST override TestDisconnect');
end;

procedure TestTIdSdpMockTcpConnection.TestForceDisconnect;
begin
  Fail(Self.ClassName + ' MUST override TestForceDisconnect');
end;

procedure TestTIdSdpMockTcpConnection.TestRemotePartyAccepts;
begin
  Check(not Self.Conn.IsConnected,
        Self.ClassName + ': New connection can''t be connected');

  Self.Activate(Self.Conn);
  Check(not Self.Conn.IsConnected,
        Self.ClassName + ': Remote party has yet to establish their end');

  Self.Conn.RemotePartyAccepts;
  Check(Self.Conn.IsConnected,
        Self.ClassName + ': Remote party established their end, but no connection');
end;

procedure TestTIdSdpMockTcpConnection.TestRemoveDataListener;
var
  L1, L2: TIdSdpTestConnectionListener;
begin
  L1 := TIdSdpTestConnectionListener.Create;
  try
    L2 := TIdSdpTestConnectionListener.Create;
    try
      Self.Conn.AddDataListener(L1);
      Self.Conn.AddDataListener(L2);
      Self.Conn.RemoveDataListener(L2);

      Self.Activate(Self.Conn);
      Self.Conn.ReceiveData(Self.Data, Self.ReceivedOn);

      Check(    L1.OnDataCalled, Self.ClassName + ': L1 not notified');
      Check(not L2.OnDataCalled, Self.ClassName + ': L2 notified, hence not removed');
    finally
      L2.Free;
    end;
  finally
    Self.Conn.RemoveDataListener(L1);
    L1.Free;
  end;
end;

procedure TestTIdSdpMockTcpConnection.TestSendData;
var
  Data: TStringStream;
begin
  CheckEquals('', Self.Conn.SentData,
              Self.ClassName + ': New connection can''t have sent data');

  Data := TStringStream.Create('foo');
  try
    Self.Conn.SendData(Data);

    CheckEquals('', Self.Conn.SentData,
                Self.ClassName + ': Cannot send data when not active');

    Self.Activate(Self.Conn);
    CheckEquals('', Self.Conn.SentData,
                Self.ClassName + ': Cannot send data when not connected');

    Self.Conn.RemotePartyAccepts;

    Self.Conn.SendData(Data);
    CheckEquals(Data.DataString, Self.Conn.SentData,
                Self.ClassName + ': Data not sent');

    Self.Conn.SendData(Data);
    CheckEquals(Data.DataString + Data.DataString, Self.Conn.SentData, 'Sent data not appended');
  finally
    Data.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpTcpMediaStream                                                   *
//******************************************************************************
//* TestTIdSdpTcpMediaStream Public methods ************************************

procedure TestTIdSdpTcpMediaStream.Setup;
var
  Nihongo: Widestring;
begin
  // The TCP transport for RTP (RFC 4145) uses MIME types as formats.
  Self.DataFormat := 'text/plain;charset=UTF-8';

  inherited SetUp;

  // The Nihongo variable points to a string containing the kanji of what,
  // romanised, would be "nihongo".
  Nihongo := WideChar($65E5);
  Nihongo := Nihongo + WideChar($672C) + WideChar($8A9E);

  Self.ReceivingBinding := TIdConnectionBindings.Create;

  Self.Text := TStringStream.Create(UTF16LEToUTF8(Nihongo));
end;

procedure TestTIdSdpTcpMediaStream.TearDown;
begin
  Self.Text.Free;

  inherited TearDown;
end;

procedure TestTIdSdpTcpMediaStream.CheckPortActive(Address: String;
                                                   Port: Cardinal;
                                                   Msg: String);
var
  C:           TIdSdpBaseTcpConnection;
  Connections: TStringList;
  Found:       Boolean;
  I:           Integer;
begin
  Connections := TStringList.Create;
  try
    TIdObjectRegistry.CollectAllObjectsOfClass(TIdSdpBaseTcpConnection, Connections);

    Found := false;
    for I := 0 to Connections.Count - 1 do begin
      C := TIdSdpBaseTcpConnection(Connections.Objects[I]);

      Found := (C.Address = Address) and (C.Port = Port);
      if Found then Break;
    end;
  finally
    Connections.Free;
  end;

  Check(Found, Format(Msg + ': %s:%d', [Address, Port]));
end;

procedure TestTIdSdpTcpMediaStream.CheckPortFree(Address: String;
                                                 Port: Cardinal;
                                                 Msg: String);
var
  C:           TIdSdpBaseTcpConnection;
  Connections: TStringList;
  Found:       Boolean;
  I:           Integer;
begin
  Connections := TStringList.Create;
  try
    TIdObjectRegistry.CollectAllObjectsOfClass(TIdSdpBaseTcpConnection, Connections);

    Found := false;
    for I := 0 to Connections.Count - 1 do begin
      C := TIdSdpBaseTcpConnection(Connections.Objects[I]);

      Found := (C.Address = Address) and (C.Port = Port);
      if Found then Break;
    end;
  finally
    Connections.Free;
  end;

  Check(not Found, Format(Msg + ': %s:%d', [Address, Port]));
end;

//* TestTIdSdpTcpMediaStream Protected methods *********************************

procedure TestTIdSdpTcpMediaStream.Activate(Stream: TIdSdpBaseMediaStream);
var
  S: TIdSdpTcpMediaStream;
begin
  S := Stream as TIdSdpTcpMediaStream;

  if S.IsActiveConnection then
    Self.FindMockClient(S.RemoteDescription).RemotePartyAccepts
  else
    Self.FindMockServer(S.LocalDescription).RemotePartyAccepts
end;

function TestTIdSdpTcpMediaStream.BasicMediaDesc(Port: Cardinal = 8000; PortCount: Cardinal = 1): String;
begin
  if (PortCount > 1) then
    Result := Format('m=audio %d/%d TCP %s'#13#10, [Port, PortCount, Self.DataFormat])
  else
    Result := Format('m=audio %d TCP %s'#13#10, [Port, Self.DataFormat]);
end;

function TestTIdSdpTcpMediaStream.CreateStream: TIdSdpBaseMediaStream;
begin
  Result := TIdSdpTcpMediaStream.Create(TIdSdpMockTcpConnection);
  Result.IsOffer           := true;
  Result.LocalDescription  := Self.LocalDescription;
  Result.RemoteDescription := Self.RemoteDescription;
  Result.Timer             := Self.Timer;
end;

procedure TestTIdSdpTcpMediaStream.ReceiveDataOn(S: TIdSdpBaseMediaStream);
var
  Conn: TIdSdpMockTcpConnection;
begin
  if TIdSdpTcpMediaStream(S).IsActiveConnection then
    Conn := Self.FindMockClient(S.RemoteDescription)
  else
    Conn := Self.FindMockServer(S.LocalDescription);

  Self.ReceivingBinding.LocalIP   := Conn.Address;
  Self.ReceivingBinding.LocalPort := Conn.Port;
  Self.ReceivingBinding.PeerIP    := Conn.PeerAddress;
  Self.ReceivingBinding.PeerPort  := Conn.PeerPort;
  Conn.ReceiveData(Self.Text, Self.ReceivingBinding);
end;

procedure TestTIdSdpTcpMediaStream.SendData(Stream: TIdSdpBaseMediaStream);
begin
  Stream.SendData(Self.Text, Self.DataFormat);
  Self.Timer.TriggerAllEventsOfType(TIdSdpTcpSendDataWait);
end;

//* TestTIdSdpTcpMediaStream Protected methods *********************************

function TestTIdSdpTcpMediaStream.ActiveMediaDesc(Port: Cardinal = 8000;
                                                  PortCount: Cardinal = 1): String;
begin
  Result := Self.SetupMediaDesc(stActive, Port, PortCount);
end;

function TestTIdSdpTcpMediaStream.ActPassMediaDesc(Port: Cardinal = 8000): String;
begin
  Result := Self.SetupMediaDesc(stActPass, Port);
end;

procedure TestTIdSdpTcpMediaStream.CheckAtLeastOneNullConnectionCreated;
var
  Connections: TStrings;
begin
  Connections := TStringList.Create;
  try
    TIdObjectRegistry.CollectAllObjectsOfClass(TIdSdpMockTcpNullConnection, Connections, false);

    Check(Connections.Count > 0, 'No null connections created');
  finally
    Connections.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.CheckClientConnectionFormed(WhenOffer: Boolean; Offer, Answer: String);
begin
  CheckClientConnectionFormedEx(WhenOffer, true,  Offer, Answer);
  CheckClientConnectionFormedEx(WhenOffer, false, Offer, Answer);
end;

procedure TestTIdSdpTcpMediaStream.CheckClientConnectionFormedEx(WhenOffer, SetLocalDescFirst: Boolean; Offer, Answer: String);
var
  Connection: TIdSdpMockTcpConnection;
  Stream:     TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Self.SetMediaDescriptions(Stream, WhenOffer, SetLocalDescFirst, Offer, Answer);

    Stream.StartListening;

    Connection := Self.FindSoleMockClient;
    Check(Connection.ConnectToCalled, 'Connection not connecting');
    Check(Connection.IsActive,        'Connection not active');
    Check(not Connection.IsConnected, 'Connection can''t yet be connected');

    Connection.RemotePartyAccepts;
    Check(Connection.IsConnected, 'Connection not connected');
    CheckEquals(Stream.RemoteDescription.Connections[0].Address, Connection.PeerAddress, 'Connected to wrong address');
    CheckEquals(Stream.RemoteDescription.Port,                   Connection.PeerPort,    'Connected to wrong port');
    CheckEquals(Stream.LocalDescription.Connections[0].Address,  Connection.Address,     'Connected from wrong address');
    CheckNotEquals(0,                                            Connection.Port,        'Connected from invalid port');

    // cf. RFC 4145, section 4.1 (page 4, second half of first paragraph)
    CheckEquals(TcpDiscardPort, Stream.LocalDescription.Port, 'Local port');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.CheckNullConnectionFormed(WhenOffer: Boolean; Offer, Answer: String);
begin
  CheckNullConnectionFormedEx(WhenOffer, true,  Offer, Answer);
  CheckNullConnectionFormedEx(WhenOffer, false, Offer, Answer);
end;

procedure TestTIdSdpTcpMediaStream.CheckNullConnectionFormedEx(WhenOffer, SetLocalDescFirst: Boolean; Offer, Answer: String);
var
  Stream: TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Self.SetMediaDescriptions(Stream, WhenOffer, SetLocalDescFirst, Offer, Answer);

    Stream.StartListening;

    CheckAtLeastOneNullConnectionCreated;
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.CheckNullConnectionFormedForInvalidResponse(WhenOffer: Boolean; Offer, Answer: String);
begin
  CheckNullConnectionFormedForInvalidResponseEx(WhenOffer, true,  Offer, Answer);
  CheckNullConnectionFormedForInvalidResponseEx(WhenOffer, false, Offer, Answer);
end;

procedure TestTIdSdpTcpMediaStream.CheckNullConnectionFormedForInvalidResponseEx(WhenOffer, SetLocalDescFirst: Boolean; Offer, Answer: String);
begin
  // This test shows we behave well when we send an inappropriate reply.

  CheckNullConnectionFormedEx(WhenOffer, SetLocalDescFirst, Offer, Answer);

  // Add possible other stuff here.
end;

procedure TestTIdSdpTcpMediaStream.CheckServerConnectionFormed(WhenOffer: Boolean; Offer, Answer: String);
begin
  CheckServerConnectionFormedEx(WhenOffer, true,  Offer, Answer);
  CheckServerConnectionFormedEx(WhenOffer, false, Offer, Answer);
end;

procedure TestTIdSdpTcpMediaStream.CheckServerConnectionFormedEx(WhenOffer, SetLocalDescFirst: Boolean; Offer, Answer: String);
var
  Connection: TIdSdpMockTcpConnection;
  Stream:     TIdSdpBaseMediaStream;
begin
  Stream := Self.CreateStream;
  try
    Self.SetMediaDescriptions(Stream, WhenOffer, SetLocalDescFirst, Offer, Answer);

    Stream.StartListening;

    Connection := Self.FindSoleMockServer;
    Check(Connection.ListenOnCalled,  'Connection not listening');
    Check(Connection.IsActive,        'Connection not active');
    Check(not Connection.IsConnected, 'Connection can''t yet be connected');
  finally
    Stream.Free;
  end;
end;

function TestTIdSdpTcpMediaStream.FindMockClient(Desc: TIdSdpMediaDescription): TIdSdpMockTcpConnection;
var
  Address: String;
  Port:    Cardinal;
  S:       TIdSdpBaseTcpConnection;
begin
  Address := Desc.Connections[0].Address;
  Port    := Desc.Port;
  S := TIdSdpTcpConnectionRegistry.ClientConnectedTo(Address, Port);

  Check(Assigned(S), Format('FindMockClient: No client found connected to %s:%d', [Address, Port]));
  CheckEquals(TIdSdpMockTcpClientConnection, S.ClassType, 'FindMockClient: Client of the incorrect type');

  Result := S as TIdSdpMockTcpConnection;
end;

function TestTIdSdpTcpMediaStream.FindMockServer(Desc: TIdSdpMediaDescription): TIdSdpMockTcpConnection;
var
  Address: String;
  Port:    Cardinal;
  S:       TIdSdpBaseTcpConnection;
begin
  Address := Desc.Connections[0].Address;
  Port    := Desc.Port;
  S := TIdSdpTcpConnectionRegistry.ServerOn(Address, Port);

  Check(Assigned(S), Format('FindMockServer: No server found on %s:%d', [Address, Port]));
  CheckEquals(TIdSdpMockTcpServerConnection, S.ClassType, 'FindMockServer: Server of the incorrect type');

  Result := S as TIdSdpMockTcpConnection;
end;

function TestTIdSdpTcpMediaStream.FindSoleMockClient: TIdSdpMockTcpConnection;
var
  Connections: TStrings;
begin
  Connections := TStringList.Create;
  try
    TIdObjectRegistry.CollectAllObjectsOfClass(TIdSdpMockTcpClientConnection, Connections, false);

    CheckEquals(1, Connections.Count, 'Unexpected number of TIdSdpMockTcpConnections');

    Result := Connections.Objects[0] as TIdSdpMockTcpConnection;
  finally
    Connections.Free;
  end;
end;

function TestTIdSdpTcpMediaStream.FindSoleMockServer: TIdSdpMockTcpConnection;
var
  Connections: TStrings;
begin
  Connections := TStringList.Create;
  try
    TIdObjectRegistry.CollectAllObjectsOfClass(TIdSdpMockTcpServerConnection, Connections, false);

    CheckEquals(1, Connections.Count, 'Unexpected number of TIdSdpMockTcpConnections');

    Result := Connections.Objects[0] as TIdSdpMockTcpConnection;
  finally
    Connections.Free;
  end;
end;

function TestTIdSdpTcpMediaStream.HoldConnMediaDesc(Port: Cardinal = 8000): String;
begin
  Result := Self.SetupMediaDesc(stHoldConn, Port);
end;

function TestTIdSdpTcpMediaStream.PassiveMediaDesc(Port: Cardinal = 8000;
                                                   PortCount: Cardinal = 1): String;
begin
  Result := Self.SetupMediaDesc(stPassive, Port, PortCount);
end;

procedure TestTIdSdpTcpMediaStream.SetMediaDescriptions(S: TIdSdpBaseMediaStream; IsOffer, SetLocalDescFirst: Boolean; Offer, Answer: String);
begin
  // The SetLocalDescFirst flag ensures that we test that the stream works 
  // correctly regardless of which description you set first.

  S.IsOffer := IsOffer;

  if S.IsOffer then begin
    if SetLocalDescFirst then begin
      Self.SetLocalMediaDesc(S,  Offer);
      Self.SetRemoteMediaDesc(S, Answer);
    end
    else begin
      Self.SetRemoteMediaDesc(S, Answer);
      Self.SetLocalMediaDesc(S,  Offer);
    end;
  end
  else begin
    if SetLocalDescFirst then begin
      Self.SetLocalMediaDesc(S,  Answer);
      Self.SetRemoteMediaDesc(S, Offer);
    end
    else begin
      Self.SetRemoteMediaDesc(S, Offer);
      Self.SetLocalMediaDesc(S,  Answer);
    end;
  end;
end;

function TestTIdSdpTcpMediaStream.SetupMediaDesc(SetupType: TIdSdpSetupType;
                                                 Port: Cardinal = 8000;
                                                 PortCount: Cardinal = 1): String;
begin
  Result := Self.BasicMediaDesc(Port, PortCount)
          + Format('a=setup:%s'#13#10, [SetupTypeToStr(SetupType)]);
end;

function TestTIdSdpTcpMediaStream.UnknownMediaDesc(Port: Cardinal = 8000): String;
begin
  Result := Self.BasicMediaDesc(Port)
          + 'a=setup:unknown'#13#10;
end;

//* TestTIdSdpTcpMediaStream Published methods *********************************

procedure TestTIdSdpTcpMediaStream.TestPortsAndPortCount;
const
  PortCount = 5;
var
  I:      Integer;
  Stream: TIdSdpTcpMediaStream;
begin
  Stream := Self.CreateStream as TIdSdpTcpMediaStream;
  try
    // We use passive setup here because Ports tells us the ACTUAL ports used,
    // which means sockets have to be bound. We could also use active, and then
    // connect to a server.
    Self.SetLocalMediaDesc(Stream,
                           'm=audio 9000/' + IntToStr(PortCount) + ' TCP audio/x-hierarchically-encoded'#13#10
                         + 'a=setup:passive'#13#10);
    Stream.StartListening;

    CheckEquals(5, Stream.PortCount, 'Port count');

    for I := 0 to PortCount - 1 do
     CheckEquals(Stream.LocalDescription.Port + Cardinal(I),
                 Stream.Ports[I],
                 Format('Ports[%d]', [I]));
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestReceivedDataFormat;
var
  L:      TIdSdpTestMediaListener;
  Stream: TIdSdpTcpMediaStream;
begin
  L := TIdSdpTestMediaListener.Create;
  try
    Stream := Self.CreateStream as TIdSdpTcpMediaStream;
    try
      Stream.AddDataListener(L);
      Self.ReceiveDataOn(Stream);

      CheckEquals(Self.LocalDescription.Formats[0], L.FormatParam,
                  'Format not set');
    finally
      Stream.RemoveDataListener(L);
      Stream.Free;
    end;
  finally
    L.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestReceivedDataFormatMultipleFormats;
const
  SecondFormat = 'text/html';
var
  FirstFormat: String;
  L:           TIdSdpTestMediaListener;
  Stream:      TIdSdpTcpMediaStream;
begin
  // I can't think of any general way of TIdSdpTcpMediaStream being able to
  // determine the format of data it receives should the stream allow multiple
  // formats. Thus, should the stream declare multiple formats, the stream uses
  // just the first format.

  L := TIdSdpTestMediaListener.Create;
  try
    Stream := Self.CreateStream as TIdSdpTcpMediaStream;
    try
      Stream.AddDataListener(L);
      FirstFormat := Stream.LocalDescription.Formats[0];
      Stream.LocalDescription.AddFormat(SecondFormat);

      Self.ReceiveDataOn(Stream);

      CheckEquals(Self.LocalDescription.Formats[0], L.FormatParam,
                  'Format not set');
    finally
      Stream.RemoveDataListener(L);
      Stream.Free;
    end;
  finally
    L.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestSetRemoteDescriptionDoesntTerminateExistingConnections;
var
  L:      TIdSdpTestConnectionListener;
  Server: TIdSdpMockTcpConnection;
  S:      TIdSdpTcpMediaStream;
begin
  S := TIdSdpTcpMediaStream.Create(TIdSdpMockTcpConnection);
  try
    S.IsOffer := true;
    S.Timer   := Self.Timer;
    Self.SetLocalMediaDesc(S, Self.PassiveMediaDesc);
    S.StartListening;

    Server := Self.FindMockServer(S.LocalDescription);
    L := TIdSdpTestConnectionListener.Create;
    try
      Server.AddDataListener(L);
      Self.SetRemoteMediaDesc(S, Self.ActiveMediaDesc);
      Check(Server = Self.FindMockServer(S.LocalDescription),
            'Server recreated - which could disconnect remote party''s connections');
      Check(not L.OnDisconnectCalled, 'Connection disconnected');

      Server.RemotePartyAccepts;
      Self.SetRemoteMediaDesc(S, Self.ActiveMediaDesc);
      Check(not L.OnDisconnectCalled, 'Connection disconnected after client connected');
    finally
      L.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestSetTimer;
var
  NewTimer: TIdDebugTimerQueue;
  NullData: TStream;
  OldTimer: TIdDebugTimerQueue;
  Stream:   TIdSdpTcpMediaStream;
begin
  Stream := Self.CreateStream as TIdSdpTcpMediaStream;
  try
    OldTimer := Stream.Timer as TIdDebugTimerQueue;

    NewTimer := TIdDebugTimerQueue.Create(false);
    try
      Stream.Timer := NewTimer;

      NullData := TMemoryStream.Create;
      try
        Stream.SendData(NullData, '', 0);
      finally
        NullData.Free;
      end;

      Check(nil =  OldTimer.LastEventScheduled(TIdSdpTcpSendDataWait), 'Connection used the old timer');
      Check(nil <> NewTimer.LastEventScheduled(TIdSdpTcpSendDataWait), 'Connection didn''t use the new timer');
    finally
      Stream.Timer := OldTimer;
      NewTimer.Terminate;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestStartListeningPortAboveAllowedRange;
const
  LowerLimit  = 15000;
  HigherLimit = 16000;
  TooHighPort = HigherLimit + 1000;
var
  Stream: TIdSdpTcpMediaStream;
begin
  CheckPortFree('127.0.0.1', LowerLimit, 'Required port is not free');

  Stream := Self.CreateStream as TIdSdpTcpMediaStream;
  try
    Stream.LowestAllowedPort  := LowerLimit;
    Stream.HighestAllowedPort := HigherLimit;

    Stream.IsOffer := true;
    Self.SetLocalMediaDesc(Stream, Self.PassiveMediaDesc(TooHighPort));

    Stream.StartListening;

    CheckPortAdjusted(LowerLimit, Stream.LocalDescription.Port, Stream.IsNull, Stream.ClassName);
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestStartListeningPortBelowAllowedRange;
const
  LowerLimit  = 15000;
  HigherLimit = 16000;
  TooLowPort = HigherLimit - 1000;
var
  Stream: TIdSdpTcpMediaStream;
begin
  CheckPortFree('127.0.0.1', LowerLimit, 'Required port is not free');

  Stream := Self.CreateStream as TIdSdpTcpMediaStream;
  try
    Stream.LowestAllowedPort  := LowerLimit;
    Stream.HighestAllowedPort := HigherLimit;

    Stream.IsOffer := true;
    Self.SetLocalMediaDesc(Stream, Self.PassiveMediaDesc(TooLowPort));

    Stream.StartListening;

    CheckPortAdjusted(LowerLimit, Stream.LocalDescription.Port, Stream.IsNull, Stream.ClassName);
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestStopListening;
var
  L:      TIdSdpTestMediaListener;
  Stream: TIdSdpTcpMediaStream;
begin
  Stream := Self.CreateStream as TIdSdpTcpMediaStream;
  try
    Stream.StartListening;

    Stream.StopListening;

    L := TIdSdpTestMediaListener.Create;
    try
      Stream.AddDataListener(L);
      Self.ReceiveDataOn(Stream);
      Check(not L.ReceivedData, 'Server didn''t stop listening');
    finally
      Stream.RemoveDataListener(L);
      L.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestUnusedPortsSwitchOff;
var
  Address: String;
  Port:    Cardinal;
  Stream:  TIdSdpTcpMediaStream;
begin
  Address := Localhost(Id_IPv4);
  Port    := 8000;

  Stream := Self.CreateStream as TIdSdpTcpMediaStream;
  try
    Stream.IsOffer := true;
    Self.SetLocalMediaDesc(Stream,
                           Self.PassiveMediaDesc(Port));
    Stream.StartListening;
    CheckPortActive(Address, Port, 'TCP port not listening');

    Self.SetRemoteMediaDesc(Stream,
                            Self.RefusedMediaDesc);
    CheckPortFree(Address, Port, 'Unused TCP port not free');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActiveSendActive;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.ActiveMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActiveSendActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.ActiveMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActiveSendBlank;
begin
  CheckServerConnectionFormed(false, Self.ActiveMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActiveSendHoldConn;
begin
  CheckNullConnectionFormed(false, Self.ActiveMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActiveSendPassive;
begin
  CheckServerConnectionFormed(false, Self.ActiveMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActiveSendUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.ActiveMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActPassSendActive;
begin
  CheckClientConnectionFormed(false, Self.ActPassMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActPassSendActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.ActPassMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActPassSendBlank;
begin
  CheckServerConnectionFormed(false, Self.ActPassMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActPassSendHoldConn;
begin
  CheckNullConnectionFormed(false, Self.ActPassMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActPassSendPassive;
begin
  CheckServerConnectionFormed(false, Self.ActPassMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveActPassSendUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.ActPassMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveBlankSendActive;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.BasicMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveBlankSendActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.BasicMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveBlankSendBlank;
begin
  CheckServerConnectionFormed(false, Self.BasicMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveBlankSendHoldConn;
begin
  CheckNullConnectionFormed(false, Self.BasicMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveBlankSendPassive;
begin
  CheckServerConnectionFormed(false, Self.BasicMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveBlankSendUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.BasicMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveHoldConnSendActive;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.HoldConnMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveHoldConnSendActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.HoldConnMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveHoldConnSendBlank;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.HoldConnMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveHoldConnSendHoldConn;
begin
  CheckNullConnectionFormed(false, Self.HoldConnMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveHoldConnSendPassive;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.HoldConnMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveHoldConnSendUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.HoldConnMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceivePassiveSendActive;
begin
  CheckClientConnectionFormed(false, Self.PassiveMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceivePassiveSendActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.PassiveMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceivePassiveSendBlank;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.PassiveMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceivePassiveSendHoldConn;
begin
  CheckNullConnectionFormed(false, Self.PassiveMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceivePassiveSendPassive;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.PassiveMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceivePassiveSendUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.PassiveMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveUnknownSendActive;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.UnknownMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveUnknownSendActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.UnknownMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveUnknownSendBlank;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.UnknownMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveUnknownSendHoldConn;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.UnknownMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveUnknownSendPassive;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.UnknownMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenAnswerReceiveUnknownSendUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(false, Self.UnknownMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferActPassStartsListening;
var
  Address: String;
  Port:    Cardinal;
  Stream:  TIdSdpTcpMediaStream;
begin
  Address := Localhost(Id_IPv4);
  Port    := 8000;

  Stream := TIdSdpTcpMediaStream.Create(TIdSdpMockTcpConnection);
  try
    Stream.IsOffer := true;
    Self.SetLocalMediaDesc(Stream,
                           Self.ActPassMediaDesc(Port));
    Stream.StartListening;

    CheckPortActive(Address, Port, 'Actpass connection not listening');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferActPassStopsListeningWhenAnswerHoldConn;
var
  Address: String;
  Port:    Cardinal;
  Stream:  TIdSdpTcpMediaStream;
begin
  Address := Localhost(Id_IPv4);
  Port    := 8000;

  Stream := TIdSdpTcpMediaStream.Create(TIdSdpMockTcpConnection);
  try
    Stream.IsOffer := true;
    Self.SetLocalMediaDesc(Stream,
                           Self.ActPassMediaDesc(Port));
    Stream.StartListening;

    CheckPortActive(Address, Port, 'Actpass connection not listening');

    Self.SetRemoteMediaDesc(Stream,
                            Self.HoldConnMediaDesc(Port + 1000));

    CheckPortFree(Address, Port, 'Actpass connection still listening rather than doing nothing');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferActPassStopsListeningWhenAnswerPassive;
var
  Address: String;
  Port:    Cardinal;
  Stream:  TIdSdpTcpMediaStream;
begin
  Address := Localhost(Id_IPv4);
  Port    := 8000;

  Stream := TIdSdpTcpMediaStream.Create(TIdSdpMockTcpConnection);
  try
    Stream.IsOffer := true;
    Self.SetLocalMediaDesc(Stream,
                           Self.ActPassMediaDesc(Port));
    Stream.StartListening;

    CheckPortActive(Address, Port, 'Actpass connection not listening');

    Self.SetRemoteMediaDesc(Stream,
                            Self.PassiveMediaDesc(Port + 1000));

    CheckPortFree(Address, Port, 'Actpass connection still listening rather than initiating connection');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferPassiveStartsListening;
var
  Address: String;
  Port:    Cardinal;
  Stream:  TIdSdpTcpMediaStream;
begin
  Address := Localhost(Id_IPv4);
  Port    := 8000;

  Stream := TIdSdpTcpMediaStream.Create(TIdSdpMockTcpConnection);
  try
    Stream.IsOffer := true;
    Self.SetLocalMediaDesc(Stream,
                           Self.PassiveMediaDesc(Port));
    Stream.StartListening;

    CheckPortActive(Address, Port, 'Passive connection not listening');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferPassiveStopsListeningWhenAnswerHoldConn;
var
  Address: String;
  Port:    Cardinal;
  Stream:  TIdSdpTcpMediaStream;
begin
  Address := Localhost(Id_IPv4);
  Port    := 8000;

  Stream := TIdSdpTcpMediaStream.Create(TIdSdpMockTcpConnection);
  try
    Stream.IsOffer := true;
    Self.SetLocalMediaDesc(Stream,
                           Self.PassiveMediaDesc(Port));
    Stream.StartListening;

    CheckPortActive(Address, Port, 'Passive connection not listening');

    Self.SetRemoteMediaDesc(Stream,
                            Self.HoldConnMediaDesc(Port + 1000));

    CheckPortFree(Address, Port, 'Passive connection still listening rather than doing nothing');
  finally
    Stream.Free;
  end;
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActiveReceiveActive;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.ActiveMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActiveReceiveActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.ActiveMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActiveReceiveBlank;
begin
  CheckClientConnectionFormed(true, Self.ActiveMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActiveReceivePassive;
begin
  CheckClientConnectionFormed(true, Self.ActiveMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActiveReceiveHoldConn;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.ActiveMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActiveReceiveUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.ActiveMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActPassReceiveActive;
begin
  CheckServerConnectionFormed(true, Self.ActPassMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActPassReceiveActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.ActPassMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActPassReceiveBlank;
begin
  CheckClientConnectionFormed(true, Self.ActPassMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActPassReceiveHoldConn;
begin
  CheckNullConnectionFormed(true, Self.ActPassMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActPassReceivePassive;
begin
  CheckClientConnectionFormed(true, Self.ActPassMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendActPassReceiveUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.ActPassMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendBlankReceiveActive;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.BasicMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendBlankReceiveActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.BasicMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendBlankReceiveBlank;
begin
  CheckClientConnectionFormed(true, Self.BasicMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendBlankReceivePassive;
begin
  CheckClientConnectionFormed(true, Self.BasicMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendBlankReceiveHoldConn;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.BasicMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendBlankReceiveUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.BasicMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendHoldConnReceiveActive;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.HoldConnMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendHoldConnReceiveActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.HoldConnMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendHoldConnReceiveBlank;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.HoldConnMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendHoldConnReceiveHoldConn;
begin
  CheckNullConnectionFormed(true, Self.HoldConnMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendHoldConnReceivePassive;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.HoldConnMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendHoldConnReceiveUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.HoldConnMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendPassiveReceiveActive;
begin
  CheckServerConnectionFormed(true, Self.PassiveMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendPassiveReceiveActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.PassiveMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendPassiveReceiveBlank;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.PassiveMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendPassiveReceiveHoldConn;
begin
  CheckNullConnectionFormed(true, Self.PassiveMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendPassiveReceivePassive;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.PassiveMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendPassiveReceiveUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.PassiveMediaDesc, Self.UnknownMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendUnknownReceiveActive;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.UnknownMediaDesc, Self.ActiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendUnknownReceiveActPass;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.UnknownMediaDesc, Self.ActPassMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendUnknownReceiveBlank;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.UnknownMediaDesc, Self.BasicMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendUnknownReceiveHoldConn;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.UnknownMediaDesc, Self.HoldConnMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendUnknownReceivePassive;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.UnknownMediaDesc, Self.PassiveMediaDesc);
end;

procedure TestTIdSdpTcpMediaStream.TestWhenOfferSendUnknownReceiveUnknown;
begin
  CheckNullConnectionFormedForInvalidResponse(true, Self.UnknownMediaDesc, Self.UnknownMediaDesc);
end;

//******************************************************************************
//* TestTIdSDPMultimediaSession                                                *
//******************************************************************************
//* TestTIdSDPMultimediaSession Public methods *********************************

procedure TestTIdSDPMultimediaSession.SetUp;
begin
  inherited SetUp;

  Self.Profile := TIdAudioVisualProfile.Create;

  Self.MS := TIdSDPMultimediaSession.Create(Self.Profile, TIdMockRTPPeer);

  Self.PortBlocker := TIdMockRTPPeer.Create;

  // We only instantiate Server so that we know that GStack points to an
  // instantiated stack.
  Self.Server := TIdUdpServer.Create(nil);
end;

procedure TestTIdSDPMultimediaSession.TearDown;
begin
  Self.Server.Free;
  Self.PortBlocker.Free;
  Self.MS.Free;
  Self.Profile.Free;

  inherited TearDown;
end;

//* TestTIdSDPMultimediaSession Private methods ********************************

procedure TestTIdSDPMultimediaSession.CheckOrigin(ExpectedNetType: String;
                                                  ExpectedAddressType: TIdIPVersion;
                                                  ExpectedAddress: String;
                                                  ExpectedSessionDescription: String);
var
  SDP: TIdSDPPayload;
begin
  SDP := TIdSdpPayload.CreateFrom(Self.MS.LocalSessionDescription);
  try
    CheckEquals(ExpectedNetType,
                SDP.Origin.NetType,
                ExpectedAddress + ': Setting local address didn''t set the NetType');
    CheckEquals(AddressTypeToStr(ExpectedAddressType),
                AddressTypeToStr(SDP.Origin.AddressType),
                ExpectedAddress + ': Setting local address didn''t set the AddressType');
    CheckEquals(ExpectedAddress,
                SDP.Origin.Address,
                ExpectedAddress + ': Setting local address didn''t set the Address');
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.CheckSessionName(ExpectedName: String;
                                                       SessionDescription: String;
                                                       Msg: String);
var
  SDP: TIdSDPPayload;
begin
  SDP := TIdSdpPayload.CreateFrom(Self.MS.LocalSessionDescription);
  try
    CheckEquals(ExpectedName,
                SDP.SessionName,
                Msg);
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.CheckOriginUsername(ExpectedUsername: String;
                                                          SessionDescription: String);
var
  SDP: TIdSdpPayload;
begin
  SDP := TIdSdpPayload.CreateFrom(SessionDescription);
  try
    CheckEquals(ExpectedUsername, SDP.Origin.Username, 'Username not set');
  finally
    SDP.Free;
  end;
end;

function TestTIdSDPMultimediaSession.MediaDescription(Address: String; Port: Cardinal; Protocol: String = Id_SDP_RTPAVP): String;
var
  Fmt:     String;
  Attribs: String;
begin
  if (Protocol = Id_SDP_TCP) then begin
    Fmt     := 'text/plain;charset=UTF-8';
    Attribs := '';
  end
  else begin
    Fmt     := '96';
    Attribs := Format('a=rtpmap:%s t140/1000'#13#10, [Fmt]);
  end;

  Result := Format('m=text %d %s %s'#13#10, [Port, Protocol, Fmt])
          + 'c=IN IP4 127.0.0.1'#13#10
          + Attribs;
end;

function TestTIdSDPMultimediaSession.MultiStreamSDP(LowPort, HighPort: Cardinal; Protocol: String = Id_SDP_RTPAVP): String;
begin
  // One stream on loopback:LowPort; one stream on NIC:HighPort
  Result := 'v=0'#13#10
          + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
          + 's=-'#13#10
          + Self.MediaDescription('127.0.0.1', LowPort, Protocol)
          + Self.MediaDescription(GStack.LocalAddress, HighPort, Protocol);
end;

function TestTIdSDPMultimediaSession.MultiStreamSDPSameIP(LowPort, HighPort: Cardinal; Protocol: String = Id_SDP_RTPAVP): String;
begin
  // One stream on loopback:LowPort; one stream on loopback:HighPort
  Result := 'v=0'#13#10
          + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
          + 's=-'#13#10
          + Self.MediaDescription('127.0.0.1', LowPort, Protocol)
          + Self.MediaDescription('127.0.0.1', HighPort, Protocol);
end;

procedure TestTIdSDPMultimediaSession.ReceiveDataOfType(PayloadType: Cardinal);
var
  Binding: TIdConnectionBindings;
  NoData:  TIdRTPPacket;
begin
  Binding := TIdConnectionBindings.Create;
  try
    Binding.LocalIP   := '127.0.0.1';
    Binding.LocalPort := Self.LocalPort;
    Binding.PeerIP    := Binding.LocalIP;
    Binding.PeerPort  := Self.RemotePort;

    NoData := TIdRTPPacket.Create(Self.Profile);
    try
      NoData.PayloadType := PayloadType;
      TIdRTPPeerRegistry.ServerOn(Binding.LocalIP, Self.LocalPort).ReceivePacket(NoData, Binding);
    finally
      NoData.Free;
    end;
  finally
    Binding.Free;
  end;
end;

function TestTIdSDPMultimediaSession.RefusedStreamSDP(Port: Cardinal): String;
begin
  // One stream refused, one accepted.
  Result := 'v=0'#13#10
          + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
          + 's=-'#13#10
          + 'c=IN IP4 127.0.0.1'#13#10
          + 'm=text 0 RTP/AVP 0'#13#10
          + 'm=text ' + IntToStr(Port) + ' RTP/AVP 96'#13#10
          + 'a=rtpmap:96 t140/1000'#13#10
end;

function TestTIdSDPMultimediaSession.SingleStreamSDP(Port: Cardinal;
                                                     PayloadType: Cardinal = 96): String;
begin
  Result := 'v=0'#13#10
          + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
          + 's=-'#13#10
          + 'c=IN IP4 127.0.0.1'#13#10
          + 'm=text ' + IntToStr(Port) + ' RTP/AVP ' + IntToStr(PayloadType) + #13#10
          + 'a=rtpmap:' + IntToStr(PayloadType) + ' t140/1000'#13#10
end;

procedure TestTIdSDPMultimediaSession.StartServerOnPort(Port: Integer);
begin
  Self.PortBlocker.Active := false;

  Self.PortBlocker.Address := Localhost(Id_IPv4);
  Self.PortBlocker.RTPPort  := Port;
  Self.PortBlocker.RTCPPort := Self.PortBlocker.RTPPort;

  Self.PortBlocker.Active := true;

  CheckPortActive(Self.PortBlocker.Address, Self.PortBlocker.RTPPort,
                  'StartServerOnPort didn''t bind to '
                + Self.PortBlocker.Address + IntToStr(Self.PortBlocker.RTPPort));
end;

//* TestTIdSDPMultimediaSession Published methods ******************************

procedure TestTIdSDPMultimediaSession.TestAddressTypeFor;
begin
  CheckEquals(AddressTypeToStr(Id_IPv4),
              AddressTypeToStr(Self.MS.AddressTypeFor(IPv4LocalHost)),
              IPv4LocalHost);

  CheckEquals(AddressTypeToStr(Id_IPv6),
              AddressTypeToStr(Self.MS.AddressTypeFor(IPv6LocalHost)),
              IPv6LocalHost);

  Check(Id_IPUnknown = Self.MS.AddressTypeFor(''),
        'Unknown address (the empty string)');
end;

procedure TestTIdSDPMultimediaSession.TestDifferentPayloadTypesForSameEncoding;
const
  LocalPT  = 96;
  RemotePT = 98;
begin
  Self.LocalPort  := 8000;
  Self.RemotePort := 9000;

  Self.MS.StartListening(Self.SingleStreamSDP(Self.LocalPort, LocalPT));
  Self.MS.SetRemoteDescription(Self.SingleStreamSDP(Self.RemotePort, RemotePT));

  Check(Self.MS.StreamCount > 0, 'Sanity check: no ports open!');
  Self.MS.Streams[0].AddDataListener(Self);

  Self.ReceiveDataOfType(RemotePT);

  Check(Self.ReceivedData, 'No RTP data received');
end;

procedure TestTIdSDPMultimediaSession.TestIndexOfStream;
var
  I:        Integer;
  UnownedS: TIdSdpBaseMediaStream;
begin
  UnownedS := TIdSDPMediaStream.Create;
  try
    CheckEquals(-1, Self.MS.IndexOfStream(UnownedS), 'Unowned stream');
  finally
    UnownedS.Free;
  end;

  CheckEquals(-1, Self.MS.IndexOfStream(nil), 'Unowned stream (nil)');

  Self.MS.StartListening(Self.MultiStreamSDP(8000, 8010));
  for I := 0 to Self.MS.StreamCount - 1 do
    CheckEquals(I, Self.MS.IndexOfStream(Self.MS.Streams[I]), 'Stream #' + IntToStr(I));
end;

procedure TestTIdSDPMultimediaSession.TestIsListening;
begin
  Check(not Self.MS.IsListening, 'Initial IsListening must be false');

  Self.MS.StartListening(Self.SingleStreamSDP(Self.MS.LowestAllowedPort));

  Check(Self.MS.IsListening, 'IsListening after StartListening');

  Self.MS.StopListening;

  Check(not Self.MS.IsListening, 'IsListening after StopListening');
end;

procedure TestTIdSDPMultimediaSession.TestLocalSessionDescription;
var
  CurrentDesc: String;
begin
  CurrentDesc := Self.MS.LocalSessionDescription;
  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));
  CheckNotEquals(CurrentDesc,
                 Self.MS.LocalSessionDescription,
                 'After StartListening: session description not updated');

  CurrentDesc := Self.MS.LocalSessionDescription;
  Self.MS.PutOnHold;
  CheckNotEquals(CurrentDesc,
                 Self.MS.LocalSessionDescription,
                 'After PutOnHold: session description not updated');

  CurrentDesc := Self.MS.LocalSessionDescription;
  Self.MS.StopListening;
  CheckNotEquals(CurrentDesc,
                 Self.MS.LocalSessionDescription,
              'After StopListening: session description not updated');
end;

procedure TestTIdSDPMultimediaSession.TestLocalSessionDescriptionWithRefusedStream;
const
  NormalPort          = 8000;
  RefusedPortSentinel = 0;
var
  Desc: TIdSdpPayload;
  S:    String;
begin
  Self.MS.LowestAllowedPort := NormalPort;

  S := Self.MS.StartListening(Self.RefusedStreamSDP(NormalPort));
  Desc := TIdSdpPayload.CreateFrom(S);
  try
    CheckEquals(2, Desc.MediaDescriptionCount, 'Incorrect number of media descriptions');

    CheckEquals('RTP/AVP', Desc.MediaDescriptionAt(0).Protocol, 'Protocol changed: 1st desc');
    CheckEquals(RefusedPortSentinel, Desc.MediaDescriptionAt(0).Port, 'Port changed: 1st desc');

    CheckEquals('RTP/AVP', Desc.MediaDescriptionAt(1).Protocol, 'Protocol changed: 2nd desc');
    CheckEquals(NormalPort, Desc.MediaDescriptionAt(1).Port, 'Port changed: 2nd desc');
  finally
    Desc.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestLocalSessionDescriptionWithTimeHeader;
const
  TimeHeader        = 't=0 0'#13#10;
  SDPWithTimeHeader = 'v=0'#13#10
                    + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
                    + 's=-'#13#10
                    + TimeHeader
                    + 'm=text 0 RTP/AVP 0'#13#10
                    + 'c=IN IP4 127.0.0.1'#13#10;
var
  ActualDesc: TIdSdpPayload;
begin
  // RFC 3264 says that an offer SHOULD have a "t=0 0" header (section 5) and
  // that the answer MUST have a t header identical to that in the offer
  // (section 6).
  Self.MS.IsOffer := false;
  Self.MS.SetRemoteDescription(SDPWithTimeHeader);

  Self.MS.StartListening(SDPWithTimeHeader);

  ActualDesc := TIdSdpPayload.CreateFrom(Self.MS.LocalSessionDescription);
  try
    Check(ActualDesc.Times.Count > 0, 'No time header in answer');
    CheckEquals(TimeHeader, ActualDesc.Times[0].AsString, 'Wrong time header');
  finally
    ActualDesc.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestLocalSessionDescriptionWithWeirdTimeHeader;
const
  TimeHeader        = 't=1 2'#13#10;
  SDPWithTimeHeader = 'v=0'#13#10
                    + 'o=local 0 0 IN IP4 127.0.0.1'#13#10
                    + 's=-'#13#10
                    + TimeHeader
                    + 'm=text 0 RTP/AVP 0'#13#10
                    + 'c=IN IP4 127.0.0.1'#13#10;
var
  ActualDesc: TIdSdpPayload;
begin
  // The offer has a weird time header: this test demonstrates that the answer
  // complies with RFC 3264 section 6's statement that the answer's time header
  // MUST be identical to that of the offer's. 
  Self.MS.IsOffer := false;
  Self.MS.SetRemoteDescription(SDPWithTimeHeader);

  Self.MS.StartListening(SDPWithTimeHeader);

  ActualDesc := TIdSdpPayload.CreateFrom(Self.MS.LocalSessionDescription);
  try
    Check(ActualDesc.Times.Count > 0, 'No time header in answer');
    CheckEquals(TimeHeader, ActualDesc.Times[0].AsString, 'Wrong time header');
  finally
    ActualDesc.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestLocalSessionVersionIncrements;
begin
  CheckEquals(0, Self.MS.LocalSessionVersion, 'Before setting the local description');

  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));
  CheckEquals(0, Self.MS.LocalSessionVersion, 'First sess-version');

  Self.MS.StopListening;
  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));
  CheckEquals(1, Self.MS.LocalSessionVersion, 'Second sess-version');

  Self.MS.StopListening;
  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));
  CheckEquals(2, Self.MS.LocalSessionVersion, 'Third sess-version');
end;

procedure TestTIdSDPMultimediaSession.TestMimeType;
begin
  CheckEquals(SdpMimeType, Self.MS.MimeType, 'SDP MIME type');
end;

procedure TestTIdSDPMultimediaSession.TestNetTypeFor;
begin
  CheckEquals(Id_SDP_IN, Self.MS.NetTypeFor(IPv4LocalHost), 'IPv4 address (' + IPv4LocalHost + ')');
  CheckEquals(Id_SDP_IN, Self.MS.NetTypeFor(IPv6LocalHost), 'IPv6 address (' + IPv6LocalHost + ')');
  CheckEquals('UNKNOWN', Self.MS.NetTypeFor(''), 'Unknown net type (the empty string)');
end;

procedure TestTIdSDPMultimediaSession.TestOnHold;
begin
  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));
  Check(not Self.MS.OnHold, 'Session shouldn''t be on hold at first');
  Self.MS.PutOnHold;
  Check(Self.MS.OnHold, 'OnHold after PutOnHold');

  Self.MS.TakeOffHold;
  Check(not Self.MS.OnHold, 'OnHold after TakeOffHold');

  CheckEquals(2,
              Self.MS.StreamCount,
              'Sanity check: MultiStreamSDP made the wrong number of streams');
  Self.MS.PutOnHold;
  Self.MS.Streams[0].TakeOffHold;
  Check(Self.MS.OnHold, 'The Session can''t know you manually took any streams off hold');
  Self.MS.Streams[1].TakeOffHold;
  Check(Self.MS.OnHold, 'The Session can''t know you manually took all streams off hold');

  Self.MS.TakeOffHold;
  Check(not Self.MS.OnHold,
        'OnHold after TakeOffHold, having manually taken all streams off hold');
end;

procedure TestTIdSDPMultimediaSession.TestPortAboveHighestAllowedPort;
var
  Desc:        String;
  HighestPort: Cardinal;
begin
  Self.MS.LowestAllowedPort  := 8000;
  Self.MS.HighestAllowedPort := 8100;
  HighestPort := Self.MS.HighestAllowedPort + 2;
  Desc := Self.MS.StartListening(Self.SingleStreamSDP(HighestPort));

  Check(Self.MS.StreamCount > 0,
        'Not enough streams instantiated');
  CheckEquals(Self.MS.LowestAllowedPort,
              Self.MS.Streams[0].LocalDescription.Port,
              'Port value');
end;

procedure TestTIdSDPMultimediaSession.TestPortAsHighestAllowedPort;
var
  Desc:        String;
  HighestPort: Cardinal;
begin
  Self.MS.LowestAllowedPort  := 8000;
  Self.MS.HighestAllowedPort := 8100;

  // We can't start an RTP stream on port 8100, because we need to also open an
  // RTCP port on 8101, which is OUTSIDE the range. Hence, the last port we
  // could possibly try is 8099, even though that's an odd numbered port.
  HighestPort := Self.MS.HighestAllowedPort - 1;
  Desc := Self.MS.StartListening(Self.SingleStreamSDP(HighestPort));

  Check(Self.MS.StreamCount > 0,
        'Not enough streams instantiated');
  CheckEquals(HighestPort,
              Self.MS.Streams[0].LocalDescription.Port,
              'Port value');
end;

procedure TestTIdSDPMultimediaSession.TestPortAsLowestAllowedPort;
var
  Desc:        String;
  LowestPort: Cardinal;
begin
  Self.MS.LowestAllowedPort  := 8000;
  Self.MS.HighestAllowedPort := 8100;
  LowestPort := Self.MS.LowestAllowedPort;
  Desc := Self.MS.StartListening(Self.SingleStreamSDP(LowestPort));

  Check(Self.MS.StreamCount > 0,
        'Not enough streams instantiated');
  CheckEquals(LowestPort,
              Self.MS.Streams[0].LocalDescription.Port,
              'Port value');
end;

procedure TestTIdSDPMultimediaSession.TestPortBelowLowestAllowedPort;
var
  Desc:       String;
  LowestPort: Cardinal;
begin
  Self.MS.LowestAllowedPort  := 8000;
  Self.MS.HighestAllowedPort := 8100;
  LowestPort := Self.MS.LowestAllowedPort - 2;
  Desc := Self.MS.StartListening(Self.SingleStreamSDP(LowestPort));

  Check(Self.MS.StreamCount > 0,
        'Not enough streams instantiated');
  CheckEquals(Self.MS.LowestAllowedPort,
              Self.MS.Streams[0].LocalDescription.Port,
              'Port value');
end;

procedure TestTIdSDPMultimediaSession.TestPortThreePortRange;
const
  LowPort = 8000;
begin
  // This gives us three allowed ports, and we're trying to open two media
  // s0treams (which requires four ports).
  Self.MS.LowestAllowedPort  := LowPort;
  Self.MS.HighestAllowedPort := LowPort + 2;

  Self.MS.StartListening(Self.MultiStreamSDPSameIP(LowPort, LowPort + 2));

  CheckEquals(2, Self.MS.StreamCount, 'Wrong number of streams');
  CheckEquals(LowPort,
              Self.MS.Streams[0].LocalDescription.Port,
              'First stream port value');
  CheckEquals(0,
              Self.MS.Streams[1].LocalDescription.Port,
              'Second stream port value');
end;

procedure TestTIdSDPMultimediaSession.TestPutOnHold;
var
  Stream1MediaType:  TIdSdpMediaType;
  Stream2MediaType:  TIdSdpMediaType;
  OldSessionVersion: Int64;
begin
  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));
  Self.MS.Streams[0].Direction := sdRecvOnly;
  Self.MS.Streams[1].Direction := sdSendRecv;

  Stream1MediaType := Self.MS.Streams[0].LocalDescription.MediaType;
  Stream2MediaType := Self.MS.Streams[1].LocalDescription.MediaType;

  OldSessionVersion := Self.MS.LocalSessionVersion;
  Self.MS.PutOnHold;

  Check(Self.MS.Streams[0].OnHold,
        'Stream #0 not put on hold');
  Check(Self.MS.Streams[1].OnHold,
        'Stream #1 not put on hold');

  CheckEquals(MediaTypeToStr(Stream1MediaType),
              MediaTypeToStr(Self.MS.Streams[0].LocalDescription.MediaType),
              'Stream #1 changed its media type');
  CheckEquals(MediaTypeToStr(Stream2MediaType),
              MediaTypeToStr(Self.MS.Streams[1].LocalDescription.MediaType),
              'Stream #2 changed its media type');

  CheckEquals(OldSessionVersion + 1, Self.MS.LocalSessionVersion, 'sess-version not incremented');
end;

procedure TestTIdSDPMultimediaSession.TestSetIsOffer;
begin
  Self.MS.IsOffer := false;
  Self.MS.StartListening(Self.MultiStreamSDP(ArbitraryPort, ArbitraryPort));
  CheckEquals(Self.MS.IsOffer, Self.MS.Streams[0].IsOffer, 'Stream''s IsOffer not set to false');

  Self.MS.StopListening;
  Self.MS.IsOffer := true;
  Self.MS.StartListening(Self.MultiStreamSDP(ArbitraryPort, ArbitraryPort));
  CheckEquals(Self.MS.IsOffer, Self.MS.Streams[0].IsOffer, 'Stream''s IsOffer not set to true');
end;

procedure TestTIdSDPMultimediaSession.TestSetIsOfferSetsStreamsIsOffer;
begin
  Self.MS.StartListening(Self.MultiStreamSDP(ArbitraryPort, ArbitraryPort));

  Self.MS.IsOffer := true;
  CheckEquals(Self.MS.IsOffer, Self.MS.Streams[0].IsOffer, 'Stream''s IsOffer not set to true');

  Self.MS.IsOffer := false;
  CheckEquals(Self.MS.IsOffer, Self.MS.Streams[0].IsOffer, 'Stream''s IsOffer not set to false');
end;

procedure TestTIdSDPMultimediaSession.TestSetLocalMachineName;
begin
  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));
  Self.MS.LocalMachineName := IPv4LocalHost;

  CheckOrigin(Id_SDP_IN,
              Id_IPv4,
              IPv4LocalHost,
              Self.MS.LocalSessionDescription);

  Self.MS.LocalMachineName := IPv6LocalHost;

  CheckOrigin(Id_SDP_IN,
              Id_IPv6,
              IPv6LocalHost,
              Self.MS.LocalSessionDescription);
end;

procedure TestTIdSDPMultimediaSession.TestSetLocalSessionName;
const
  OldSessionName = 'old';
  NewSessionName = 'new';
begin
  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));

  CheckSessionName(BlankSessionName,
                   Self.MS.LocalSessionDescription,
                   'Default SessionName value');

  Self.MS.LocalSessionName := OldSessionName;

  CheckSessionName(OldSessionName,
                   Self.MS.LocalSessionDescription,
                  'SessionName not set');

  Self.MS.LocalSessionName := NewSessionName;
  CheckSessionName(NewSessionName,
                   Self.MS.LocalSessionDescription,
                   'SessionName not reset');
end;

procedure TestTIdSDPMultimediaSession.TestSetRemoteDescription;
var
  LowPort:  Cardinal;
  HighPort: Cardinal;
begin
  LowPort  := 8900;
  HighPort := 9900;

  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));

  Self.MS.SetRemoteDescription(Self.MultiStreamSDP(LowPort, HighPort));

  CheckEquals(LowPort,
              Self.MS.Streams[0].RemoteDescription.Port,
              'Remote description of first stream not set');
  CheckEquals(HighPort,
              Self.MS.Streams[1].RemoteDescription.Port,
              'Remote description of second stream not set');
end;

procedure TestTIdSDPMultimediaSession.TestSetRemoteDescriptionWithSdpPayload;
var
  LowPort:  Cardinal;
  HighPort: Cardinal;
  SDP:      TIdSdpPayload;
begin
  LowPort  := 8900;
  HighPort := 9900;

  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));

  SDP := TIdSdpPayload.CreateFrom(Self.MultiStreamSDP(LowPort, HighPort));
  try
    Self.MS.StartListening(SDP.AsString);
    Self.MS.SetRemoteDescription(SDP);

    CheckEquals(LowPort,
                Self.MS.Streams[0].RemoteDescription.Port,
                'Remote description of first stream not set');
    CheckEquals(HighPort,
                Self.MS.Streams[1].RemoteDescription.Port,
                'Remote description of second stream not set');
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestSetUsername;
const
  UsernameCase       = 'Case';
  UsernameWintermute = 'Wintermute';
begin
  CheckEquals(BlankUsername, Self.MS.Username, 'Default Username value');

  Self.MS.StartListening(Self.SingleStreamSDP(ArbitraryPort));

  Self.MS.Username := UsernameCase;
  CheckOriginUsername(Self.MS.Username, Self.MS.LocalSessionDescription);

  Self.MS.Username := UsernameWintermute;
  CheckOriginUsername(Self.MS.Username, Self.MS.LocalSessionDescription);
end;

procedure TestTIdSDPMultimediaSession.TestSetRemoteDescriptionMalformedSdp;
begin
  try
    Self.MS.SetRemoteDescription('');
    Fail('Set remote description on a malformed session description');
  except
    on EParserError do;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningSingleStream;
var
  Port: Cardinal;
begin
  Port := 8000;
  Self.MS.StartListening(Self.SingleStreamSDP(Port));

  Self.CheckPortActive('127.0.0.1',
                       Port,
                       'Server not listening on port 8000');
  CheckEquals(1, Self.MS.StreamCount, 'StreamCount');
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningMalformedSdp;
begin
  try
    Self.MS.StartListening('');
    Fail('Started listening on malformed SDP');
  except
    on EParserError do;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningMultipleStreams;
var
  Desc:     TIdSdpPayload;
  LowPort:  Cardinal;
  HighPort: Cardinal;
begin
  LowPort  := 8000;
  HighPort := 9000;

  Desc := TIdSdpPayload.CreateFrom(Self.MultiStreamSDP(LowPort, HighPort));
  try
    Self.MS.StartListening(Desc.AsString);

    CheckEquals(2, Self.MS.StreamCount, 'StreamCount');
    Self.CheckPortActive(Desc.MediaDescriptionAt(0).Connections[0].Address,
                         LowPort,
                         'Server not listening on ' + Desc.MediaDescriptionAt(0).Connections[0].AsString);
    Self.CheckPortActive(Desc.MediaDescriptionAt(1).Connections[0].Address,
                         HighPort,
                         'Server not listening on ' + Desc.MediaDescriptionAt(1).Connections[0].AsString);
  finally
    Desc.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningNoAvailablePorts;
var
  SDP: TIdSdpPayload;
begin
  Self.MS.LowestAllowedPort  := 8000;
  Self.MS.HighestAllowedPort := Self.MS.LowestAllowedPort + 2;

  Self.StartServerOnPort(Self.MS.LowestAllowedPort);

  SDP := TIdSdpPayload.CreateFrom(Self.MS.StartListening(Self.SingleStreamSDP(Self.MS.LowestAllowedPort)));
  try
    CheckEquals(0,
                SDP.MediaDescriptionAt(0).Port,
                'Unavailable port not marked as refused');
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningPortsOutsideAllowedRange;
var
  I:   Integer;
  SDP: TIdSdpPayload;
begin
  // We create an abnormal setup: there's no legal ports to use!
  Self.MS.LowestAllowedPort := 10000;
  Self.MS.HighestAllowedPort := 9000;

  SDP := TIdSdpPayload.CreateFrom(Self.MS.StartListening(Self.MultiStreamSDP(Self.MS.HighestAllowedPort, Self.MS.LowestAllowedPort)));
  try
    for I := 0 to SDP.MediaDescriptionCount - 1 do
      CheckEquals(0,
                  SDP.MediaDescriptionAt(0).Port,
                  IntToStr(I) + 'th description''s port');
  finally
    SDP.Free;
  end;
end;

procedure TestTIdSDPMultimediaSession.TestStartListeningTCP;
begin
  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000, Id_SDP_TCP));

  CheckEquals(TIdSdpTcpMediaStream,
              Self.MS.Streams[0].ClassType,
              'Type of first stream');
  CheckEquals(TIdSdpTcpMediaStream,
              Self.MS.Streams[1].ClassType,
              'Type of first stream');
end;

procedure TestTIdSDPMultimediaSession.TestStopListening;
var
  LowPort:  Cardinal;
  HighPort: Cardinal;
begin
  LowPort  := 8000;
  HighPort := 9000;

  Self.MS.StartListening(Self.MultiStreamSDP(LowPort, HighPort));

  Self.MS.StopListening;

  Self.CheckPortFree('127.0.0.1',
                     LowPort,
                     'Server still listening on 127.0.0.7:' + IntToStr(LowPort));
  Self.CheckPortFree(GStack.LocalAddress,
                     HighPort,
                     'Server still listening on '
                   + GStack.LocalAddress + ':' + IntToStr(HighPort));
end;

procedure TestTIdSDPMultimediaSession.TestStreamNeedsMorePortsThanAvailable;
var
  Desc:        String;
  HighestPort: Cardinal;
begin
  Self.MS.LowestAllowedPort  := 8000;
  Self.MS.HighestAllowedPort := 8100;

  // This port allocation is invalid, because the stream needs to open an RTCP
  // port, on port 8101, which lies outside the permitted range.
  HighestPort := Self.MS.HighestAllowedPort;
  Desc := Self.MS.StartListening(Self.SingleStreamSDP(HighestPort));

  Check(Self.MS.StreamCount > 0,
        'Not enough streams instantiated');
  CheckEquals(Self.MS.LowestAllowedPort,
              Self.MS.Streams[0].LocalDescription.Port,
              'Port value');
end;

procedure TestTIdSDPMultimediaSession.TestTakeOffHold;
var
  OldSessionVersion: Int64;
begin
  Self.MS.StartListening(Self.MultiStreamSDP(8000, 9000));
  Self.MS.Streams[0].Direction := sdRecvOnly;
  Self.MS.Streams[1].Direction := sdSendRecv;

  Self.MS.PutOnHold;

  OldSessionVersion := Self.MS.LocalSessionVersion;
  Self.MS.TakeOffHold;

  Check(not Self.MS.Streams[0].OnHold,
        'Stream #0 not taken off hold');
  Check(not Self.MS.Streams[1].OnHold,
        'Stream #1 not taken off hold');

  CheckEquals(OldSessionVersion + 1, Self.MS.LocalSessionVersion, 'sess-version not incremented');
end;

//******************************************************************************
//* TIdSdpTcpConnectionWaitTestCase                                            *
//******************************************************************************
//* TIdSdpTcpConnectionWaitTestCase Public methods *****************************

procedure TIdSdpTcpConnectionWaitTestCase.SetUp;
begin
  inherited SetUp;

  Self.Connection := TIdSdpMockTcpClientConnection.Create;
  Self.Data       := TStringStream.Create('foo');

  Self.Connection.ConnectTo('127.0.0.1', 8000);

  Self.Wait := Self.CreateWait;
  Self.Wait.ConnectionID := Self.Connection.ID;
end;

procedure TIdSdpTcpConnectionWaitTestCase.TearDown;
begin
  Self.Data.Free;
  Self.Connection.Free;
  Self.Wait.Free;

  inherited TearDown;
end;

//* TIdSdpTcpConnectionWaitTestCase Protected methods **************************

procedure TIdSdpTcpConnectionWaitTestCase.CheckTriggerDoesNothing(Msg: String);
begin
  Fail(Self.ClassName + ' MUST override CheckTriggerDoesNothing');
end;

procedure TIdSdpTcpConnectionWaitTestCase.CheckTriggerFired;
begin
  Fail(Self.ClassName + ' MUST override CheckTriggerFired');
end;

function TIdSdpTcpConnectionWaitTestCase.CreateWait: TIdSdpTcpConnectionWait;
begin
  Result := nil;
  Fail(Self.ClassName + ' MUST override CreateWait');
end;

//* TIdSdpTcpConnectionWaitTestCase Published methods **************************

procedure TIdSdpTcpConnectionWaitTestCase.TestTrigger;
begin
  Self.Wait.Trigger;
  CheckTriggerFired;
end;

procedure TIdSdpTcpConnectionWaitTestCase.TestTriggerWithNonexistentConnection;
begin
  Self.Wait.ConnectionID := 'fake ID';
  Self.Wait.Trigger;
  CheckTriggerDoesNothing('Triggered using a nonexistent object ID');
end;

procedure TIdSdpTcpConnectionWaitTestCase.TestTriggerWithWrongTypeOfObject;
var
  R: TIdRegisteredObject;
begin
  R := TIdRegisteredObject.Create;
  try
    Self.Wait.ConnectionID := R.ID;
    Self.Wait.Trigger;
    CheckTriggerDoesNothing('Triggered using the ID of some inappropriate object');
  finally
    R.Free;
  end;
end;

//******************************************************************************
//* TestTIdSdpTcpReceiveDataWait                                               *
//******************************************************************************
//* TestTIdSdpTcpReceiveDataWait Public methods ********************************

procedure TestTIdSdpTcpReceiveDataWait.SetUp;
begin
  inherited SetUp;

  Self.Binding := TIdConnectionBindings.Create(Self.Connection.Address,
                                               Self.Connection.Port,
                                               Self.Connection.PeerAddress,
                                               Self.Connection.PeerPort,
                                               Id_SDP_TCP);

  Self.RecvWait := Self.Wait as TIdSdpTcpReceiveDataWait;
  Self.RecvWait.Data       := Self.Data;
  Self.RecvWait.ReceivedOn := Self.Binding;
end;

procedure TestTIdSdpTcpReceiveDataWait.TearDown;
begin
  Self.Binding.Free;

  inherited TearDown;
end;

//* TestTIdSdpTcpReceiveDataWait Protected methods *****************************

procedure TestTIdSdpTcpReceiveDataWait.CheckTriggerDoesNothing(Msg: String);
begin
  CheckEquals('', Self.Connection.ReceivedData, Msg);
end;

procedure TestTIdSdpTcpReceiveDataWait.CheckTriggerFired;
begin
  CheckEquals(Self.Data.DataString, Self.Connection.ReceivedData, 'Data not received, so Wait didn''t Trigger');
end;

function TestTIdSdpTcpReceiveDataWait.CreateWait: TIdSdpTcpConnectionWait;
begin
  Result := TIdSdpTcpReceiveDataWait.Create;
end;

//******************************************************************************
//* TestTIdSdpTcpSendDataWait                                                  *
//******************************************************************************
//* TestTIdSdpTcpSendDataWait Public methods ***********************************

procedure TestTIdSdpTcpSendDataWait.SetUp;
begin
  inherited SetUp;

  Self.SendWait := Self.Wait as TIdSdpTcpSendDataWait;
  Self.SendWait.Data := Self.Data;
end;

//* TestTIdSdpTcpSendDataWait Protected methods ********************************

procedure TestTIdSdpTcpSendDataWait.CheckTriggerDoesNothing(Msg: String);
begin
  CheckEquals('', Self.Connection.SentData, Msg);
end;

procedure TestTIdSdpTcpSendDataWait.CheckTriggerFired;
begin
  CheckEquals(Self.Data.DataString, Self.Connection.SentData, 'Data not sent, so Wait didn''t Trigger');
end;

function TestTIdSdpTcpSendDataWait.CreateWait: TIdSdpTcpConnectionWait;
begin
  Result := TIdSdpTcpSendDataWait.Create;
end;

//******************************************************************************
//* TestTIdSdpTcpConnectionConnectedWait                                       *
//******************************************************************************
//* TestTIdSdpTcpConnectionConnectedWait Public methods ************************

procedure TestTIdSdpTcpConnectionConnectedWait.SetUp;
begin
  inherited SetUp;

  Self.Listener := TIdSdpTestConnectionListener.Create;
  Self.Connection.AddDataListener(Self.Listener);
end;

procedure TestTIdSdpTcpConnectionConnectedWait.TearDown;
begin
  inherited TearDown;

//  Self.Connection.RemoveDataListener(Self.Listener);

  Self.Listener.Free;
end;

//* TestTIdSdpTcpConnectionConnectedWait Protected methods *********************

procedure TestTIdSdpTcpConnectionConnectedWait.CheckTriggerDoesNothing(Msg: String);
begin
  Check(not Self.Listener.OnConnectCalled, Msg);
end;

procedure TestTIdSdpTcpConnectionConnectedWait.CheckTriggerFired;
begin
  Check(Self.Listener.OnConnectCalled, 'Listener not notified - Wait didn''t trigger');
end;

function TestTIdSdpTcpConnectionConnectedWait.CreateWait: TIdSdpTcpConnectionWait;
begin
  Result := TIdSdpTcpConnectionConnectedWait.Create;
end;

//******************************************************************************
//* TestTIdSdpTcpConnectionDisconnectedWait                                    *
//******************************************************************************
//* TestTIdSdpTcpConnectionDisconnectedWait Public methods *********************

procedure TestTIdSdpTcpConnectionDisconnectedWait.CheckTriggerDoesNothing(Msg: String);
begin
  Check(not Self.Listener.OnDisconnectCalled, Msg);
end;

procedure TestTIdSdpTcpConnectionDisconnectedWait.CheckTriggerFired;
begin
  Check(Self.Listener.OnDisconnectCalled, 'Listener not notified - Wait didn''t trigger');
end;

function TestTIdSdpTcpConnectionDisconnectedWait.CreateWait: TIdSdpTcpConnectionWait;
begin
  Result := TIdSdpTcpConnectionDisconnectedWait.Create;
end;

procedure TestTIdSdpTcpConnectionDisconnectedWait.SetUp;
begin
  inherited SetUp;

  Self.Listener := TIdSdpTestConnectionListener.Create;
  Self.Connection.AddDataListener(Self.Listener);
end;

procedure TestTIdSdpTcpConnectionDisconnectedWait.TearDown;
begin
  Self.Listener.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSdpTcpConnectionExceptionWait                                       *
//******************************************************************************
//* TestTIdSdpTcpConnectionExceptionWait Public methods ************************

procedure TestTIdSdpTcpConnectionExceptionWait.SetUp;
begin
  Self.ExceptionMessage := 'Access violation reading address 00000000';
  Self.ExceptionType    := EAccessViolation;

  inherited SetUp;

  Self.Listener := TIdSdpTestConnectionListener.Create;
  Self.Connection.AddDataListener(Self.Listener);
end;

procedure TestTIdSdpTcpConnectionExceptionWait.TearDown;
begin
  Self.Listener.Free;

  inherited TearDown;
end;

//* TestTIdSdpTcpConnectionExceptionWait Protected methods *********************

procedure TestTIdSdpTcpConnectionExceptionWait.CheckTriggerDoesNothing(Msg: String);
begin
  Check(not Self.Listener.OnExceptionCalled, Msg);
end;

procedure TestTIdSdpTcpConnectionExceptionWait.CheckTriggerFired;
begin
  Check(Self.Listener.OnExceptionCalled, 'Listener not notified - Wait didn''t trigger');

  CheckEquals(Self.ExceptionType,    Self.Listener.ExceptionType,    'ExceptionType parameter');
  CheckEquals(Self.ExceptionMessage, Self.Listener.ExceptionMessage, 'ExceptionMessage parameter');
end;

function TestTIdSdpTcpConnectionExceptionWait.CreateWait: TIdSdpTcpConnectionWait;
var
  W: TIdSdpTcpConnectionExceptionWait;
begin
  W := TIdSdpTcpConnectionExceptionWait.Create;
  W.ExceptionMessage := Self.ExceptionMessage;
  W.ExceptionType    := Self.ExceptionType;

  Result := W;
end;

//******************************************************************************
//* TIdSdpMediaListenerMethodTestCase                                          *
//******************************************************************************
//* TIdSdpMediaListenerMethodTestCase Public methods ***************************

procedure TIdSdpMediaListenerMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Chunk    := TMemoryStream.Create;
  Self.Format   := '96';
  Self.Listener := TIdSdpTestMediaListener.Create;
  Self.Stream   := TIdSdpMediaStream.Create;
end;

procedure TIdSdpMediaListenerMethodTestCase.TearDown;
begin
  Self.Stream.Free;
  Self.Listener.Free;
  Self.Chunk.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSdpMediaListenerOnDataMethod                                        *
//******************************************************************************
//* TestTIdSdpMediaListenerOnDataMethod Public methods *************************

procedure TestTIdSdpMediaListenerOnDataMethod.SetUp;
begin
  inherited SetUp;

  Self.Binding  := TIdConnectionBindings.Create;
  Self.Method   := TIdSdpMediaListenerOnDataMethod.Create;

  Self.Method.Binding := Self.Binding;
  Self.Method.Chunk   := Self.Chunk;
  Self.Method.Stream  := Self.Stream;
end;

procedure TestTIdSdpMediaListenerOnDataMethod.TearDown;
begin
  Self.Method.Free;
  Self.Binding.Free;

  inherited TearDown;
end;

//* TestTIdSdpMediaListenerOnDataMethod Published methods **********************

procedure TestTIdSdpMediaListenerOnDataMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.ReceivedData, 'Listener not notified');

  Check(Self.Method.Binding = Self.Listener.BindingParam,    'BindingParam');
  Check(Self.Method.Chunk   = Self.Listener.ChunkParam,      'ChunkParam');
  CheckEquals(Self.Method.Format, Self.Listener.FormatParam, 'FormatParam');
  Check(Self.Method.Stream  = Self.Listener.StreamParam,     'StreamParam');
end;

//******************************************************************************
//* TestTIdSdpMediaListenerOnSentDataMethod                                    *
//******************************************************************************
//* TestTIdSdpMediaListenerOnSentDataMethod Public methods *********************

procedure TestTIdSdpMediaListenerOnSentDataMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSdpMediaListenerOnSentDataMethod.Create;

  Self.Method.Chunk   := Self.Chunk;
  Self.Method.LayerID := 42;
  Self.Method.Stream  := Self.Stream;
end;

procedure TestTIdSdpMediaListenerOnSentDataMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSdpMediaListenerOnSentDataMethod Published methods ******************

procedure TestTIdSdpMediaListenerOnSentDataMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.SentData, 'Listener not notified');

  Check(Self.Method.Chunk   = Self.Listener.ChunkParam,        'ChunkParam');
  CheckEquals(Self.Method.Format,  Self.Listener.FormatParam,  'FormatParam');
  CheckEquals(Self.Method.LayerID, Self.Listener.LayerIDParam, 'LayerIDParam');
  Check(Self.Method.Stream  = Self.Listener.StreamParam,       'StreamParam');
end;

//******************************************************************************
//* TIdSdpTcpConnectionMethodTestCase                                          *
//******************************************************************************
//* TIdSdpTcpConnectionMethodTestCase Public methods ***************************

procedure TIdSdpTcpConnectionMethodTestCase.SetUp;
begin
  inherited SetUp;

  Self.Connection := TIdSdpMockTcpConnection.Create;
  Self.Listener   := TIdSdpTestConnectionListener.Create;
end;

procedure TIdSdpTcpConnectionMethodTestCase.TearDown;
begin
  Self.Listener.Free;
  Self.Connection.Free;

  inherited TearDown;
end;

//******************************************************************************
//* TestTIdSdpTcpConnectionOnConnectMethod                                     *
//******************************************************************************
//* TestTIdSdpTcpConnectionOnConnectMethod Public methods **********************

procedure TestTIdSdpTcpConnectionOnConnectMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSdpTcpConnectionOnConnectMethod.Create;

  Self.Method.Connection := Self.Connection;
end;

procedure TestTIdSdpTcpConnectionOnConnectMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSdpTcpConnectionOnConnectMethod Published methods *******************

procedure TestTIdSdpTcpConnectionOnConnectMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.OnConnectCalled,
        'Listener not notified');
  Check(Self.Connection = Self.Listener.ConnectionParam,
        'Connection parameter');
end;

//******************************************************************************
//* TestTIdSdpTcpConnectionOnDataMethod                                        *
//******************************************************************************
//* TestTIdSdpTcpConnectionOnDataMethod Public methods *************************

procedure TestTIdSdpTcpConnectionOnDataMethod.SetUp;
begin
  inherited SetUp;

  Self.Data       := TStringStream.Create('chunk of data');
  Self.Method     := TIdSdpTcpConnectionOnDataMethod.Create;

  Self.Method.Connection := Self.Connection;
  Self.Method.Data       := Self.Data;
end;

procedure TestTIdSdpTcpConnectionOnDataMethod.TearDown;
begin
  Self.Method.Free;
  Self.Data.Free;

  inherited TearDown;
end;

//* TestTIdSdpTcpConnectionOnDataMethod Published methods **********************

procedure TestTIdSdpTcpConnectionOnDataMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.OnDataCalled,
        'Listener not notified');
  Check(Self.Connection = Self.Listener.ConnectionParam,
        'Connection parameter');
  CheckEquals(Self.Data.DataString, StreamToStr(Self.Listener.DataParam),
        'Data parameter');
end;

//******************************************************************************
//* TestTIdSdpTcpConnectionOnDisconnectMethod                                  *
//******************************************************************************
//* TestTIdSdpTcpConnectionOnDisconnectMethod Public methods *******************

procedure TestTIdSdpTcpConnectionOnDisconnectMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSdpTcpConnectionOnDisconnectMethod.Create;

  Self.Method.Connection := Self.Connection;
end;

procedure TestTIdSdpTcpConnectionOnDisconnectMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSdpTcpConnectionOnDisconnectMethod Published methods ****************

procedure TestTIdSdpTcpConnectionOnDisconnectMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.OnDisconnectCalled,
        'Listener not notified');
  Check(Self.Connection = Self.Listener.ConnectionParam,
        'Connection parameter');
end;

//******************************************************************************
//* TestTIdSdpTcpConnectionOnExceptionMethod                                   *
//******************************************************************************
//* TestTIdSdpTcpConnectionOnExceptionMethod Public methods ********************

procedure TestTIdSdpTcpConnectionOnExceptionMethod.SetUp;
begin
  inherited SetUp;

  Self.Method := TIdSdpTcpConnectionOnExceptionMethod.Create;
  Self.Method.Connection       := Self.Connection;
  Self.Method.ExceptionMessage := 'Access violation read of address 00000000';
  Self.Method.ExceptionType    := EAccessViolation;
end;

procedure TestTIdSdpTcpConnectionOnExceptionMethod.TearDown;
begin
  Self.Method.Free;

  inherited TearDown;
end;

//* TestTIdSdpTcpConnectionOnExceptionMethod Published methods *****************

procedure TestTIdSdpTcpConnectionOnExceptionMethod.TestRun;
begin
  Self.Method.Run(Self.Listener);

  Check(Self.Listener.OnExceptionCalled,
        'Listener not notified');
  Check(Self.Method.Connection = Self.Listener.ConnectionParam,
        'Connection param');
  CheckEquals(Self.Method.ExceptionMessage, Self.Listener.ExceptionMessage,
              'ExceptionMessage param');
  CheckEquals(Self.Method.ExceptionType, Self.Listener.ExceptionType,
              'ExceptionType param');
end;

initialization
  RegisterTest('SDP classes', Suite);
end.
