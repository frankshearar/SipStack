{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipConsts;

interface

// for IdResourceStrings ?
const
  AcceptHeader               = 'Accept';
  AcceptEncodingHeader       = 'Accept-Encoding';
  AcceptLanguageHeader       = 'Accept-Language';
  AlertInfoHeader            = 'Alert-Info';
  AlgorithmParam             = 'algorithm';
  AllowHeader                = 'Allow';
  AuthenticationInfoHeader   = 'Authentication-Info';
  AuthorizationHeader        = 'Authorization';
  BasicAuthorizationScheme   = 'Basic';
  BranchMagicCookie          = 'z9hG4bK';
  BranchParam                = 'branch';
  CallIDHeaderFull           = 'Call-ID';
  CallIDHeaderShort          = 'i';
  CallInfoHeader             = 'Call-Info';
  CNonceParam                = 'cnonce';
  ContactHeaderFull          = 'Contact';
  ContactHeaderShort         = 'm';
  ContactWildCard            = '*';
  ContentDispositionHeader   = 'Content-Disposition';
  ContentEncodingHeaderFull  = 'Content-Encoding';
  ContentEncodingHeaderShort = 'e';
  ContentLanguageHeader      = 'Content-Language';
  ContentLengthHeaderFull    = 'Content-Length';
  ContentLengthHeaderShort   = 'l';
  ContentTypeHeaderFull      = 'Content-Type';
  ContentTypeHeaderShort     = 'c';
  CSeqHeader                 = 'CSeq';
  DateHeader                 = 'Date';
  DigestAuthorizationScheme  = 'Digest';
  DigestResponseParam        = 'response';
  DigestUriParam             = 'uri';
  DispositionAlert           = 'alert';
  DispositionIcon            = 'icon';
  DispositionRender          = 'render';
  DispositionSession         = 'session';
  DomainParam                = 'domain';
  ErrorInfoHeader            = 'Error-Info';
  ExpiresHeader              = 'Expires';
  ExpiresParam               = 'expires';
  FromHeaderFull             = 'From';
  FromHeaderShort            = 'f';
  HandlingOptional           = 'optional';
  HandlingParam              = 'handling';
  HandlingRequired           = 'required';
  InReplyToHeader            = 'In-Reply-To';
  LooseRoutableParam         = 'lr';
  MaddrParam                 = 'maddr';
  MaxForwardsHeader          = 'Max-Forwards';
  MD5Name                    = 'MD5';
  MD5SessionName             = 'MD5-sess';
  MethodAck                  = 'ACK';
  MethodBye                  = 'BYE';
  MethodCancel               = 'CANCEL';
  MethodInvite               = 'INVITE';
  MethodOptions              = 'OPTIONS';
  MethodParam                = 'method';
  MethodRegister             = 'REGISTER';
  MIMEVersionHeader          = 'MIME-Version';
  MinExpiresHeader           = 'Min-Expires';
  NonceCountParam            = 'nc';
  NonceParam                 = 'nonce';
  OpaqueParam                = 'opaque';
  OrganizationHeader         = 'Organization';
  PriorityHeader             = 'Priority';
  ProxyAuthenticateHeader    = 'Proxy-Authenticate';
  ProxyAuthorizationHeader   = 'Proxy-Authorization';
  ProxyRequireHeader         = 'Proxy-Require';
  QopAuth                    = 'auth';
  QopAuthInt                 = 'auth-int';
  QopParam                   = 'qop';
  QParam                     = 'q';
  RealmParam                 = 'realm';
  ReceivedParam              = 'received';
  RecordRouteHeader          = 'Record-Route';
  ReplyToHeader              = 'Reply-To';
  RequireHeader              = 'Require';
  RetryAfterHeader           = 'Retry-After';
  RouteHeader                = 'Route';
  RportParam                 = 'rport';
  ServerHeader               = 'Server';
  SipName                    = 'SIP';
  SipScheme                  = 'sip';
  SipsScheme                 = 'sips';
  StaleParam                 = 'stale';
  SubjectHeaderFull          = 'Subject';
  SubjectHeaderShort         = 's';
  SupportedHeaderFull        = 'Supported';
  SupportedHeaderShort       = 'k';
  TagParam                   = 'tag';
  TimestampHeader            = 'Timestamp';
  ToHeaderFull               = 'To';
  ToHeaderShort              = 't';
  TransportParam             = 'transport';
  TransportParamSCTP         = 'sctp';
  TransportParamTCP          = 'tcp';
  TransportParamTLS          = 'tls';
  TransportParamUDP          = 'udp';
  TTLParam                   = 'ttl';
  UnsupportedHeader          = 'Unsupported';
  UserAgentHeader            = 'User-Agent';
  UsernameParam              = 'username';
  UserParam                  = 'user';
  UserParamIp                = 'ip';
  UserParamPhone             = 'phone';
  ViaHeaderFull              = 'Via';
  ViaHeaderShort             = 'v';
  WarningHeader              = 'Warning';
  WWWAuthenticateHeader      = 'WWW-Authenticate';

const
  SIPVersion  = SipName + '/2.0';
  SIPVersion1 = SipName + '/1.0';

// for IdAssignedNumbers
const
  IdPORT_SIP  = 5060;
  IdPORT_SIPS = 5061;

// for IdResourceStrings
const
  RSSIPTrying                           = 'Trying';
  RSSIPRinging                          = 'Ringing';
  RSSIPCallIsBeingForwarded             = 'Call Is Being Forwarded';
  RSSIPQueued                           = 'Queued';
  RSSIPSessionProgess                   = 'Session Progress';
  RSSIPOK                               = 'OK';
  RSSIPMultipleChoices                  = 'Multiple Choices';
  RSSIPMovedPermanently                 = 'Moved Permanently';
  RSSIPMovedTemporarily                 = 'Moved Temporarily';
  RSSIPUseProxy                         = 'Use Proxy';
  RSSIPAlternativeService               = 'Alternative Service';
  RSSIPBadRequest                       = 'Bad Request';
  RSSIPUnauthorized                     = 'Unauthorized';
  RSSIPPaymentRequired                  = 'Payment Required';
  RSSIPForbidden                        = 'Forbidden';
  RSSIPNotFound                         = 'Not Found';
  RSSIPMethodNotAllowed                 = 'Method Not Allowed';
  RSSIPNotAcceptableClient              = 'Not Acceptable';
  RSSIPProxyAuthenticationRequired      = 'Proxy Authentication Required';
  RSSIPRequestTimeout                   = 'Request Timeout';
  RSSIPGone                             = 'Gone';
  RSSIPRequestEntityTooLarge            = 'Request Entity Too Large';
  RSSIPRequestURITooLarge               = 'Request-URI Too Large';
  RSSIPUnsupportedMediaType             = 'Unsupported Media Type';
  RSSIPUnsupportedURIScheme             = 'Unsupported URI Scheme';
  RSSIPBadExtension                     = 'Bad Extension';
  RSSIPExtensionRequired                = 'Extension Required';
  RSSIPIntervalTooBrief                 = 'Interval Too Brief';
  RSSIPTemporarilyUnavailable           = 'Temporarily unavailable';
  RSSIPCallLegOrTransactionDoesNotExist = 'Call Leg/Transaction Does Not Exist';
  RSSIPLoopDetected                     = 'Loop Detected';
  RSSIPTooManyHops                      = 'Too Many Hops';
  RSSIPAddressIncomplete                = 'Address Incomplete';
  RSSIPAmbiguous                        = 'Ambiguous';
  RSSIPBusyHere                         = 'Busy Here';
  RSSIPRequestTerminated                = 'Request Terminated';
  RSSIPNotAcceptableHere                = 'Not Acceptable Here';
  RSSIPRequestPending                   = 'Request Pending';
  RSSIPUndecipherable                   = 'Undecipherable';
  RSSIPInternalServerError              = 'Internal Server Error';
  RSSIPNotImplemented                   = 'Not Implemented';
  RSSIPBadGateway                       = 'Bad Gateway';
  RSSIPServiceUnavailable               = 'Service Unavailable';
  RSSIPServerTimeOut                    = 'Server Time-out';
  RSSIPSIPVersionNotSupported           = 'SIP Version not supported';
  RSSIPMessageTooLarge                  = 'Message Too Large';
  RSSIPBusyEverywhere                   = 'Busy Everywhere';
  RSSIPDecline                          = 'Decline';
  RSSIPDoesNotExistAnywhere             = 'Does not exist anywhere';
  RSSIPNotAcceptableGlobal              = RSSIPNotAcceptableClient;
  RSSIPUnknownResponseCode              = 'Unknown Response Code';

  RSSIPRequestOutOfOrder = 'Request out of order';

const
  SIPProvisionalResponseClass   = 1;
  SIPOKResponseClass            = 2;
  SIPRedirectionResponseClass   = 3;
  SIPFailureResponseClass       = 4;
  SIPServerFailureResponseClass = 5;
  SIPGlobalFailureResponseClass = 6;

const
  SIPTrying                           = 100;
  SIPRinging                          = 180;
  SIPCallIsBeingForwarded             = 181;
  SIPQueued                           = 182;
  SIPSessionProgess                   = 183;
  SIPOK                               = 200;
  SIPMultipleChoices                  = 300;
  SIPMovedPermanently                 = 301;
  SIPMovedTemporarily                 = 302;
  SIPUseProxy                         = 305;
  SIPAlternativeService               = 380;
  SIPBadRequest                       = 400;
  SIPUnauthorized                     = 401;
  SIPPaymentRequired                  = 402;
  SIPForbidden                        = 403;
  SIPNotFound                         = 404;
  SIPMethodNotAllowed                 = 405;
  SIPNotAcceptableClient              = 406;
  SIPProxyAuthenticationRequired      = 407;
  SIPRequestTimeout                   = 408;
  SIPGone                             = 410;
  SIPRequestEntityTooLarge            = 413;
  SIPRequestURITooLarge               = 414;
  SIPUnsupportedMediaType             = 415;
  SIPUnsupportedURIScheme             = 416;
  SIPBadExtension                     = 420;
  SIPExtensionRequired                = 421;
  SIPIntervalTooBrief                 = 423;
  SIPTemporarilyUnavailable           = 480;
  SIPCallLegOrTransactionDoesNotExist = 481;
  SIPLoopDetected                     = 482;
  SIPTooManyHops                      = 483;
  SIPAddressIncomplete                = 484;
  SIPAmbiguous                        = 485;
  SIPBusyHere                         = 486;
  SIPRequestTerminated                = 487;
  SIPNotAcceptableHere                = 488;
  SIPRequestPending                   = 491;
  SIPUndecipherable                   = 493;
  SIPInternalServerError              = 500;
  SIPNotImplemented                   = 501;
  SIPBadGateway                       = 502;
  SIPServiceUnavailable               = 503;
  SIPServerTimeOut                    = 504;
  SIPSIPVersionNotSupported           = 505;
  SIPMessageTooLarge                  = 513;
  SIPBusyEverywhere                   = 600;
  SIPDecline                          = 603;
  SIPDoesNotExistAnywhere             = 604;
  SIPNotAcceptableGlobal              = 606;

const
  WarningIncompatibleNetworkProtocol              = 300;
  WarningIncompatibleNetworkAddressFormats        = 301;
  WarningIncompatibleTransportProtocol            = 302;
  WarningIncompatibleBandwidthUnits               = 303;
  WarningMediaTypeNotAvailable                    = 304;
  WarningIncompatibleMediaFormat                  = 305;
  WarningAttributeNotUnderstood                   = 306;
  WarningSessionDescriptionParameterNotUnderstood = 307;
  WarningMulticastNotAvailable                    = 330;
  WarniningUnicastNotAvailable                    = 331;
  WarningInsufficientBandwidth                    = 370;
  WarningMisc                                     = 399;

const
  RSWarningIncompatibleNetworkProtocol =
      'Incompatible network protocol: One or more network protocols contained '
    + 'in the session description are not available.';
  RSWarningIncompatibleNetworkAddressFormats =
      'Incompatible network address formats: One or more network address '
    + 'formats contained in the session description are not available.';
  RSWarningIncompatibleTransportProtocol =
      'Incompatible transport protocol: One or more transport protocols '
    + 'described in the session description are not available.';
  RSWarningIncompatibleBandwidthUnits =
      'Incompatible bandwidth units: One or more bandwidth measurement units '
    + 'contained in the session description were not understood.';
  RSWarningMediaTypeNotAvailable =
      'Media type not available: One or more media types contained in the '
    + 'session description are not available.';
  RSWarningIncompatibleMediaFormat =
      'Incompatible media format: One or more media formats contained in the '
    + 'session description are not available.';
  RSWarningAttributeNotUnderstood =
      'Attribute not understood: One or more of the media attributes in the '
    + 'session description are not supported.';
  RSWarningSessionDescriptionParameterNotUnderstood =
      'Session description parameter not understood: A parameter other than '
    + 'those listed above was not understood.';
  RSWarningMulticastNotAvailable =
      'Multicast not available: The site where the user is located does not '
    + 'support multicast.';
  RSWarniningUnicastNotAvailable =
      'Unicast not available: The site where the user is located does not '
    + 'support unicast communication (usually due to the presence of a '
    + 'firewall).';
  RSWarningInsufficientBandwidth =
      'Insufficient bandwidth: The bandwidth specified in the session '
    + 'description or defined by the media exceeds that known to be '
    + 'available.';
  RSWarningMisc =
      'Miscellaneous warning';


implementation

end.
