unit IdSipConsts;

interface

// for IdResourceStrings ?
const
  AcceptHeader               = 'Accept';
  AcceptEncodingHeader       = 'Accept-Encoding';
  AcceptLanguageHeader       = 'Accept-Language';
  AlertInfoHeader            = 'Alert-Info';
  AllowHeader                = 'Allow';
  AuthenticationInfoHeader   = 'Authentication-Info';
  AuthorizationHeader        = 'Authorization';
  BranchMagicCookie          = 'z9hG4bK';
  BranchParam                = 'branch';
  CallIDHeaderFull           = 'Call-ID';
  CallIDHeaderShort          = 'i';
  CallInfoHeader             = 'Call-Info';
  ContactHeaderFull          = 'Contact';
  ContactHeaderShort         = 'm';
  ContentDispositionHeader   = 'Content-Disposition';
  ContentEncodingHeaderFull  = 'Content-Encoding';
  ContentEncodingHeaderShort = 'e';
  ContentLanguageHeader      = 'Content-Language';
  ContentLengthHeaderFull    = 'Content-Length';
  ContentLengthHeaderShort   = 'l';
  ContentTypeHeaderFull      = 'Content-Type';
  ContentTypeHeaderShort     = 'c';
  CSeqHeader                 = 'CSeq';
  DefaultMaxForwards         = 70;
  DateHeader                 = 'Date';
  ErrorInfoHeader            = 'Error-Info';
  ExpiresHeader              = 'Expires';
  ExpiresParam               = ExpiresHeader;
  FromHeaderFull             = 'From';
  FromHeaderShort            = 'f';
  InReplyToHeader            = 'In-Reply-To';
  LrParam                    = 'lr';
  MaddrParam                 = 'maddr';
  MaxForwardsHeader          = 'Max-Forwards';
  MethodAck                  = 'ACK';
  MethodBye                  = 'BYE';
  MethodCancel               = 'CANCEL';
  MethodInvite               = 'INVITE';
  MethodOptions              = 'OPTIONS';
  MethodRegister             = 'REGISTER';
  MIMEVersionHeader          = 'MIME-Version';
  MinExpiresHeader           = 'Min-Expires';
  OrganizationHeader         = 'Organization';
  PriorityHeader             = 'Priority';
  ProxyAuthenticateHeader    = 'Proxy-Authenticate';
  ProxyAuthorizationHeader   = 'Proxy-Authorization';
  ProxyRequireHeader         = 'Proxy-Require';
  QParam                     = 'q';
  ReceivedParam              = 'received';
  RecordRouteHeader          = 'Record-Route';
  ReplyToHeader              = 'Reply-To';
  RequireHeader              = 'Require';
  RetryAfterHeader           = 'Retry-After';
  RouteHeader                = 'Route';
  ServerHeader               = 'Server';
  SipName                    = 'SIP';
  SipScheme                  = 'sip';
  SipsScheme                 = 'sips';
  SubjectHeaderFull          = 'Subject';
  SubjectHeaderShort         = 's';
  SupportedHeaderFull        = 'Supported';
  SupportedHeaderShort       = 'k';
  TagParam                   = 'tag';
  TimestampHeader            = 'Timestamp';
  ToHeaderFull               = 'To';
  ToHeaderShort              = 't';
  TTLParam                   = 'ttl';
  UnsupportedHeader          = 'Unsupported';
  UserAgentHeader            = 'User-Agent';
  ViaHeaderFull              = 'Via';
  ViaHeaderShort             = 'v';
  WarningHeader              = 'Warning';
  WWWAuthenticateHeader      = 'WWW-Authenticate';

const
  SIPVersion = SipName + '/2.0';  

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
  RSSIPTemporarilyNotAvailable          = 'Temporarily not available';
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
  SIPTemporarilyNotAvailable          = 480;
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

// MIME types
const
  SdpMimeType  = 'application/sdp';
  T140MimeType = 'text/t140';

implementation

end.
