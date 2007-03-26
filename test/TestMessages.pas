{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestMessages;

interface

const
  BasicBody = 'I am a message. Hear me roar!';
  BasicContentLengthHeader = 'Content-Length: 29'#13#10;
  BasicRequest = 'INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
               + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
               + 'Max-Forwards: 70'#13#10
               + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>;tag=1928301775'#13#10
               + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
               + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
               + 'CSeq: 314159 INVITE'#13#10
               + 'Contact: sip:wintermute@tessier-ashpool.co.luna'#13#10
               + 'Content-Type: text/plain'#13#10
               + BasicContentLengthHeader
               + #13#10
               + BasicBody;
  BasicResponse = 'SIP/2.0 486 Busy Here'#13#10
                + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>;tag=1928301775'#13#10
                + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                + 'CSeq: 314159 INVITE'#13#10
                + 'Contact: sip:wintermute@tessier-ashpool.co.luna'#13#10
                + 'Content-Type: text/plain'#13#10
                + BasicContentLengthHeader
                + #13#10
                + BasicBody;
  EmptyRequest = 'INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
               + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
               + 'Max-Forwards: 70'#13#10
               + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>;tag=1928301775'#13#10
               + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
               + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
               + 'CSeq: 314159 INVITE'#13#10
               + 'Contact: sip:wintermute@tessier-ashpool.co.luna'#13#10;
  ExtensiveRequest = 'INVITE sip:wintermute@tessier-ashpool.co.luna SIP/2.0'#13#10
                   + 'Accept: text/t140;q=1.0, text/plain;q=0.7;foo=bar, text/xml'#13#10
                   + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                   + 'Contact: sip:wintermute@tessier-ashpool.co.luna'#13#10
                   + BasicContentLengthHeader
                   + 'Content-Type: text/plain'#13#10
                   + 'CSeq: 314159 INVITE'#13#10
                   + 'Date: Thu, 1 Jan 1970 00:00:00 GMT'#13#10
                   + 'Expires: 1000'#13#10
                   + 'From: Case <sip:case@fried.neurons.org>;tag=1928301774'#13#10
                   + 'Max-Forwards: 70'#13#10
                   + 'Record-Route: localhost <sip:127.0.0.1;lr>'#13#10
                   + 'Route: localhost <sip:127.0.0.1;lr>'#13#10
                   + 'Subject: I am a SIP request with every legal header (even an extension)'#13#10
                   + 'To: Wintermute <sip:wintermute@tessier-ashpool.co.luna>;tag=1928301775'#13#10
                   + 'Via: SIP/2.0/TCP gw1.leo-ix.org;branch=z9hG4bK776asdhds'#13#10
                   + 'Warning: 301 draugr "Not really interested"'#13#10
                   + 'X-Not-A-Header: I am not defined in RFC 3261'#13#10
                   + #13#10
                   + BasicBody;

  LocalLoopRequest = 'INVITE sip:franks@127.0.0.1 SIP/2.0'#13#10
                   + 'Via: SIP/2.0/TCP 127.0.0.1;branch=z9hG4bK776asdhds'#13#10
                   + 'Max-Forwards: 70'#13#10
                   + 'To: Wintermute <sip:franks@127.0.0.1>'#13#10
                   + 'From: Case <sip:franks@127.0.0.1>;tag=1928301774'#13#10
                   + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                   + 'CSeq: 314159 INVITE'#13#10
                   + 'Contact: sip:franks@127.0.0.1'#13#10
                   + 'Content-Type: text/plain'#13#10
                   + BasicContentLengthHeader
                   + #13#10
                   + BasicBody;
  LocalLoopResponse = 'SIP/2.0 486 Busy Here'#13#10
                + 'Via: SIP/2.0/TCP 127.0.0.1;branch=z9hG4bK776asdhds'#13#10
                + 'To: Wintermute <sip:franks@127.0.0.1>;tag=1928301775'#13#10
                + 'From: Case <sip:franks@127.0.0.1>;tag=1928301774'#13#10
                + 'Call-ID: a84b4c76e66710@gw1.leo-ix.org'#13#10
                + 'CSeq: 314159 INVITE'#13#10
                + 'Contact: sip:franks@127.0.0.1'#13#10
                + 'Content-Type: text/plain'#13#10
                + 'Content-Length: 0'#13#10
                + #13#10;

  // Each TortureTestN constant is a message defined in
  // http://www.ietf.org/internet-drafts/draft-ietf-sipping-torture-tests-00.txt
  // TortureTextN is defined in section 2.N of the above.
  // Each torture test has a string '%s' that you can StringReplace with the
  // address/port of the server you wish to test.

  //   This message is a correctly formatted SIP message. It contains:
  //
  //   line folding all over
  //   escaped characters within quotes
  //   LWS between colons, semicolons, headers, and other fields
  //   both comma separated and separate listing of headers
  //   mix or short and long form for the same header
  //   unknown header field
  //   unusual header ordering
  //   unknown parameters of a known header
  //
  //   Proxies should forward message and clients should respond as to a
  //   normal INVITE message.
  TortureTest1 = 'INVITE sip:vivekg@chair.dnrc.bell-labs.com SIP/2.0'#13#10
               + 'TO :'#13#10
               + ' sip:vivekg@chair.dnrc.bell-labs.com ;   tag    = 1918181833n'#13#10
               + 'From   : "J Rosenberg \\\"" <sip:jdrosen@lucent.com>'#13#10
               + '  ;'#13#10
               + '  tag = 98asjd8'#13#10
               + 'Max-Forwards: 6'#13#10
               + 'Call-ID: 0ha0isndaksdj@10.1.1.1'#13#10
               + 'cseq: 8'#13#10
               + '  INVITE'#13#10
               + 'Via  : SIP  /   2.0'#13#10
               + ' /UDP'#13#10
               + '    135.180.130.133;branch=z9hG4bKkdjuw'#13#10
               + 'Subject :'#13#10
               + 'NewFangledHeader:   newfangled value'#13#10
               + ' more newfangled value'#13#10
               + 'Content-Type: application/sdp'#13#10
               + 'v:  SIP  / 2.0  / TCP     1192.168.156.222   ;'#13#10
               + '  branch  =   9ikj8  ,'#13#10
               + ' SIP  /    2.0   / UDP  192.168.255.111   ; hidden'#13#10
               + 'm:"Quoted string \"\"" <sip:jdrosen@bell-labs.com> ; newparam ='#13#10
               // http://www.ietf.org/internet-drafts/draft-ietf-sipping-torture-tests-00.txt
               // claims that this line starts with no space. That's illegal syntax though.
               // Therefore we use a TAB just to make things difficult for the parser.
               + #9'newvalue ;'#13#10
               + '  secondparam = secondvalue  ; q = 0.33,'#13#10
               + ' tel:4443322'#13#10
               + #13#10
               + 'v=0'#13#10
               + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
               + 's=-'#13#10
               + 'c=IN IP4 135.180.130.88'#13#10
               + 't=0 0'#13#10        
               + 'm=audio 492170 RTP/AVP 0 12'#13#10
               + 'm=video 3227 RTP/AVP 31'#13#10
               + 'a=rtpmap:31 LPC';

  //   This message is nearly identical to the Unknown Method message. It is
  //   a request with a new unknown method, but with a CSeq method tag which
  //   does not match.
  //
  //   A proxy should either respond with an error, or correct the method
  //   tag. The user agent should reject it with an error, and list the
  //   available methods in the response.
  TortureTest8 = 'NEWMETHOD sip:user@%s SIP/2.0'#13#10
               + 'To: sip:j.user@%s'#13#10
               + 'From: sip:caller@university.edu;tag=23411413'#13#10
               + 'Max-Forwards: 3'#13#10
               + 'Call-ID: 0ha0isndaksdj@10.0.1.1'#13#10
               + 'CSeq: 8 INVITE'#13#10
               + 'Via: SIP/2.0/UDP 135.180.130.133;branch=z9hG4bKkdjuw'#13#10
               + 'Content-Type: application/sdp'#13#10
               + #13#10
               + 'v=0'#13#10
               + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
               + 's=-'#13#10
               + 'c=IN IP4 135.180.130.88'#13#10
               + 't=0 0'#13#10
               + 'm=audio 492170 RTP/AVP 0 12'#13#10
               + 'm=video 3227 RTP/AVP 31'#13#10
               + 'a=rtpmap:31 LPC';

  //   This message contains no Call-ID, From, or To header.
  //
  //   The server should not crash, and ideally should respond with an
  //   error
  TortureTest11 = 'INVITE sip:user@%s SIP/2.0'#13#10
                + 'CSeq: 0 INVITE'#13#10
                + 'Via: SIP/2.0/UDP 135.180.130.133;branch=z9hG4bKkdjuw'#13#10
                + 'Content-Type: application/sdp'#13#10
                + #13#10
                + 'v=0'#13#10
                + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                + 's=-'#13#10
                + 'c=IN IP4 135.180.130.88'#13#10
                + 't=0 0'#13#10
                + 'm=audio 492170 RTP/AVP 0 12'#13#10
                + 'm=video 3227 RTP/AVP 31'#13#10
                + 'a=rtpmap:31 LPC';

  //   This message contains an Expires header which has illegal values for
  //   a number of components, but otherwise is syntactically correct.
  TortureTest13 = 'INVITE sip:user@%s SIP/2.0'#13#10
                + 'Via: SIP/2.0/UDP 135.180.130.133;branch=z9hG4bKkdjuw'#13#10
                + 'Max-Forwards: 88'#13#10
                + 'CSeq: 0 INVITE'#13#10
                + 'Call-ID: 98asdh@10.1.1.2'#13#10
                + 'Expires: Thu, 44 Dec 19999 16:00:00 EDT'#13#10
                + 'From: sip:caller@university.edu;tag=3651'#13#10
                + 'To: sip:user@%s'#13#10
                + 'Content-Type: application/sdp'#13#10
                + #13#10
                + 'v=0'#13#10
                + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                + 's=-'#13#10
                + 'c=IN IP4 135.180.130.88'#13#10
                + 't=0 0'#13#10
                + 'm=audio 492170 RTP/AVP 0 12'#13#10
                + 'm=video 3227 RTP/AVP 31'#13#10
                + 'a=rtpmap:31 LPC';

  //   This is a request with the Via and Contact headers incorrect. They
  //   contain additional semicolons and commas without parameters or
  //   values.
  //
  //   The server should respond with a Bad Request error.
  TortureTest15 = 'INVITE sip:user@%s SIP/2.0'#13#10
                + 'To: sip:j.user@%s'#13#10
                + 'From: sip:caller@university.edu;tag=134161461246'#13#10
                + 'Max-Forwards: 7'#13#10
                + 'Call-ID: 0ha0isndaksdj@10.0.0.1'#13#10
                + 'CSeq: 8 INVITE'#13#10
                + 'Via: SIP/2.0/UDP 135.180.130.133;;,;'#13#10
                + 'Contact: "" <> ;,"Joe" <sip:joe@org.org>;;,,;;'#13#10
                + 'Content-Type: application/sdp'#13#10
                + #13#10
                + 'v=0'#13#10
                + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                + 's=-'#13#10
                + 'c=IN IP4 135.180.130.88'#13#10
                + 't=0 0'#13#10
                + 'm=audio 492170 RTP/AVP 0 12'#13#10
                + 'm=video 3227 RTP/AVP 31'#13#10
                + 'a=rtpmap:31 LPC';

  //   This is a request message with a Content Length that is much larger
  //   than the length of the body.
  //
  //   When sent UDP, the server should respond with an error. With TCP,
  //   there's not much you can do but wait...
  TortureTest16 = 'INVITE sip:user@%s SIP/2.0'#13#10
                + 'Max-Forwards: 80'#13#10
                + 'To: sip:j.user@%s'#13#10
                + 'From: sip:caller@university.edu;tag=93942939o2'#13#10
                + 'Call-ID: 0ha0isndaksdj@10.0.0.1'#13#10
                + 'CSeq: 8 INVITE'#13#10
                + 'Via: SIP/2.0/UDP 135.180.130.133'#13#10
                + 'Content-Type: application/sdp'#13#10
                + 'Content-Length: 9999'#13#10
                + #13#10
                + 'v=0'#13#10
                + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                + 's=-'#13#10
                + 'c=IN IP4 135.180.130.88'#13#10
                + 't=0 0'#13#10
                + 'm=audio 492170 RTP/AVP 0 12'#13#10
                + 'm=video 3227 RTP/AVP 31'#13#10
                + 'a=rtpmap:31 LPC';


  //   This is a request message with a negative value for Content-Length.
  //
  //   The server should respond with an error.
  TortureTest17 = 'INVITE sip:user@%s SIP/2.0'#13#10
                + 'Max-Forwards: 254'#13#10
                + 'To: sip:j.user@%s'#13#10
                + 'From: sip:caller@university.edu;tag=3'#13#10
                + 'Call-ID: 0ha0isndaksdj@10.0.0.1'#13#10
                + 'CSeq: 8 INVITE'#13#10
                + 'Via: SIP/2.0/UDP 135.180.130.133;branch=z9hG4bKkdjuw'#13#10
                + 'Content-Type: application/sdp'#13#10
                + 'Content-Length: -999'#13#10
                + #13#10
                + 'v=0'#13#10
                + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                + 's=-'#13#10
                + 'c=IN IP4 135.180.130.88'#13#10
                + 't=0 0'#13#10;

  //   This is a request with an unterminated quote in the display name of
  //   the To field.
  //
  //   The server can either return an error, or proxy it if it is
  //   successful parsing without the terminating quote.
  TortureTest19 = 'INVITE sip:user@%s SIP/2.0'#13#10
                + 'To: "Mr. J. User <sip:j.user@%s>'#13#10
                + 'From: sip:caller@university.edu;tag=93334'#13#10
                + 'Max-Forwards: 10'#13#10
                + 'Call-ID: 0ha0isndaksdj@10.0.0.1'#13#10
                + 'CSeq: 8 INVITE'#13#10
                + 'Via: SIP/2.0/UDP 135.180.130.133:5050;branch=z9hG4bKkdjuw'#13#10
                + 'Content-Type: application/sdp'#13#10
                + 'Content-Length: 138'#13#10
                + #13#10
                + 'v=0'#13#10
                + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                + 's=-'#13#10
                + 'c=IN IP4 135.180.130.88'#13#10
                + 't=0 0'#13#10
                + 'm=audio 492170 RTP/AVP 0 12'#13#10
                + 'm=video 3227 RTP/AVP 31'#13#10
                + 'a=rtpmap:31 LPC';

  //   This INVITE is illegal because the Request-URI has been enclosed
  //   within in "<>".
  //   An intelligent server may be able to deal with this and fix up
  //   athe Request-URI if acting as a Proxy. If not it should respond 400
  //   with an appropriate reason phrase.
  TortureTest21 = 'INVITE <sip:user@%s> SIP/2.0'#13#10
                + 'To: sip:user@%s'#13#10
                + 'From: sip:caller@university.edu;tag=39291'#13#10
                + 'Max-Forwards: 23'#13#10
                + 'Call-ID: 1@10.0.0.1'#13#10
                + 'CSeq: 1 INVITE'#13#10
                + 'Via: SIP/2.0/UDP 135.180.130.133'#13#10
                + 'Content-Type: application/sdp'#13#10
                + 'Content-Length: 174'#13#10
                + #13#10
                + 'v=0'#13#10
                + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                + 's=-'#13#10
                + 'c=IN IP4 135.180.130.88'#13#10
                + 't=3149328700 0'#13#10
                + 'm=audio 492170 RTP/AVP 0 12'#13#10
                + 'm=video 3227 RTP/AVP 31'#13#10
                + 'a=rtpmap:31 LPC';

  //   This INVITE has illegal LWS within the SIP URI.
  //
  //   An intelligent server may be able to deal with this and fix up
  //   the Request-URI if acting as a Proxy. If not it should respond 400
  //   with an appropriate reason phrase.
  TortureTest22 = 'INVITE sip:user@%s; transport=udp SIP/2.0'#13#10
                + 'To: sip:user@%s'#13#10
                + 'From: sip:caller@university.edu;tag=231413434'#13#10
                + 'Max-Forwards: 5'#13#10
                + 'Call-ID: 2@10.0.0.1'#13#10
                + 'CSeq: 1 INVITE'#13#10
                + 'Via: SIP/2.0/UDP 135.180.130.133:5060;branch=z9hG4bKkdjuw'#13#10
                + 'Content-Type: application/sdp'#13#10
                + 'Content-Length: 174'#13#10
                + #13#10
                + 'v=0'#13#10
                + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                + 's=-'#13#10
                + 'c=IN IP4 135.180.130.88'#13#10
                + 't=3149328700 0'#13#10
                + 'm=audio 492170 RTP/AVP 0 12'#13#10
                + 'm=video 3227 RTP/AVP 31'#13#10
                + 'a=rtpmap:31 LPC';

  //   This INVITE has illegal >1 SP between elements of the Request-Line.
  //
  //   An intelligent server may be able to deal with this and fix up
  //   the Request-URI if acting as a Proxy. If not it should respond 400
  //   with an appropriate reason phrase.
  TortureTest23 = 'INVITE sip:user@%s  SIP/2.0'#13#10
                + 'Max-Forwards: 8'#13#10
                + 'To: sip:user@%s'#13#10
                + 'From: sip:caller@university.edu;tag=8814'#13#10
                + 'Call-ID: 3@10.0.0.1'#13#10
                + 'CSeq: 1 INVITE'#13#10
                + 'Via: SIP/2.0/UDP 135.180.130.133:5060;branch=z9hG4bKkdjuw'#13#10
                + 'Content-Type: application/sdp'#13#10
                + 'Content-Length: 174'#13#10
                + #13#10
                + 'v=0'#13#10
                + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                + 's=-'#13#10
                + 'c=IN IP4 135.180.130.88'#13#10
                + 't=0 0'#13#10
                + 'm=audio 492170 RTP/AVP 0 12'#13#10
                + 'm=video 3227 RTP/AVP 31'#13#10
                + 'a=rtpmap:31 LPC';

  //   This INVITE is legal and has a Request-URI with a SIP URI containing
  //   escaped characters.
  TortureTest24 = 'INVITE sip:sip%3Auser%40example.com@%s;other-param=summit SIP/2.0'#13#10
                + 'To: sip:user@%s'#13#10
                + 'From: sip:caller@university.edu;tag=938'#13#10
                + 'Max-Forwards: 87'#13#10
                + 'Call-ID: 4@10.0.0.1'#13#10
                + 'CSeq: 1 INVITE'#13#10
                + 'Via: SIP/2.0/UDP 135.180.130.133:5060;branch=z9hG4bKkdjuw'#13#10
                + 'Content-Type: application/sdp'#13#10
                + 'Content-Length: 174'#13#10
                + #13#10
                + 'v=0'#13#10
                + 'o=mhandley 29739 7272939 IN IP4 126.5.4.3'#13#10
                + 's=-'#13#10
                + 'c=IN IP4 135.180.130.88'#13#10
                + 't=0 0'#13#10
                + 'm=audio 492170 RTP/AVP 0 12'#13#10
                + 'm=video 3227 RTP/AVP 31'#13#10
                + 'a=rtpmap:31 LPC';

  //   This is an illegal and badly mangled message.
  //
  //   A server should respond 400 with an appropriate reason phrase if it
  //   can. It may just drop this message.
  TortureTest35 = 'OPTIONS sip:%s SIP/2.0'#13#10
                + 'Via: SIP/2.0/UDP %s'#13#10
                + 'Max-Forwards: 70'#13#10
                + 'From: sip:iuser@company.com;tag=74345345'#13#10
                + 'To: sip:user@135.180.130.133'#13#10
                + 'Call-ID: 1804928587@company.com'#13#10
                + 'CSeq: 1 OPTIONS'#13#10
                + 'Expires: 0 0l@company.com'#13#10               // mangled
                + 'To: sip:user@135.180.130.133'#13#10            // 2nd To header
                + 'Call-ID: 1804928587@company.com'#13#10         // 2nd Call-ID
                + 'CSeq: 1 OPTIONS'#13#10                         // 2nd CSeq
                + 'Contact: sip:host.company.com'#13#10
                + 'Expires: 0xpires: 0sip:host.company.com'#13#10 // mangled
                + 'Expires: 0'#13#10
                + 'Contact: sip:host.company.com'#13#10
                + #13#10;

  //   This is an illegal invite as the display names in the To and From
  //   headers contain non-token characters but are unquoted.
  //
  //   A server may be intelligent enough to cope with this but may also
  //   return a 400 response with an appropriate reason phrase.

  TortureTest40 = 'INVITE sip:t.watson@%s SIP/2.0'#13#10
                + 'Via:     SIP/2.0/UDP c.bell-tel.com:5060;branch=z9hG4bKkdjuw'#13#10
                + 'Max-Forwards:      70'#13#10
                + 'From:    Bell, Alexander <sip:a.g.bell@bell-tel.com>;tag=43'#13#10
                + 'To:      Watson, Thomas <sip:t.watson@%s>'#13#10
                + 'Call-ID: 31415@c.bell-tel.com'#13#10
                + 'CSeq:    1 INVITE'#13#10
                + #13#10;

  //   This is an illegal INVITE as the SIP Protocol version is unknown.
  //
  //   The server should respond to the request with a bad version error.
  TortureTest41 = 'INVITE sip:t.watson@%s SIP/7.0'#13#10
                + 'Via:     SIP/2.0/UDP c.bell-tel.com;branch=z9hG4bKkdjuw'#13#10
                + 'Max-Forwards:     70'#13#10
                + 'From:    A. Bell <sip:a.g.bell@bell-tel.com>;tag=qweoiqpe'#13#10
                + 'To:      T. Watson <sip:t.watson@%s>'#13#10
                + 'Call-ID: 31417@c.bell-tel.com'#13#10
                + 'CSeq:    1 INVITE'#13#10
                + #13#10;

implementation

end.
