unit TortureTests;

interface

const
  // Each constant is a message defined in
  // http://www.ietf.org/internet-drafts/draft-ietf-sipping-torture-tests-00.txt
  // TortureTextN is defined in section 2.N of the above.

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

  //   This INVITE is illegal because the Request-URI has been enclosed
  //   within in "<>".
  //   An intelligent server may be able to deal with this and fix up
  //   athe Request-URI if acting as a Proxy. If not it should respond 400
  //   with an appropriate reason phrase.
  TortureTest21 = 'INVITE <sip:user@company.com> SIP/2.0'#13#10
                + 'To: sip:user@company.com'#13#10
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

implementation

end.
