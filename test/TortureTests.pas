unit TortureTests;

interface

const
  // Each constant is a message defined in
  // http://www.ietf.org/internet-drafts/draft-ietf-sipping-torture-tests-00.txt
  // TortureTextN is defined in section 2.N of the above.

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
