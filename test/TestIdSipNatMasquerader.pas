{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdSipNatMasquerader;

interface

uses
  IdSdp, IdSimpleParser, IdSipInviteModule, IdSipNatMasquerader,
  TestFrameworkSip;

type
  TestTIdSipNatMasquerader = class(TTestCaseTU,
                                   IIdSipInviteModuleListener)
  private
    Masq:    TIdSipNatMasquerader;
    Session: TIdSipInboundSession;

    procedure CheckAddress(const ExpectedAddress: String;
                           Sdp: TIdSdpPayload);
    procedure CheckAddressType(const ExpectedAddressType: TIdIPVersion;
                               Sdp: TIdSdpPayload);
    procedure CheckSdpModified;
    function  DummySdp(const ConnectionAddress: String): String;
    procedure OnInboundCall(UserAgent: TIdSipInviteModule;
                            Session: TIdSipInboundSession);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultAddressType;
    procedure TestSentRequestContactHeaders;
    procedure TestSentRequestContactHeadersNonSip;
    procedure TestSentRequestIPv6Sdp;
    procedure TestSentRequestSdp;
    procedure TestSentRequestViaHeader;
    procedure TestSentResponseContactHeaders;
    procedure TestSentResponseSdp;
  end;

implementation

uses
  IdSipMessage, SysUtils, TestFramework;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdSipNatMasquerader tests');
  Result.AddTest(TestTIdSipNatMasquerader.Suite);
end;

//******************************************************************************
//* TestTIdSipNatMasquerader                                                   *
//******************************************************************************
//* TestTIdSipNatMasquerader Public methods ************************************

procedure TestTIdSipNatMasquerader.SetUp;
var
  I: Integer;
begin
  inherited SetUp;

  Self.Core.InviteModule.AddListener(Self);

  Self.Masq := TIdSipNatMasquerader.Create;
  Self.Masq.NatAddress := '1.2.3.4';

  Self.Dispatcher.TransportType := UdpTransport;

  for I := 0 to Self.Core.Dispatcher.TransportCount - 1 do
    Self.Core.Dispatcher.Transports[I].AddTransportSendingListener(Self.Masq);

  Self.Session := nil;
end;

procedure TestTIdSipNatMasquerader.TearDown;
var
  I: Integer;
begin
  for I := 0 to Self.Core.Dispatcher.TransportCount - 1 do
    Self.Core.Dispatcher.Transports[I].RemoveTransportSendingListener(Self.Masq);

  Self.Masq.Free;

  inherited TearDown;
end;

//* TestTIdSipNatMasquerader Private methods ***********************************

procedure TestTIdSipNatMasquerader.CheckAddress(const ExpectedAddress: String;
                                                Sdp: TIdSdpPayload);
var
  I, J: Integer;
begin
  CheckEquals(ExpectedAddress,
              Sdp.Origin.Address,
              'Origin address');

  for I := 0 to Sdp.ConnectionCount - 1 do
    CheckEquals(ExpectedAddress,
                Sdp.ConnectionAt(I).Address,
                'ConnectionAt(' + IntToStr(I) + ') address');

  for I := 0 to Sdp.MediaDescriptionCount - 1 do
    for J := 0 to Sdp.MediaDescriptionAt(I).Connections.Count - 1 do
    CheckEquals(ExpectedAddress,
                Sdp.MediaDescriptionAt(I).Connections[J].Address,
                'MediaDescriptionAt((' + IntToStr(I) + ').Connections[' + IntToStr(J) + '] address');
end;

procedure TestTIdSipNatMasquerader.CheckAddressType(const ExpectedAddressType: TIdIPVersion;
                                                    Sdp: TIdSdpPayload);
var
  I, J: Integer;
begin
  Check(ExpectedAddressType = Sdp.Origin.AddressType,
        'Origin address type');

  for I := 0 to Sdp.ConnectionCount - 1 do
    Check(ExpectedAddressType = Sdp.ConnectionAt(I).AddressType,
          'ConnectionAt(' + IntToStr(I) + ') address type');

  for I := 0 to Sdp.MediaDescriptionCount - 1 do
    for J := 0 to Sdp.MediaDescriptionAt(I).Connections.Count - 1 do
    Check(ExpectedAddressType = Sdp.MediaDescriptionAt(I).Connections[J].AddressType,
          'MediaDescriptionAt((' + IntToStr(I) + ').Connections[' + IntToStr(J) + '] address type');
end;

procedure TestTIdSipNatMasquerader.CheckSdpModified;
var
  Invite: TIdSipRequest;
  Sdp: TIdSdpPayload;
begin
  Self.MarkSentRequestCount;
  Self.Core.InviteModule.Call(Self.Destination,
                              Self.DummySdp('127.0.0.1'),
                              SdpMimeType).Send;
  Invite := Self.LastSentRequest;

  CheckRequestSent('No INVITE sent');

  Sdp := TIdSdpPayload.CreateFrom(Invite.Body);
  try
    Self.CheckAddress(Self.Masq.NatAddress, Sdp);
    Self.CheckAddressType(Self.Masq.AddressType, Sdp);
  finally
    Sdp.Free;
  end;

  CheckEquals(Invite.ContentLength,
              Length(Invite.Body),
              'Content-Length isn''t correct');
end;

function TestTIdSipNatMasquerader.DummySdp(const ConnectionAddress: String): String;
begin
  // Note the presence of both a session-wide connection header and a per-media
  // connection header.

  Result := 'v=0'#13#10
          + 'o=f00L 123456 123456 IN IP4 ' + ConnectionAddress + #13#10
          + 's=-'#13#10
          + 'c=IN IP4 ' + ConnectionAddress + #13#10
          + 't=0 0'#13#10
          + 'm=audio 8000 RTP/AVP 0'#13#10
          + 'c=IN IP4 ' + ConnectionAddress + #13#10;
end;

procedure TestTIdSipNatMasquerader.OnInboundCall(UserAgent: TIdSipInviteModule;
                                                 Session: TIdSipInboundSession);
begin
  Self.Session := Session;
end;

//* TestTIdSipNatMasquerader Published methods *********************************

procedure TestTIdSipNatMasquerader.TestDefaultAddressType;
begin
  Check(Id_IPv4 = Self.Masq.AddressType,
        'Default address type');
end;

procedure TestTIdSipNatMasquerader.TestSentRequestContactHeaders;
begin
  Self.MarkSentRequestCount;
  Self.Core.InviteModule.Call(Self.Destination, '', '').Send;

  CheckRequestSent('No INVITE sent');
  CheckEquals(Self.Masq.NatAddress,
              Self.LastSentRequest.FirstContact.Address.Host,
              'Contact''s host not set to the NAT''s address');
end;

procedure TestTIdSipNatMasquerader.TestSentRequestContactHeadersNonSip;
const
  OriginalContact = 'mailto:sip@localhost';
var
  NonSipContact: TIdSipContactHeader;
begin
  NonSipContact := Self.Invite.AddHeader(ContactHeaderFull) as TIdSipContactHeader;
  NonSipContact.Value := OriginalContact;

  Self.Masq.RewriteRequest(Self.Invite);

  CheckEquals(OriginalContact,
              NonSipContact.FullValue,
              'Non-SIP Contact altered');
end;

procedure TestTIdSipNatMasquerader.TestSentRequestIPv6Sdp;
begin
  Self.Masq.AddressType := Id_IPv6;
  Self.Masq.NatAddress  := '::1';

  Self.CheckSdpModified;
end;

procedure TestTIdSipNatMasquerader.TestSentRequestSdp;
begin
  Self.Masq.AddressType := Id_IPv4;

  Self.CheckSdpModified;
end;

procedure TestTIdSipNatMasquerader.TestSentRequestViaHeader;
begin
  Self.MarkSentRequestCount;
  Self.Core.InviteModule.Call(Self.Destination, '', '').Send;

  CheckRequestSent('No INVITE sent');
  CheckEquals(Self.Masq.NatAddress,
              Self.LastSentRequest.LastHop.SentBy,
              'Via sent-by not sent');
end;

procedure TestTIdSipNatMasquerader.TestSentResponseContactHeaders;
begin
  // We do this to ensure that the right mock transport gets the message.
  // See TIdSipMockTransactionDispatcher and note that it has several transports,
  // and that the result of Transport (and hence of Self.SentResponseCount)
  // depends on the value of the TransportType property. That's set to UDP in
  // all the tests, but Self.Invite usually has a TCP transport specified in
  // its topmost Via header.
  Self.Invite.LastHop.Transport := Self.Dispatcher.TransportType;

  Self.MarkSentResponseCount;
  Self.ReceiveInvite;

  CheckResponseSent('No Trying or Ringing sent');
  CheckEquals(Self.Masq.NatAddress,
              Self.LastSentResponse.FirstContact.Address.Host,
              'Contact''s host not set to the NAT''s address');
end;

procedure TestTIdSipNatMasquerader.TestSentResponseSdp;
var
  Ok:  TIdSipResponse;
  Sdp: TIdSdpPayload;
begin
  Self.Masq.AddressType := Id_IPv4;

  Ok := TIdSipResponse.InResponseTo(Self.Invite, SIPOK);
  try
    Ok.Body := Self.DummySdp('127.0.0.1');
    Ok.ContentType := SdpMimeType;

    Self.Masq.RewriteResponse(Ok);

    Sdp := TIdSdpPayload.CreateFrom(Ok.Body);
    try
      Self.CheckAddress(Self.Masq.NatAddress, Sdp);
      Self.CheckAddressType(Self.Masq.AddressType, Sdp);
    finally
      Sdp.Free;
    end;

    CheckEquals(Ok.ContentLength, Length(Ok.Body), 'Content-Length isn''t correct');
  finally
    Ok.Free;
  end;
end;

initialization
  RegisterTest('NAT Masquerading', Suite);
end.
