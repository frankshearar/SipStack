{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
program SIP;

uses
  Forms,
  GuiTestRunner,
  SysUtils,
  TextTestRunner,
  audioclasses in 'src\audioclasses.pas',
  ConfigUtils in 'src\ConfigUtils.pas',
  IdAddressSpace in 'src\IdAddressSpace.pas',
  IdBaseThread in 'src\IdBaseThread.pas',
  IdConnectionBindings in 'src\IdConnectionBindings.pas',
  IdDTMFPanel in 'src\IdDTMFPanel.pas',
  IdIndyUtils in 'src\IdIndyUtils.pas',
  IdInterfacedObject in 'src\IdInterfacedObject.pas',
  IdNotification in 'src\IdNotification.pas',
  IdObservable in 'src\IdObservable.pas',
  IdRandom in 'src\IdRandom.pas',
  IdRegisteredObject in 'src\IdRegisteredObject.pas',
  IdRoutingTable in 'src\IdRoutingTable.pas',
  IdRTP in 'src\IdRTP.pas',
  IdRTPDiagnostics in 'src\IdRTPDiagnostics.pas',
  IdRTPServer in 'src\IdRTPServer.pas',
  IdSdp in 'src\IdSdp.pas',
  IdSimpleParser in 'src\IdSimpleParser.pas',
  IdSipAuthentication in 'src\IdSipAuthentication.pas',
  IdSipCore in 'src\IdSipCore.pas',
  IdSipDialog in 'src\IdSipDialog.pas',
  IdSipDialogID in 'src\IdSipDialogID.pas',
  IdSipDns in 'src\IdSipDns.pas',
  IdSipIndyLocator in 'src\IdSipIndyLocator.pas',
  IdSipInviteModule in 'src\IdSipInviteModule.pas',
  IdSipLocation in 'src\IdSipLocation.pas',
  IdSipLocator in 'src\IdSipLocator.pas',
  IdSipMessage in 'src\IdSipMessage.pas',
  IdSipMockBindingDatabase in 'test\IdSipMockBindingDatabase.pas',
  IdSipMockCore in 'test\IdSipMockCore.pas',
  IdSipMockDnsServer in 'test\IdSipMockDnsServer.pas',
  IdSipMockLocator in 'test\IdSipMockLocator.pas',
  IdSipMockTransactionDispatcher in 'test\IdSipMockTransactionDispatcher.pas',
  IdSipMockTransport in 'test\IdSipMockTransport.pas',
  IdSipOptionsModule in 'src\IdSipOptionsModule.pas',
  IdSipProxy in 'src\IdSipProxy.pas',
  IdSipProxyDescription in 'src\IdSipProxyDescription.pas',
  IdSipRegistration in 'src\IdSipRegistration.pas',
  IdSipSctpTransport in 'src\IdSipSctpTransport.pas',
  IdSipStackInterface in 'src\IdSipStackInterface.pas',
  IdSipSubscribeModule in 'src\IdSipSubscribeModule.pas',
  IdSipTcpTransport in 'src\IdSipTcpTransport.pas',
  IdSipTlsOverSctpTransport in 'src\IdSipTlsOverSctpTransport.pas',
  IdSipTlsTransport in 'src\IdSipTlsTransport.pas',
  IdSipTransaction in 'src\IdSipTransaction.pas',
  IdSipTransport in 'src\IdSipTransport.pas',
  IdSipTransportAddressSpace in 'src\IdSipTransportAddressSpace.pas',
  IdSipTransportLogger in 'src\IdSipTransportLogger.pas',
  IdSipUdpTransport in 'src\IdSipUdpTransport.pas',
  IdSipUserAgent in 'src\IdSipUserAgent.pas',
  IdSystem in 'src\IdSystem.pas',
  IdThreadableTcpClient in 'src\IdThreadableTcpClient.pas',
  IdTimerQueue in 'src\IdTimerQueue.pas',
  IdUnicode in 'src\IdUnicode.pas',
  PluggableLogging in 'src\PluggableLogging.pas',
  RuntimeSafety in 'src\RuntimeSafety.pas',
  StringDictionary in 'src\StringDictionary.pas',
  TestFrameworkEx in 'test\TestFrameworkEx.pas',
  TestFrameworkRtp in 'test\TestFrameworkRtp.pas',
  TestFrameworkSip in 'test\TestFrameworkSip.pas',
  TestFrameworkSipTransport in 'test\TestFrameworkSipTransport.pas',
  TestFrameworkSipTU in 'test\TestFrameworkSipTU.pas',
  TestFrameworkStackInterface in 'test\TestFrameworkStackInterface.pas',
  TestFrameworkTimerQueue in 'test\TestFrameworkTimerQueue.pas',
  TestMessages in 'test\TestMessages.pas',
  TestConfigUtils in 'test\TestConfigUtils.pas',
  TestIdAddressSpace in 'test\TestIdAddressSpace.pas',
  TestIdConnectionBindings in 'test\TestIdConnectionBindings.pas',
  TestIdIndyUtils in 'test\TestIdIndyUtils.pas',
  TestIdNotification in 'test\TestIdNotification.pas',
  TestIdObservable in 'test\TestIdObservable.pas',
  TestIdRandom in 'test\TestIdRandom.pas',
  TestIdRegisteredObject in 'test\TestIdRegisteredObject.pas',
  TestIdRoutingTable in 'test\TestIdRoutingTable.pas',
  TestIdRTP in 'test\TestIdRTP.pas',
  TestIdRTPDiagnostics in 'test\TestIdRTPDiagnostics.pas',
  TestIdRTPServer in 'test\TestIdRTPServer.pas',
  TestIdSdp in 'test\TestIdSdp.pas',
  TestIdSimpleParser in 'test\TestIdSimpleParser.pas',
  TestIdSipAuthentication in 'test\TestIdSipAuthentication.pas',
  TestIdSipCore in 'test\TestIdSipCore.pas',
  TestIdSipDialog in 'test\TestIdSipDialog.pas',
  TestIdSipDialogID in 'test\TestIdSipDialogID.pas',
  TestIdSipDns in 'test\TestIdSipDns.pas',
  TestIdSipHeaders in 'test\TestIdSipHeaders.pas',
  TestIdSipIndyLocator in 'test\TestIdSipIndyLocator.pas',
  TestIdSipInviteModule in 'test\TestIdSipInviteModule.pas',
  TestIdSipLocation in 'test\TestIdSipLocation.pas',
  TestIdSipLocator in 'test\TestIdSipLocator.pas',
  TestIdSipMessage in 'test\TestIdSipMessage.pas',
  TestIdSipOptionsModule in 'test\TestIdSipOptionsModule.pas',
  TestIdSipParser in 'test\TestIdSipParser.pas',
  TestIdSipProxy in 'test\TestIdSipProxy.pas',
  TestIdSipProxyDescription in 'test\TestIdSipProxyDescription.pas',
  TestIdSipRegistrar in 'test\TestIdSipRegistrar.pas',
  TestIdSipRegistration in 'test\TestIdSipRegistration.pas',
  TestIdSipStackInterface in 'test\TestIdSipStackInterface.pas',
  TestIdSipSubscribeModule in 'test\TestIdSipSubscribeModule.pas',
  TestIdSipSctpTransport in 'test\TestIdSipSctpTransport.pas',
  TestIdSipTcpTransport in 'test\TestIdSipTcpTransport.pas',
  TestIdSipTlsTransport in 'test\TestIdSipTlsTransport.pas',
  TestIdSipTransaction in 'test\TestIdSipTransaction.pas',
  TestIdSipTransport in 'test\TestIdSipTransport.pas',
  TestIdSipTransportAddressSpace in 'test\TestIdSipTransportAddressSpace.pas',
  TestIdSipTransportLogger in 'test\TestIdSipTransportLogger.pas',
  TestIdSipUdpTransport in 'test\TestIdSipUdpTransport.pas',
  TestIdSipUserAgent in 'test\TestIdSipUserAgent.pas',
  TestIdSipUri in 'test\TestIdSipUri.pas',
  TestIdSystem in 'test\TestIdSystem.pas',
  TestIdTimerQueue in 'test\TestIdTimerQueue.pas',
  TestIdUnicode in 'test\TestIdUnicode.pas',
  TestStringDictionary in 'test\TestStringDictionary.pas';

{,
  TestIdSipTlsServer in 'test\TestIdSipTlsServer.pas'}

//{$DEFINE SPIKE}
//{$DEFINE GUI}

{$IFNDEF GUI}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}

begin
  {$IFDEF SPIKE}
  Forms.Application.Initialize;
//  Forms.Application.CreateForm(TrnidSpike, rnidSpike);
  Forms.Forms.Application.Run;
  {$ELSE}
    {$IFDEF GUI}
    GuiTestRunner.RunRegisteredTests;
    {$ELSE}
    TextTestRunner.RunRegisteredTests;
    {$ENDIF}
  {$ENDIF}
end.
