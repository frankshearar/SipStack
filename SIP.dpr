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
  IdBaseThread in 'src\IdBaseThread.pas',
  IdDTMFPanel in 'src\IdDTMFPanel.pas',
  IdInterfacedObject in 'src\IdInterfacedObject.pas',
  IdNotification in 'src\IdNotification.pas',
  IdObservable in 'src\IdObservable.pas',
  IdRandom in 'src\IdRandom.pas',
  IdRTP in 'src\IdRTP.pas',
  IdRTPDiagnostics in 'src\IdRTPDiagnostics.pas',
  IdRTPServer in 'src\IdRTPServer.pas',
  IdSdp in 'src\IdSdp.pas',
  IdSimpleParser in 'src\IdSimpleParser.pas',
  IdSipAuthentication in 'src\IdSipAuthentication.pas',
  IdSipConsts in 'src\IdSipConsts.pas',
  IdSipCore in 'src\IdSipCore.pas',
  IdSipDialog in 'src\IdSipDialog.pas',
  IdSipDialogID in 'src\IdSipDialogID.pas',
  IdSipDns in 'src\IdSipDns.pas',
  IdSipIndyLocator in 'src\IdSipIndyLocator.pas',
  IdSipLocator in 'src\IdSipLocator.pas',
  IdSipMessage in 'src\IdSipMessage.pas',
  IdSipMockBindingDatabase in 'test\IdSipMockBindingDatabase.pas',
  IdSipMockCore in 'test\IdSipMockCore.pas',
  IdSipMockLocator in 'test\IdSipMockLocator.pas',
  IdSipMockTransactionDispatcher in 'test\IdSipMockTransactionDispatcher.pas',
  IdSipMockTransport in 'test\IdSipMockTransport.pas',
  IdSipProxy in 'src\IdSipProxy.pas',
  IdSipRegistration in 'src\IdSipRegistration.pas',
  IdSipServerNotifier in 'src\IdSipServerNotifier.pas',
  IdSipStackInterface in 'src\IdSipStackInterface.pas',
  IdSipSubscribeModule in 'src\IdSipSubscribeModule.pas',
  IdSipTcpClient in 'src\IdSipTcpClient.pas',
  IdSipTcpServer in 'src\IdSipTcpServer.pas',
  IdSipTlsOverSctpTransport in 'src\IdSipTlsOverSctpTransport.pas',
  IdSipTlsServer in 'src\IdSipTlsServer.pas',
  IdSipTransaction in 'src\IdSipTransaction.pas',
  IdSipTransport in 'src\IdSipTransport.pas',
  IdSipTransportLogger in 'src\IdSipTransportLogger.pas',
  IdSipUdpClient in 'src\IdSipUdpClient.pas',
  IdSipUdpServer in 'src\IdSipUdpServer.pas',
  IdSystem in 'src\IdSystem.pas',
  IdTimerQueue in 'src\IdTimerQueue.pas',
  IdUnicode in 'src\IdUnicode.pas',
  Spike in 'test\Spike.pas' {rnidSpike},
  SpikeRegister in 'test\SpikeRegister.pas' {rnidSpikeRegister},
  SpikeRegistrar in 'test\SpikeRegistrar.pas' {rnidSpikeRegistrar},
  SpikeT140 in 'test\SpikeT140.pas' {IdSpikeT140},
  StackWindow in 'test\StackWindow.pas' {StackWindow},
  TestFrameworkEx in 'test\TestFrameworkEx.pas',
  TestFrameworkRtp in 'test\TestFrameworkRtp.pas',
  TestFrameworkSip in 'test\TestFrameworkSip.pas',
  TestMessages in 'test\TestMessages.pas',
  TestIdNotification in 'test\TestIdNotification.pas',
  TestIdObservable in 'test\TestIdObservable.pas',
  TestIdRandom in 'test\TestIdRandom.pas',
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
  TestIdSipLocator in 'test\TestIdSipLocator.pas',
  TestIdSipMessage in 'test\TestIdSipMessage.pas',
  TestIdSipParser in 'test\TestIdSipParser.pas',
  TestIdSipProxy in 'test\TestIdSipProxy.pas',
  TestIdSipRegistrar in 'test\TestIdSipRegistrar.pas',
  TestIdSipRegistration in 'test\TestIdSipRegistration.pas',
  TestIdSipServerNotifier in 'test\TestIdSipServerNotifier.pas',
  TestIdSipStackInterface in 'test\TestIdSipStackInterface.pas',
  TestIdSipSubscribeModule in 'test\TestIdSipSubscribeModule.pas',
  TestIdSipTcpClient in 'test\TestIdSipTcpClient.pas',
  TestIdSipTcpServer in 'test\TestIdSipTcpServer.pas',
  TestIdSipTransaction in 'test\TestIdSipTransaction.pas',
  TestIdSipTransport in 'test\TestIdSipTransport.pas',
  TestIdSipTransportLogger in 'test\TestIdSipTransportLogger.pas',
  TestIdSipUdpServer in 'test\TestIdSipUdpServer.pas',
  TestIdSipUri in 'test\TestIdSipUri.pas',
  TestIdTimerQueue in 'test\TestIdTimerQueue.pas',
  TestIdUnicode in 'test\TestIdUnicode.pas';

{,
  TestIdSipTlsServer in 'test\TestIdSipTlsServer.pas'}

//{$DEFINE SPIKE}
{$DEFINE GUI}

{$IFNDEF GUI}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}

begin
  {$IFDEF SPIKE}
  Forms.Application.Initialize;
  Forms.Application.CreateForm(TrnidSpike, rnidSpike);
  Forms.Application.Run;
  {$ELSE}
    {$IFDEF GUI}
    GuiTestRunner.RunRegisteredTests;
    {$ELSE}
    TextTestRunner.RunRegisteredTests;
    {$ENDIF}
  {$ENDIF}
end.
