program SIP;

uses
  Forms,
  GuiTestRunner,
  TextTestRunner,
  audioclasses in 'src\audioclasses.pas',
  IdRandom in 'src\IdRandom.pas',
  IdRTP in 'src\IdRTP.pas',
  IdRTPServer in 'src\IdRTPServer.pas',
  IdRTPTimerQueue in 'src\IdRTPTimerQueue.pas',
  IdSdp in 'src\IdSdp.pas',
  IdSimpleParser in 'src\IdSimpleParser.pas',
  IdSipConsts in 'src\IdSipConsts.pas',
  IdSipCore in 'src\IdSipCore.pas',
  IdSipDialog in 'src\IdSipDialog.pas',
  IdSipDialogID in 'src\IdSipDialogID.pas',
  IdSipHeaders in 'src\IdSipHeaders.pas',
  IdSipInterfacedObject in 'src\IdSipInterfacedObject.pas',
  IdSipMessage in 'src\IdSipMessage.pas',
  IdSipMockCore in 'test\IdSipMockCore.pas',
  IdSipMockTransactionDispatcher in 'test\IdSipMockTransactionDispatcher.pas',
  IdSipMockTransport in 'test\IdSipMockTransport.pas',
  IdSipTcpClient in 'src\IdSipTcpClient.pas',
  IdSipTcpServer in 'src\IdSipTcpServer.pas',
  IdSipTimer in 'src\IdSipTimer.pas',
  IdSipTlsServer in 'src\IdSipTlsServer.pas',
  IdSipTransaction in 'src\IdSipTransaction.pas',
  IdSipTransport in 'src\IdSipTransport.pas',
  IdSipUdpClient in 'src\IdSipUdpClient.pas',
  IdSipUdpServer in 'src\IdSipUdpServer.pas',
  Spike in 'test\Spike.pas' {rnidSpike},
  SpikeT140 in 'test\SpikeT140.pas' {IdSpikeT140},
  SysUtils,
  TestFrameworkEx in 'test\TestFrameworkEx.pas',
  TestFrameworkRtp in 'test\TestFrameworkRtp.pas',
  TestFrameworkSip in 'test\TestFrameworkSip.pas',
  TestMessages in 'test\TestMessages.pas',
  TestIdRTP in 'test\TestIdRTP.pas',
  TestIdRTPServer in 'test\TestIdRTPServer.pas',
  TestIdRTPTimerQueue in 'test\TestIdRTPTimerQueue.pas',
  TestIdSdp in 'test\TestIdSdp.pas',
  TestIdSimpleParser in 'test\TestIdSimpleParser.pas',
  TestIdSipCore in 'test\TestIdSipCore.pas',
  TestIdSipDialog in 'test\TestIdSipDialog.pas',
  TestIdSipDialogID in 'test\TestIdSipDialogID.pas',
  TestIdSipHeaders in 'test\TestIdSipHeaders.pas',
  TestIdSipMessage in 'test\TestIdSipMessage.pas',
  TestIdSipParser in 'test\TestIdSipParser.pas',
  TestIdSipTcpClient in 'test\TestIdSipTcpClient.pas',
  TestIdSipTcpServer in 'test\TestIdSipTcpServer.pas',
  TestIdSipTimer in 'test\TestIdSipTimer.pas',
  TestIdSipTransaction in 'test\TestIdSipTransaction.pas',
  TestIdSipTransport in 'test\TestIdSipTransport.pas',
  TestIdSipUdpServer in 'test\TestIdSipUdpServer.pas',
  TestIdSipUri in 'test\TestIdSipUri.pas';

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
  Forms.Application.CreateForm(TrnidSpike, rnidSpike);
//  Forms.Application.CreateForm(TIdSpikeT140, IdSpikeT140);
  Forms.Application.Run;
  {$ELSE}
    {$IFDEF GUI}
    GuiTestRunner.RunRegisteredTests;
    {$ELSE}
    TextTestRunner.RunRegisteredTests;
    {$ENDIF}
  {$ENDIF}
end.
