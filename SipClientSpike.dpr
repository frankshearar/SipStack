program SipClientSpike;

uses
  Forms,
  SpikeClient in 'test\SpikeClient.pas' {Spike},
  IdSipUdpServer in 'src\IdSipUdpServer.pas',
  IdSimpleParser in 'src\IdSimpleParser.pas',
  IdSipConsts in 'src\IdSipConsts.pas',
  IdSipCore in 'src\IdSipCore.pas',
  IdSipDialog in 'src\IdSipDialog.pas',
  IdSipDialogID in 'src\IdSipDialogID.pas',
  IdSipHeaders in 'src\IdSipHeaders.pas',
  IdSipInterfacedObject in 'src\IdSipInterfacedObject.pas',
  IdSipMessage in 'src\IdSipMessage.pas',
  IdSipRandom in 'src\IdSipRandom.pas',
  IdSipTcpClient in 'src\IdSipTcpClient.pas',
  IdSipTcpServer in 'src\IdSipTcpServer.pas',
  IdSipTimer in 'src\IdSipTimer.pas',
  IdSipTransaction in 'src\IdSipTransaction.pas',
  IdSipTransport in 'src\IdSipTransport.pas',
  IdSdpParser in 'src\IdSdpParser.pas',
  IdSipTlsServer in '\\Ictfilesrv\ICT Group\FrankS\Projects\SIP\src\IdSipTlsServer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSpike, Spike);
  Application.Run;
end.
