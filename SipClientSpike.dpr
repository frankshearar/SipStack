program SipClientSpike;

uses
  Forms,
  IdRTP in 'src\IdRTP.pas',
  IdRTPClient in 'src\IdRTPClient.pas',
  IdRTPServer in 'src\IdRTPServer.pas',
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
  IdSipTlsServer in 'src\IdSipTlsServer.pas',
  IdSipUdpClient in 'src\IdSipUdpClient.pas',
  IdSdp in 'src\IdSdp.pas',
  SpikeClient in 'test\SpikeClient.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
