program SIP;

uses
  Forms,
  GuiTestRunner,
  IdSipParser in 'src\IdSipParser.pas',
  IdSipTcpServer in 'src\IdSipTcpServer.pas',
  IdSipUdpServer in 'src\IdSipUdpServer.pas',
  Spike in 'test\Spike.pas' {rnidSpike},
  TestFrameworkEx in '..\IctWebsiteCgi\test\TestFrameworkEx.pas',
  XmlCgi in '..\IctWebsiteCgi\src\XmlCgi.pas',
  TestIdSipParser in 'test\TestIdSipParser.pas',
  TestIdSipTcpServer in 'test\TestIdSipTcpServer.pas',
  TestIdSipUdpServer in 'test\TestIdSipUdpServer.pas' {,
  TestIdURI in 'test\TestIdURI.pas'},
  IdSIPClient in 'src\IdSIPClient.pas',
  TestIdSipClient in 'test\TestIdSipClient.pas',
  TestIdURI in '\\Ictfilesrv\ICT Group\FrankS\Projects\SIP\test\TestIdURI.pas';

//{$DEFINE SPIKE}

{$R *.res}

begin
  {$IFDEF SPIKE}
  Forms.Application.Initialize;
  Forms.Application.CreateForm(TrnidSpike, rnidSpike);
  Forms.Application.Run;
  {$ELSE}
  GuiTestRunner.RunRegisteredTests;
  {$ENDIF}
end.
