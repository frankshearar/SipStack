program SIP;

uses
  Forms,
  GuiTestRunner,
  TextTestRunner,
  IdSdpParser in 'src\IdSdpParser.pas',
  IdSimpleParser in 'src\IdSimpleParser.pas',
  IdSIPClient in 'src\IdSIPClient.pas',
  IdSipParser in 'src\IdSipParser.pas',
  IdSipTcpServer in 'src\IdSipTcpServer.pas',
  IdSipUdpServer in 'src\IdSipUdpServer.pas',
  Spike in 'test\Spike.pas' {rnidSpike},
  TestFrameworkEx in '..\IctWebsiteCgi\test\TestFrameworkEx.pas',
  TortureTests in 'test\TortureTests.pas',
  XmlCgi in '..\IctWebsiteCgi\src\XmlCgi.pas',
  TestIdSdpParser in 'test\TestIdSdpParser.pas',
  TestIdSipParser in 'test\TestIdSipParser.pas',
  TestIdSimpleParser in 'test\TestIdSimpleParser.pas',
  TestIdSipTcpServer in 'test\TestIdSipTcpServer.pas',
  TestIdSipUdpServer in 'test\TestIdSipUdpServer.pas',
//  TestIdURI in 'test\TestIdURI.pas',
  TestIdSipClient in 'test\TestIdSipClient.pas';

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
  Forms.Application.Run;
  {$ELSE}
    {$IFDEF GUI}
    GuiTestRunner.RunRegisteredTests;
    {$ELSE}
    TextTestRunner.RunRegisteredTests;
    {$ENDIF}
  {$ENDIF}
end.
