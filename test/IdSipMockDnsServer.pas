{
  (c) 2006 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipMockDnsServer;

interface

uses
  Classes, IdSocketHandle, IdUdpServer;

type
  TIdSipMockDnsServer = class(TObject)
  private
    AnswerIndex: Integer;
    Answers:     TStrings;
    NameServer:  TIdUdpServer;

    procedure ProvideAnswer(Sender: TObject;
                            AData: TStream;
                            ABinding: TIdSocketHandle);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddAnswer(DnsAnswer: String);
    procedure ClearAnswers;
    procedure Start;
    procedure Stop;
  end;

implementation

//******************************************************************************
//* TIdSipMockDnsServer                                                        *
//******************************************************************************
//* TIdSipMockDnsServer Public methods *****************************************

constructor TIdSipMockDnsServer.Create;
var
  LocalHost: TIdSocketHandle;
begin
  inherited Create;

  Self.Answers     := TStringList.Create;
  Self.AnswerIndex := 0;

  Self.NameServer := TIdUDPServer.Create(nil);
  LocalHost := Self.NameServer.Bindings.Add;
  LocalHost.IP   := '127.0.0.1';
  LocalHost.Port := 53;

  Self.NameServer.OnUDPRead     := Self.ProvideAnswer;
  Self.NameServer.ThreadedEvent := true;
  Self.Start;
end;

destructor TIdSipMockDnsServer.Destroy;
begin
  Self.Stop;
  Self.NameServer.Free;

  Self.Answers.Free;

  inherited Destroy;
end;

procedure TIdSipMockDnsServer.AddAnswer(DnsAnswer: String);
begin
  Self.Answers.Add(DnsAnswer);
end;

procedure TIdSipMockDnsServer.ClearAnswers;
begin
  Self.Answers.Clear;
end;

procedure TIdSipMockDnsServer.Start;
begin
  Self.NameServer.Active := true;
end;

procedure TIdSipMockDnsServer.Stop;
begin
  Self.NameServer.Active := false;
end;

//* TIdSipMockDnsServer Private methods ****************************************

procedure TIdSipMockDnsServer.ProvideAnswer(Sender: TObject;
                                            AData: TStream;
                                            ABinding: TIdSocketHandle);
var
  Answer:  String;
  ReplyID: String;
  S:       TStringStream;
begin
  S := TStringStream.Create('');
  try
    S.CopyFrom(AData, 0);

    ReplyID := Copy(S.DataString, 1, 2);
  finally
    S.Free;
  end;

  Answer := ReplyID + Self.Answers[Self.AnswerIndex];

  Self.NameServer.Send(ABinding.PeerIP,
                       ABinding.PeerPort,
                       Answer);
  Inc(Self.AnswerIndex);
end;

end.
