unit SpikeRegister;

interface

uses
  Classes, Controls, Forms, IdSipCore, IdSipMessage, IdSipTransaction,
  IdSipTransport, StdCtrls;

type
  TrnidSpikeRegister = class(TForm,
                             IIdSipRegistrationListener,
                             IIdSipTransportListener,
                             IIdSipTransportSendingListener)
    Log: TMemo;
  private
    Dispatcher: TIdSipTransactionDispatcher;
    Transport:  TIdSipTransport;
    UA:         TIdSipUserAgentCore;

    procedure LogMessage(Msg: TIdSipMessage);
    procedure OnAuthenticationChallenge(RegisterAgent: TIdSipRegistration;
                                        Response: TIdSipResponse);
    procedure OnFailure(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Transport: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Transport: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Transport: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Transport: TIdSipTransport);
    procedure OnSuccess(RegisterAgent: TIdSipRegistration;
                        CurrentBindings: TIdSipContacts);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  rnidSpikeRegister: TrnidSpikeRegister;

implementation

{$R *.dfm}

uses
  IdStack, IdSipConsts, IdSocketHandle, SysUtils;

const
  LocalHostName = '127.0.0.1';
  RunningPort   = IdPORT_SIP;

//******************************************************************************
//* TrnidSpikeRegister                                                         *
//******************************************************************************
//* TrnidSpikeRegister Public methods ******************************************

constructor TrnidSpikeRegister.Create(AOwner: TComponent);
var
  Binding: TIdSocketHandle;
  Contact: TIdSipContactHeader;
  From:    TIdSipFromHeader;
begin
  inherited Create(AOwner);

  Self.Transport := TIdSipTCPTransport.Create(IdPORT_SIP);
  if (GStack.LocalAddress <> LocalHostName) then begin
    Binding                 := Self.Transport.Bindings.Add;
    Binding.IP              := GStack.LocalAddress;
    Binding.Port            := RunningPort;
    Self.Transport.HostName := Binding.IP;
  end
  else
    Self.Transport.HostName := LocalHostName;

  Binding      := Self.Transport.Bindings.Add;
  Binding.IP   := LocalHostName;
  Binding.Port := RunningPort;
  Self.Transport.AddTransportListener(Self);
  Self.Transport.AddTransportSendingListener(Self);

  Self.Dispatcher := TIdSipTransactionDispatcher.Create;
  Self.Dispatcher.AddTransport(Self.Transport);
  Self.UA := TIdSipUserAgentCore.Create;
  Self.UA.Dispatcher := Self.Dispatcher;

  Contact := TIdSipContactHeader.Create;
  try
    Contact.Value := 'sip:franks@'
                   + Self.Transport.HostName + ':'
                   + IntToStr(Self.Transport.Bindings[0].Port);
    Self.UA.Contact := Contact;
  finally
    Contact.Free;
  end;

  From := TIdSipFromHeader.Create;
  try
    From.Value := 'sip:franks@' + Self.Transport.HostName;
    Self.UA.From := From;
  finally
    From.Free;
  end;
end;

destructor TrnidSpikeRegister.Destroy;
begin
  Self.UA.Free;
  Self.Dispatcher.Free;
  Self.Transport.Free;

  inherited Destroy;
end;

//* TrnidSpikeRegister Private methods *****************************************

procedure TrnidSpikeRegister.LogMessage(Msg: TIdSipMessage);
begin
  Self.Log.Lines.Add(Msg.AsString);
  Self.Log.Lines.Add('----');
end;

procedure TrnidSpikeRegister.OnAuthenticationChallenge(RegisterAgent: TIdSipRegistration;
                                               Response: TIdSipResponse);
begin
end;

procedure TrnidSpikeRegister.OnFailure(RegisterAgent: TIdSipRegistration;
                               CurrentBindings: TIdSipContacts;
                               const Reason: String);
begin
end;

procedure TrnidSpikeRegister.OnReceiveRequest(Request: TIdSipRequest;
                                      Transport: TIdSipTransport);
begin
  Self.LogMessage(Request);
end;

procedure TrnidSpikeRegister.OnReceiveResponse(Response: TIdSipResponse;
                                       Transport: TIdSipTransport);
begin
  Self.LogMessage(Response);
end;

procedure TrnidSpikeRegister.OnRejectedMessage(const Msg: String;
                                       const Reason: String);
begin
  Self.Log.Lines.Add('----REJECTED MESSAGE: ' + Reason + '----');
  Self.Log.Lines.Add(Msg);
  Self.Log.Lines.Add('----');
end;

procedure TrnidSpikeRegister.OnSendRequest(Request: TIdSipRequest;
                                   Transport: TIdSipTransport);
begin
  Self.LogMessage(Request);
end;

procedure TrnidSpikeRegister.OnSendResponse(Response: TIdSipResponse;
                                    Transport: TIdSipTransport);
begin
  Self.LogMessage(Response);
end;

procedure TrnidSpikeRegister.OnSuccess(RegisterAgent: TIdSipRegistration;
                               CurrentBindings: TIdSipContacts);
begin
end;

end.
