{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit SpikeRegister;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdSipCore, IdSipMessage,
  IdSipTransaction, IdSipTransport, StdCtrls, SyncObjs, SysUtils;

type
  TrnidSpikeRegister = class(TForm,
                             IIdSipRegistrationListener,
                             IIdSipTransportListener,
                             IIdSipTransportSendingListener)
    Log: TMemo;
    Panel1: TPanel;
    Register: TButton;
    Query: TButton;
    Unregister: TButton;
    Registrar: TEdit;
    Splitter1: TSplitter;
    Contacts: TMemo;
    procedure QueryClick(Sender: TObject);
    procedure RegisterClick(Sender: TObject);
    procedure UnregisterClick(Sender: TObject);
  private
    Dispatcher: TIdSipTransactionDispatcher;
    Lock:       TCriticalSection;
    Transport:  TIdSipTransport;
    UA:         TIdSipUserAgentCore;

    procedure LogMessage(Msg: TIdSipMessage);
    procedure OnAuthenticationChallenge(Action: TIdSipAction;
                                        Response: TIdSipResponse;
                                        var Password: String);
    procedure OnException(E: Exception;
                          const Reason: String);
    procedure OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts;
                        const Reason: String);
    procedure OnReceiveRequest(Request: TIdSipRequest;
                               Receiver: TIdSipTransport);
    procedure OnReceiveResponse(Response: TIdSipResponse;
                                Receiver: TIdSipTransport);
    procedure OnRejectedMessage(const Msg: String;
                                const Reason: String);
    procedure OnSendRequest(Request: TIdSipRequest;
                            Sender: TIdSipTransport);
    procedure OnSendResponse(Response: TIdSipResponse;
                             Sender: TIdSipTransport);
    procedure OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                        CurrentBindings: TIdSipContacts);
    procedure RefreshContacts(Bindings: TIdSipContacts);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  rnidSpikeRegister: TrnidSpikeRegister;

implementation

{$R *.dfm}

uses
  IdStack, IdSipConsts, IdSocketHandle;

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

  Self.Lock := TCriticalSection.Create;

  Self.Transport := TIdSipUDPTransport.Create(IdPORT_SIP);
  if (GStack.LocalAddress <> LocalHostName) then
    Self.Transport.HostName := GStack.LocalAddress
  else
    Self.Transport.HostName := LocalHostName;
  Self.Transport.Address := Self.Transport.HostName;

  Self.Transport.AddTransportListener(Self);
  Self.Transport.AddTransportSendingListener(Self);

  Self.Dispatcher := TIdSipTransactionDispatcher.Create;
  Self.Dispatcher.AddTransport(Self.Transport);
  Self.UA := TIdSipUserAgentCore.Create;
  Self.UA.Dispatcher := Self.Dispatcher;
  Self.UA.HostName := 'wsfrank';
  Self.UA.UserAgentName := 'Frank''s Registration Spike';

  Contact := TIdSipContactHeader.Create;
  try
    Contact.Value := 'sip:franks@'
                   + Self.Transport.HostName + ':'
                   + IntToStr(Self.Transport.Port);
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
  Self.Lock.Free;

  inherited Destroy;
end;

//* TrnidSpikeRegister Private methods *****************************************

procedure TrnidSpikeRegister.LogMessage(Msg: TIdSipMessage);
begin
  Self.Log.Lines.Add(Msg.AsString);
  Self.Log.Lines.Add('----');
end;

procedure TrnidSpikeRegister.OnAuthenticationChallenge(Action: TIdSipAction;
                                                       Response: TIdSipResponse;
                                                       var Password: String);
begin
  Self.Log.Lines.Add('---- Authentication challenge for registration ----');
end;

procedure TrnidSpikeRegister.OnException(E: Exception;
                                         const Reason: String);
begin
  Self.Log.Lines.Add('---- Exception ' + E.ClassName
                   + ' raised: ' + E.Message
                   + ' because: ''' + Reason + ''' ----');
end;

procedure TrnidSpikeRegister.OnFailure(RegisterAgent: TIdSipOutboundRegistration;
                                       CurrentBindings: TIdSipContacts;
                                       const Reason: String);
begin
  Self.Log.Lines.Add('---- Registration action failed: ' + Reason + ' ----');
  Self.RefreshContacts(CurrentBindings);
  RegisterAgent.RemoveListener(Self);
end;

procedure TrnidSpikeRegister.OnReceiveRequest(Request: TIdSipRequest;
                                              Receiver: TIdSipTransport);
begin
  Self.LogMessage(Request);
end;

procedure TrnidSpikeRegister.OnReceiveResponse(Response: TIdSipResponse;
                                               Receiver: TIdSipTransport);
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
                                           Sender: TIdSipTransport);
begin
  Self.LogMessage(Request);
end;

procedure TrnidSpikeRegister.OnSendResponse(Response: TIdSipResponse;
                                            Sender: TIdSipTransport);
begin
  Self.LogMessage(Response);
end;

procedure TrnidSpikeRegister.OnSuccess(RegisterAgent: TIdSipOutboundRegistration;
                                       CurrentBindings: TIdSipContacts);
begin
  Self.Log.Lines.Add('---- Registration action succeeded ----');
  RegisterAgent.RemoveListener(Self);

  Self.RefreshContacts(CurrentBindings);
end;

procedure TrnidSpikeRegister.RefreshContacts(Bindings: TIdSipContacts);
begin
  Self.Lock.Acquire;
  try
    Self.Contacts.Lines.Clear;
    Bindings.First;

    while Bindings.HasNext do begin
      Self.Contacts.Lines.Add(Bindings.CurrentContact.AsAddressOfRecord);
      Bindings.Next;
    end;
  finally
    Self.Lock.Release;
  end;
end;

//* TrnidSpikeRegister Published methods ***************************************

procedure TrnidSpikeRegister.QueryClick(Sender: TObject);
var
  RegistrarUri: TIdSipUri;
begin
  RegistrarUri := TIdSipUri.Create(Self.Registrar.Text);
  try
    Self.UA.CurrentRegistrationWith(RegistrarUri).AddListener(Self);
  finally
    RegistrarUri.Free;
  end;
end;

procedure TrnidSpikeRegister.RegisterClick(Sender: TObject);
var
  RegistrarUri: TIdSipUri;
begin
  RegistrarUri := TIdSipUri.Create(Self.Registrar.Text);
  try
    Self.UA.RegisterWith(RegistrarUri).AddListener(Self);
  finally
    RegistrarUri.Free;
  end;
end;

procedure TrnidSpikeRegister.UnregisterClick(Sender: TObject);
var
  RegistrarUri: TIdSipUri;
begin
  RegistrarUri := TIdSipUri.Create(Self.Registrar.Text);
  try
    Self.UA.UnregisterFrom(RegistrarUri).AddListener(Self);
  finally
    RegistrarUri.Free;
  end;
end;

end.
