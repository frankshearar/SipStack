{
  (c) 2005 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit MultipleCoreSpike;

interface

uses
  Classes, Controls, ExtCtrls, Forms, IdSipMessage, StdCtrls;

type
  TMultiCore = class(TForm)
    VertSplitter: TSplitter;
    Panel1: TPanel;
    RecreateUAs: TButton;
    RightPanel: TPanel;
    LeftPanel: TPanel;
    procedure RecreateUAsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    UA1: TForm;
    UA2: TForm;
    UAConfig1: TStrings;
    UAConfig2: TStrings;

    function  AddCore(Config: TStrings; Parent: TPanel): TForm;
    procedure ResizePanels;
    function  UAConfig(From: TIdSipAddressHeader): String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  MultiCore: TMultiCore;

implementation

{$R *.dfm}

uses
 IdSipSubscribeModule, IdSipTcpTransport, IdSipTransport, IdSipUdpTransport,
 SingleCoreSpike, SysUtils;

//******************************************************************************
//* TMultiCore                                                                 *
//******************************************************************************
//* TMultiCore Public methods **************************************************

constructor TMultiCore.Create(AOwner: TComponent);
var
  From: TIdSipFromHeader;
begin
  inherited Create(AOwner);

  TIdSipEventPackageRegistry.RegisterEvent(TIdSipReferPackage);

  TIdSipTransportRegistry.RegisterTransport(TcpTransport, TIdSipTcpTransport);
  TIdSipTransportRegistry.RegisterTransport(UdpTransport, TIdSipUdpTransport);

  Self.UAConfig1 := TStringList.Create;
  Self.UAConfig2 := TStringList.Create;

  From := TIdSipFromHeader.Create;
  try
    From.Value := '"Count Zero" <sip:case@127.0.0.1:55060>';
    Self.UAConfig1.Text := Self.UAConfig(From);

    From.Value := '"Wintermute" <sip:wintermute@127.0.0.1:65060>';
    Self.UAConfig2.Text := Self.UAConfig(From);
  finally
    From.Free;
  end;

  Self.ResizePanels;
  Self.RecreateUAs.Click;
end;

destructor TMultiCore.Destroy;
begin
  TIdSipTransportRegistry.UnregisterTransport(TcpTransport);
  TIdSipTransportRegistry.UnregisterTransport(UdpTransport);

  TIdSipEventPackageRegistry.UnregisterEvent(TIdSipReferPackage);

  Self.UAConfig2.Free;
  Self.UAConfig1.Free;

  inherited Destroy;
end;

//* TMultiCore Private methods *************************************************

function TMultiCore.AddCore(Config: TStrings; Parent: TPanel): TForm;
begin
  Result := TSingleCore.CreateUA(Self, Config);
//  Result.Parent  := Parent;
//  Result.Align   := alClient;
  Result.Visible := true;
end;

procedure TMultiCore.ResizePanels;
begin
  Self.LeftPanel.Width := (Self.ClientWidth - Self.VertSplitter.Width) div 2;
end;

function TMultiCore.UAConfig(From: TIdSipAddressHeader): String;
begin
  Result := 'NameServer: MOCK'#$D#$A
          + 'Listen: UDP ' + From.Address.Host + ':' + IntToStr(From.Address.Port) + #$D#$A
          + 'Listen: TCP ' + From.Address.Host + ':' + IntToStr(From.Address.Port) + #$D#$A
          + 'From: ' + From.Value + #$D#$A
          + 'Contact: ' + From.Value + #$D#$A
          + 'SupportEvent: ' + TIdSipReferPackage.EventPackage;
end;

//* TMultiCore Published methods ***********************************************

procedure TMultiCore.RecreateUAsClick(Sender: TObject);
var
  Config: TStrings;
begin
  Config := TStringList.Create;
  try
    if Assigned(Self.UA1) then
      FreeAndNil(Self.UA1);

    Self.UA1 := Self.AddCore(Self.UAConfig1, Self.LeftPanel);

    if Assigned(Self.UA2) then
      FreeAndNil(Self.UA2);

    Self.UA2 := Self.AddCore(Self.UAConfig2, Self.RightPanel);
  finally
    Config.Free;
  end;
end;

procedure TMultiCore.FormResize(Sender: TObject);
begin
  Self.ResizePanels;
end;

end.
