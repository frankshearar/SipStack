unit BasicClient;

interface

uses
  Forms, IdRTPDiagnostics, StdCtrls, Controls, ExtCtrls, Classes;

type
  TfmBasicClient = class(TForm)
    Holder: TPanel;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Event: TEdit;
    Trigger: TButton;
    procedure TriggerClick(Sender: TObject);
  private
    Hist:      TIdRTPPayloadHistogram;
    HistPanel: TIdHistogramPanel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  fmBasicClient: TfmBasicClient;

implementation

{$R *.dfm}

constructor TfmBasicClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Hist      := TIdRTPPayloadHistogram.Create;
  Self.HistPanel := TIdHistogramPanel.Create(nil);

  Self.HistPanel.Align      := alClient;
  Self.HistPanel.BevelInner := bvNone;
  Self.HistPanel.BevelOuter := bvNone;
  Self.HistPanel.Parent     := Self.Holder;

  Self.Hist.AddObserver(Self.HistPanel);
end;

destructor TfmBasicClient.Destroy;
begin
  Self.HistPanel.Free;
  Self.Hist.Free;

  inherited Destroy;
end;

procedure TfmBasicClient.TriggerClick(Sender: TObject);
begin
  Self.Hist.RecordEvent(Self.Event.Text);
end;

end.
