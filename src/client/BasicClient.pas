unit BasicClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TfmBasicClient = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Memo2: TMemo;
    Panel2: TPanel;
    Splitter2: TSplitter;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmBasicClient: TfmBasicClient;

implementation

{$R *.dfm}

end.
