unit IdSdpParser;

interface

uses
  Classes;

type
  TIdSdpParser = class(TObject)
  private
    fSource: TStream;
  public
    property Source: TStream read fSource write fSource;
  end;


implementation

end.
