{
  (c) 2007 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdMockRoutingTable;

interface

uses
  Contnrs, IdRoutingTable;

type
  TIdMockRoutingTable = class(TIdIPv4RoutingTable)
  public
    procedure AddRoute(Destination, Mask, Gateway: String; Metric: Cardinal; InterfaceIndex: String); overload; override;
    procedure AddRoute(Route: TIdRouteEntry); overload; override;
  end;

implementation

//******************************************************************************
//* TIdMockRoutingTable                                                        *
//******************************************************************************
//* TIdMockRoutingTable Public methods *****************************************

procedure TIdMockRoutingTable.AddRoute(Destination, Mask, Gateway: String; Metric: Cardinal; InterfaceIndex: String);
begin
  inherited;
end;

procedure TIdMockRoutingTable.AddRoute(Route: TIdRouteEntry);
begin
  inherited;
end;

end.
