{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSipLocator;

interface

uses
  Classes, Contnrs, IdSipDns, IdSipMessage, SysUtils;

// The classes below all encapsulate DNS lookups, or the SIP processing of these
// lookups according to RFC 3263.
//
// As a refresher, here's what a sample zonefile looks like, for the Low Earth
// Orbit Internet Exchange domain:
//
// $TTL    3600
//
// @       IN      SOA     leo-ix.net. root.leo-ix.net.  (
//                                 21000302        ; Serial
//                                 3600    ; Refresh
//                                 900     ; Retry
//                                 3600000 ; Expire
//                                 3600 )  ; Minimum
// @      IN NS   ns1.leo-ix.net.
//
// ;;                   order pref flags service      regexp  replacement
// leo-ix.net. IN NAPTR 0     0    "s"   "SIPS+D2T"   ""      _sips._tcp.leo-ix.net
// leo-ix.net. IN NAPTR 0     0    "s"   "SIP+D2T"    ""      _sip._tcp.leo-ix.net
// leo-ix.net. IN NAPTR 0     0    "s"   "SIP+D2U"    ""      _sip._udp.leo-ix.net
//
// ;;                           priority weight port name
//  _sips._tcp.leo-ix.net.  SRV 1        2      5061 paranoid
//  _sips._tcp.leo-ix.net.  SRV 1        1      5061 paranoid-bak
//  _sip._tcp.leo-ix.net.   SRV 2        0      5060 sip-proxy
//  _sip._udp.leo-ix.net.   SRV 3        0      5060 sip-proxy
// 
//
// ns1             A       127.0.0.1
// paranoid        A       127.0.0.1
// paranoid-bak    AAAA    ::11
// sip-proxy       A       127.0.0.2

type
  TIdSipLocation = class(TObject)
  private
    fTransport: String;
    fIPAddress: String;
    fPort:      Cardinal;
  public
    constructor Create(const Transport: String;
                       const IPAddress:   String;
                             Port: Cardinal); overload;
    constructor Create(Via: TIdSipViaHeader); overload;

    function Copy: TIdSipLocation;

    property Transport: String   read fTransport;
    property IPAddress: String   read fIPAddress;
    property Port:      Cardinal read fPort;
  end;

  TIdSipLocations = class(TIdBaseList)
  private
    function GetLocation(Index: Integer): TIdSipLocation;
  public
    procedure AddLocationsFromNames(const Transport: String;
                                    Port: Cardinal;
                                    Names: TIdDomainNameRecords);
    procedure AddLocationsFromSRVs(SRV: TIdSrvRecords);
    procedure AddLocation(const Transport: String;
                          const Address: String;
                          Port: Cardinal);
    function  First: TIdSipLocation;

    property Items[Index: Integer]: TIdSipLocation read GetLocation; default;
  end;

  // Given a SIP or SIPS URI, I return (using the FindServersFor methods) a set
  // of tuples of the form (transport, IP address, port) that you can use to
  // send a SIP message.
  TIdSipAbstractLocator = class(TObject)
  private
    procedure AddLocationsFromSRVsOrNames(Result: TIdSipLocations;
                                          const Transport: String;
                                          const Target: String;
                                          Port: Cardinal;
                                          Srv: TIdSrvRecords;
                                          Names: TIdDomainNameRecords);
    function  ChooseSupportedTransport(TargetUri: TIdUri;
                                       Naptr: TIdNaptrRecords): String;
    procedure ClearOutUnwantedNaptrRecords(TargetUri: TIdUri;
                                           Recs: TIdNaptrRecords);
    procedure ClearOutUnwantedSrvRecords(Recs: TIdSrvRecords);
    function  FindTransportFromSrv(AddressOfRecord: TIdUri;
                                   SRV: TIdSrvRecords): String;
    function  PassNaptrFiltering(TargetUri: TIdUri;
                                 NAPTR: TIdNaptrRecord): Boolean;
    function  PassSrvFiltering(SRV: TIdSrvRecord): Boolean;
    procedure RemoveAlreadyResolvedSrvs(SupportedTransports: TStrings;
                                        SRV: TIdSrvRecords);
    procedure ResolveSRVForAllSupportedTransports(TargetUri: TIdUri;
                                                  SRV: TIdSrvRecords);
    procedure SupportedTransports(TargetUri: TIdUri; Transports: TStrings);
  protected
    procedure AddUriLocation(AddressOfRecord: TIdSipUri;
                             List: TIdSipLocations);
    function  CreateLocationFromUri(AddressOfRecord: TIdSipUri): TIdSipLocation;
    procedure PerformNameLookup(const DomainName: String;
                                Result: TIdDomainNameRecords); virtual;
    procedure PerformNAPTRLookup(TargetUri: TIdUri;
                                 Result: TIdNaptrRecords); virtual;
    procedure PerformSRVLookup(const ServiceAndDomain: String;
                               Result: TIdSrvRecords); virtual;
    procedure PreFindServersFor(AddressOfRecord: TIdSipUri); overload; virtual;
    procedure PreFindServersFor(Response: TIdSipResponse); overload; virtual;
    procedure PostFindServersFor(AddressOfRecord: TIdSipUri); overload; virtual;
    procedure PostFindServersFor(Response: TIdSipResponse); overload; virtual;
  public
    constructor Create; virtual;

    procedure FindServersFor(AddressOfRecord: TIdSipUri; Result: TIdSipLocations); overload;
    procedure FindServersFor(const AddressOfRecord: String; Result: TIdSipLocations); overload;
    procedure FindServersFor(Response: TIdSipResponse; Result: TIdSipLocations); overload;
    procedure ResolveNameRecords(const DomainName: String;
                                 Result: TIdDomainNameRecords); virtual;
    procedure ResolveNAPTR(TargetUri: TIdUri;
                           Result: TIdNaptrRecords);
    procedure ResolveSRV(const ServiceAndDomain: String;
                         Result: TIdSrvRecords);
    procedure ResolveSRVs(Naptr: TIdNaptrRecords;
                          Result: TIdSrvRecords); overload;
    procedure ResolveSRVs(ServiceAndDomains: TStrings;
                          Result: TIdSrvRecords); overload;
    function  SrvTarget(Target: TIdUri;
                        const Protocol: String): String;
    function  TransportFor(AddressOfRecord: TIdSipUri;
                           NAPTR: TIdNaptrRecords;
                           SRV: TIdSrvRecords;
                           NameRecords: TIdDomainNameRecords): String;
  end;

  ESipLocator = class(Exception);

implementation

uses
  IdSimpleParser, IdSipTransport;

const
  NoRecordFound = 'No record found: %s';

//******************************************************************************
//* TIdSipLocation                                                             *
//******************************************************************************
//* TIdSipLocation Public methods **********************************************

constructor TIdSipLocation.Create(const Transport: String;
                                  const IPAddress:   String;
                                  Port: Cardinal);
begin
  inherited Create;

  Self.fTransport := Transport;
  Self.fIPAddress := IPAddress;
  Self.fPort      := Port;
end;

constructor TIdSipLocation.Create(Via: TIdSipViaHeader);
begin
  inherited Create;

  // Note: You'd better be sure that the Via's sent-by contains an IP address
  // and not a domain name!
  Self.fTransport := Via.Transport;
  Self.fIPAddress := Via.SentBy;
  Self.fPort      := Via.Port;
end;

function TIdSipLocation.Copy: TIdSipLocation;
begin
  Result := TIdSipLocation.Create(Self.Transport, Self.IPAddress, Self.Port);
end;

//******************************************************************************
//* TIdSipLocations                                                            *
//******************************************************************************
//* TIdSipLocations Public methods *********************************************

procedure TIdSipLocations.AddLocationsFromNames(const Transport: String;
                                                Port: Cardinal;
                                                Names: TIdDomainNameRecords);
var
  I: Integer;
begin
  for I := 0 to Names.Count - 1 do
    Self.AddLocation(Transport, Names[I].IPAddress, Port);
end;

procedure TIdSipLocations.AddLocationsFromSRVs(SRV: TIdSrvRecords);
var
  I, J: Integer;
begin
  for I := 0 to Srv.Count - 1 do
    for J := 0 to Srv[I].NameRecords.Count - 1 do
      Self.AddLocation(Srv[I].SipTransport,
                       Srv[I].NameRecords[J].IPAddress,
                       Srv[I].Port);
end;

procedure TIdSipLocations.AddLocation(const Transport: String;
                                      const Address: String;
                                      Port: Cardinal);
var
  NewLocation: TIdSipLocation;
begin
  NewLocation := TIdSipLocation.Create(Transport, Address, Port);
  Self.List.Add(NewLocation);
end;

function TIdSipLocations.First: TIdSipLocation;
begin
  Result := Self.Items[0];
end;

//* TIdSipLocations Private methods ********************************************

function TIdSipLocations.GetLocation(Index: Integer): TIdSipLocation;
begin
  Result := Self.List[Index] as TidSipLocation;
end;

//******************************************************************************
//* TIdSipAbstractLocator                                                      *
//******************************************************************************
//* TIdSipAbstractLocator Public methods ***************************************

constructor TIdSipAbstractLocator.Create;
begin
  inherited Create;
end;

procedure TIdSipAbstractLocator.FindServersFor(AddressOfRecord: TIdSipUri;
                                               Result: TIdSipLocations);
var
  ARecords:  TIdDomainNameRecords;
  Naptr:     TIdNaptrRecords;
  Srv:       TIdSrvRecords;
  Target:    TIdUri;
  Transport: String;
begin
  // See doc/locating_servers.txt for a nice flow chart of this algorithm.begin

  Self.PreFindServersFor(AddressOfRecord);
  try
    Result.Clear;

    Target := TIdUri.Create;
    try
      Target.Scheme := AddressOfRecord.Scheme;

      Naptr := TIdNaptrRecords.Create;
      try
        Srv := TIdSrvRecords.Create;
        try
          ARecords := TIdDomainNameRecords.Create;

          Transport := Self.TransportFor(AddressOfRecord, Naptr, Srv, ARecords);
          try
            if AddressOfRecord.HasMaddr then
              Target.Host := AddressOfRecord.Maddr
            else
              Target.Host := AddressOfRecord.Host;

            if TIdIPAddressParser.IsNumericAddress(Target.Host) then begin
              Result.AddLocation(Transport, Target.Host, AddressOfRecord.Port);
              Exit;
            end;

            if AddressOfRecord.PortIsSpecified then begin
              // AddressOfRecord's Host is a domain name
              Self.ResolveNameRecords(Target.Host, ARecords);

              Result.AddLocationsFromNames(Transport,
                                           AddressOfRecord.Port,
                                           ARecords);

              Exit;
            end;

            if not Naptr.IsEmpty then begin
              Self.ResolveSRVs(Naptr, Srv);

              Self.AddLocationsFromSRVsOrNames(Result,
                                               Transport,
                                               Target.Host,
                                               AddressOfRecord.Port,
                                               Srv, ARecords);

              Exit;
            end;

            if AddressOfRecord.TransportIsSpecified then begin
              Self.ResolveSRV(Self.SrvTarget(Target,
                                             Transport),
                              Srv);

              Self.AddLocationsFromSRVsOrNames(Result,
                                               Transport,
                                               Target.Host,
                                               AddressOfRecord.Port,
                                               Srv, ARecords);

              Exit;
            end;

            Self.ResolveSRVForAllSupportedTransports(AddressOfRecord, Srv);

            Self.AddLocationsFromSRVsOrNames(Result,
                                             Transport,
                                             Target.Host,
                                             AddressOfRecord.Port,
                                             Srv, ARecords);
          finally
            ARecords.Free;
          end;
        finally
          Srv.Free;
        end;
      finally
        Naptr.Free;
      end;
    finally
      Target.Free;
    end;
  finally
    Self.PostFindServersFor(AddressOfRecord);
  end;
end;

procedure TIdSipAbstractLocator.FindServersFor(const AddressOfRecord: String;
                                               Result: TIdSipLocations);
var
  Uri: TIdSipUri;
begin
  Uri := TIdSipUri.Create(AddressOfRecord);
  try
    Self.FindServersFor(Uri, Result);
  finally
    Uri.Free;
  end;
end;

procedure TIdSipAbstractLocator.FindServersFor(Response: TIdSipResponse;
                                               Result: TIdSipLocations);
var
  Names:    TIdDomainNameRecords;
  Port:     Cardinal;
  Services: TIdSrvRecords;
begin
  // cf RFC 3262, section 6:
  // Sending (unicast) responses:
  // 1.  Try send it down the existing connection (if TCP) or to the source
  //     IP/port (if UDP).
  // 2.  Look at the sent-by of the top Via.
  // 2.1 Numeric IP? Attempt the transport/IP/port in the top Via
  // 2.2 Name and port? Query for A/AAAA records; iterate over the
  //     list. Use the transport and port from the top Via.
  // 2.3 Name and no port? Query SRV for that name using "_sips" if TLS or
  //     "_sip" otherwise. Iterate over the list using the transport in the
  //     sent-by and the IP/ports from the SRV query.
  //
  // See doc/locating_servers.txt for a nice flow chart of this algorithm.

  Self.PreFindServersFor(Response);
  try
    Result.Clear;

    if Response.Path.IsEmpty then
      raise ESipLocator.Create('You cannot find locations for a response with no Via headers');

    if Response.LastHop.HasReceived then begin
      if Response.LastHop.HasRport then
        Port := Response.LastHop.RPort
      else
        Port := Response.LastHop.Port;

      Result.AddLocation(Response.LastHop.Transport,
                         Response.LastHop.Received,
                         Port);
    end;

    if TIdIPAddressParser.IsIPv4Address(Response.LastHop.SentBy)
    or TIdIPAddressParser.IsIPv6Reference(Response.LastHop.SentBy) then
      Result.AddLocation(Response.LastHop.Transport,
                         Response.LastHop.SentBy,
                         Response.LastHop.Port)
    else begin
      Services := TIdSrvRecords.Create;
      try
        Names := TIdDomainNameRecords.Create;
        try
          Self.ResolveSRV(Response.LastHop.SrvQuery, Services);

          if Services.IsEmpty then begin
            Self.ResolveNameRecords(Response.LastHop.SentBy, Names);

            Result.AddLocationsFromNames(Response.LastHop.Transport,
                                         Response.LastHop.Port,
                                         Names);
          end
          else
            Result.AddLocationsFromSRVs(Services);
        finally
          Names.Free;
        end;
      finally
        Services.Free;
      end;
    end;
  finally
    Self.PostFindServersFor(Response);
  end;
end;

procedure TIdSipAbstractLocator.ResolveNameRecords(const DomainName: String;
                                                   Result: TIdDomainNameRecords);
begin
  // My subclasses perform a DNS lookup for A/AAAA/A6 records.

  Self.PerformNameLookup(DomainName, Result);
end;

procedure TIdSipAbstractLocator.ResolveNAPTR(TargetUri: TIdUri;
                                             Result: TIdNaptrRecords);
begin
  // My subclasses perform a DNS lookup for NAPTR records. Specifically, those
  // NAPTR RRs for services SIP+D2T, SIP+D2U, SIP+D2S, etc. - all the services
  // for all the transports this stack supports.

  Self.PerformNAPTRLookup(TargetUri, Result);
  Self.ClearOutUnwantedNaptrRecords(TargetUri, Result);
  Result.Sort;
end;

procedure TIdSipAbstractLocator.ResolveSRV(const ServiceAndDomain: String;
                                           Result: TIdSrvRecords);
begin
  // My subclasses perform a DNS lookup for SRV records. ServiceAndDomain
  // typically looks something like "_sips._tcp.example.com".

  // DNS servers that support SRV RRs SHOULD (according to RFC 2782) return all
  // name records (A, AAAA, etc.) for the SRV targets. If they don't, then my
  // subclasses ensure that those SRV records in Result do have all relevant
  // name records attached, where possible.

  Self.PerformSRVLookup(ServiceAndDomain, Result);
  Self.ClearOutUnwantedSrvRecords(Result);
  Result.Sort;
end;

procedure TIdSipAbstractLocator.ResolveSRVs(Naptr: TIdNaptrRecords;
                                            Result: TIdSrvRecords);
var
  I:        Integer;
  Services: TStrings;
begin
  Services := TStringList.Create;
  try
    for I := 0 to Naptr.Count - 1 do
      Services.Add(Naptr[I].Value);

    Self.ResolveSRVs(Services, Result);
  finally
    Services.Free;
  end;
end;

procedure TIdSipAbstractLocator.ResolveSRVs(ServiceAndDomains: TStrings;
                                            Result: TIdSrvRecords);
var
  I: Integer;
begin
  for I := 0 to ServiceAndDomains.Count - 1 do
    Self.PerformSRVLookup(ServiceAndDomains[I], Result);

  Self.ClearOutUnwantedSrvRecords(Result);
  Result.Sort;
end;

function TIdSipAbstractLocator.SrvTarget(Target: TIdUri;
                                         const Protocol: String): String;
begin
  Result := TIdSipTransport.TransportFor(Protocol).SrvQuery(Target.Host);
end;

function TIdSipAbstractLocator.TransportFor(AddressOfRecord: TIdSipUri;
                                            NAPTR: TIdNaptrRecords;
                                            SRV: TIdSrvRecords;
                                            NameRecords: TIdDomainNameRecords): String;
var
  Target: String;
begin
  // See doc/locating_servers.txt for a nice flow chart of this algorithm.

  if AddressOfRecord.HasMaddr then
    Target := AddressOfRecord.Maddr
  else
    Target := AddressOfRecord.Host;

  if AddressOfRecord.TransportIsSpecified then begin
    Result := ParamToTransport(AddressOfRecord.Transport);
    Exit;
  end
  else if TIdIPAddressParser.IsNumericAddress(Target)
       or AddressOfRecord.PortIsSpecified then begin
    Result := ParamToTransport(AddressOfRecord.Transport);
    Exit;
  end;

  Self.ResolveNAPTR(AddressOfRecord, NAPTR);

  if NAPTR.IsEmpty then begin
    Result := Self.FindTransportFromSrv(AddressOfRecord, SRV);

    // No NAPTR records, no SRV records: use whatever default the URI thinks
    // we should.
    if SRV.IsEmpty then
      Result := ParamToTransport(AddressOfRecord.Transport);
  end
  else
    Result := Self.ChooseSupportedTransport(AddressOfRecord, NAPTR);
end;

//* TIdSipAbstractLocator Protected methods ************************************

procedure TIdSipAbstractLocator.AddUriLocation(AddressOfRecord: TIdSipUri;
                                               List: TIdSipLocations);
var
  UriLocation: TIdSipLocation;
begin
  UriLocation := Self.CreateLocationFromUri(AddressOfRecord);
  try
  List.AddLocation(UriLocation.Transport,
                   UriLocation.IPAddress,
                   UriLocation.Port);
  finally
    UriLocation.Free;
  end;
end;

function TIdSipAbstractLocator.CreateLocationFromUri(AddressOfRecord: TIdSipUri): TIdSipLocation;
var
  Address:   String;
  Port:      Cardinal;
  Transport: String;
begin
  if AddressOfRecord.HasParameter(TransportParam) then begin
    Transport := ParamToTransport(AddressOfRecord.Transport);
  end
  else begin
    if (AddressOfRecord.Scheme = SipsScheme) then
      Transport := TlsTransport
    else
      Transport := UdpTransport;
  end;

  Address := AddressOfRecord.Host;
  Port    := AddressOfRecord.Port;

  Result := TIdSipLocation.Create(Transport, Address, Port);
end;

procedure TIdSipAbstractLocator.PerformNameLookup(const DomainName: String;
                                                  Result: TIdDomainNameRecords);
begin
  raise Exception.Create(Self.ClassName + ' doesn''t know how to PerformNameLookup');
end;

procedure TIdSipAbstractLocator.PerformNAPTRLookup(TargetUri: TIdUri;
                                                  Result: TIdNaptrRecords);
begin
  raise Exception.Create(Self.ClassName + ' doesn''t know how to PerformNAPTRLookup');
end;

procedure TIdSipAbstractLocator.PerformSRVLookup(const ServiceAndDomain: String;
                                                 Result: TIdSrvRecords);
begin
  raise Exception.Create(Self.ClassName + ' doesn''t know how to PerformSRVLookup');
end;

procedure TIdSipAbstractLocator.PreFindServersFor(AddressOfRecord: TIdSipUri);
begin
end;

procedure TIdSipAbstractLocator.PreFindServersFor(Response: TIdSipResponse);
begin
end;

procedure TIdSipAbstractLocator.PostFindServersFor(AddressOfRecord: TIdSipUri);
begin
end;

procedure TIdSipAbstractLocator.PostFindServersFor(Response: TIdSipResponse);
begin
end;

//* TIdSipAbstractLocator Protected methods ************************************

procedure TIdSipAbstractLocator.AddLocationsFromSRVsOrNames(Result: TIdSipLocations;
                                                            const Transport: String;
                                                            const Target: String;
                                                            Port: Cardinal;
                                                            Srv: TIdSrvRecords;
                                                            Names: TIdDomainNameRecords);
begin
  if Srv.IsEmpty then begin
    Self.ResolveNameRecords(Target, Names);

    Result.AddLocationsFromNames(Transport, Port, Names);
  end
  else
    Result.AddLocationsFromSRVs(Srv);
end;

function TIdSipAbstractLocator.ChooseSupportedTransport(TargetUri: TIdUri;
                                                        Naptr: TIdNaptrRecords): String;
var
  I:              Integer;
  OurTransports:  TStrings;
  TransportIndex: Integer;
begin
//  Result := Naptr[0].AsSipTransport;

  OurTransports := TStringList.Create;
  try
    Self.SupportedTransports(TargetUri, OurTransports);

    Result := '';
    I      := 0;

    while (I < Naptr.Count) and (Result = '') do begin
      TransportIndex := OurTransports.IndexOf(Naptr[I].AsSipTransport);
      if (TransportIndex <> -1) then
        Result := OurTransports[TransportIndex]
      else
        Inc(I);
    end;
  finally
    OurTransports.Free;
  end;
end;

procedure TIdSipAbstractLocator.ClearOutUnwantedNaptrRecords(TargetUri: TIdUri;
                                                             Recs: TIdNaptrRecords);
var
  I: Integer;
begin
  I := 0;
  while (I < Recs.Count) do begin
    if Self.PassNaptrFiltering(TargetUri, Recs[I]) then
      Inc(I)
    else
      Recs.Delete(I);
  end;
end;

procedure TIdSipAbstractLocator.ClearOutUnwantedSrvRecords(Recs: TIdSrvRecords);
var
  I: Integer;
begin
  I := 0;
  while (I < Recs.Count) do begin
    if Self.PassSrvFiltering(Recs[I]) then
      Inc(I)
    else
      Recs.Delete(I);
  end;
end;

function TIdSipAbstractLocator.FindTransportFromSrv(AddressOfRecord: TIdUri;
                                                    SRV: TIdSrvRecords): String;
var
  I:          Integer;
  Transports: TStrings;
begin
  Transports := TStringList.Create;
  try
    Self.SupportedTransports(AddressOfRecord, Transports);

    // We look up SRV records for each of the transports we support,
    // and stop as soon as we receive the first non-empty result.
    I := 0;
    while (I < Transports.Count) and SRV.IsEmpty do begin
      Self.ResolveSRV(Self.SrvTarget(AddressOfRecord,
                                     Transports[I]),
                      SRV);
      Inc(I);
    end;

    if Srv.IsEmpty then
      Result := ''
    else
      Result := SRV[0].SipTransport;
  finally
    Transports.Free;
  end;
end;

function TIdSipAbstractLocator.PassNaptrFiltering(TargetUri: TIdUri;
                                                  NAPTR: TIdNaptrRecord): Boolean;
var
  Protocol:          String;
  ServiceResolution: String;
begin
  // Return true if the NAPTR service is something like "SIP+D2X" or "SIPS+D2X"
  // and false otherwise.
  // Note that if TargetUri.IsSipsUri we must return false for any non-SIPS
  // ServiceResolutions.

  Protocol          := Naptr.Service;
  ServiceResolution := Fetch(Protocol, NaptrDelimiter);

  if TargetUri.IsSipsUri then
    Result := (ServiceResolution = NaptrSipsService)
  else
    Result := (ServiceResolution = NaptrSipService)
           or (ServiceResolution = NaptrSipsService);

  Result := Result
        and (Length(Protocol) > 2)
        and (Copy(Protocol, 1, 2) = 'D2');
end;

function TIdSipAbstractLocator.PassSrvFiltering(SRV: TIdSrvRecord): Boolean;
begin
  Result := SRV.Target <> SrvNotAvailableTarget;
end;

procedure TIdSipAbstractLocator.RemoveAlreadyResolvedSrvs(SupportedTransports: TStrings;
                                                          SRV: TIdSrvRecords);
var
  I:     Integer;
  Index: Integer;
begin
  // Sometimes we'll have resolved some SRV records as a result of determining
  // a destination transport (in TransportFor) but we want to look up the other
  // SRVs. It would waste time and bandwidth re-resolving the SRV RRs currently
  // in the SRV parameter, so we remove the already-resolved transports from
  // the SupportedTrasnports parameter.

  for I := 0 to SRV.Count - 1 do begin
    Index := SupportedTransports.IndexOf(SRV[I].SipTransport);
    if (Index <> -1) then
      SupportedTransports.Delete(Index);
  end;
end;

procedure TIdSipAbstractLocator.ResolveSRVForAllSupportedTransports(TargetUri: TIdUri;
                                                                    SRV: TIdSrvRecords);
var
  I:             Integer;
  OurTransports: TStrings;
begin
  OurTransports := TStringList.Create;
  try
    Self.SupportedTransports(TargetUri, OurTransports);

    Self.RemoveAlreadyResolvedSrvs(OurTransports, SRV);

    for I := 0 to OurTransports.Count - 1 do
      OurTransports[I] := Self.SrvTarget(TargetUri,
                                         OurTransports[I]);

    Self.ResolveSRVs(OurTransports, SRV);
  finally
    OurTransports.Free;
  end;
end;

procedure TIdSipAbstractLocator.SupportedTransports(TargetUri: TIdUri; Transports: TStrings);
begin
  Transports.Clear;

  TIdSipTransport.SecureTransports(Transports);

  if not TargetUri.IsSipsUri then begin
    TIdSipTransport.InsecureTransports(Transports);
  end;
end;

end.
