{
  (c) 2009 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit SipConfigUtils;

// This unit contains helper functions for making configuration files. Unlike
// ConfigUtils.pas, these functions are SipStack-specific.

interface

function Directive(Name, Value: String): String;
function ListenOn(Transport, Address: String): String; overload;
function ListenOn(Transport, Address: String; Port: Cardinal): String; overload;
function NameServer(Address: String; Port: Cardinal): String;

implementation

uses
  IdSipUserAgent, SysUtils;

function Directive(Name, Value: String): String;
begin
  Result := Format('%s: %s', [Name, Value]);
end;

function ListenOn(Transport, Address: String): String;
begin
  Result := Directive(ListenDirective, Format('%s %s', [Transport, Address]));
end;

function ListenOn(Transport, Address: String; Port: Cardinal): String;
begin
  Result := Directive(ListenDirective, Format('%s %s:%d', [Transport, Address, Port]));
end;

function NameServer(Address: String; Port: Cardinal): String;
begin
  Result := Directive(NameServerDirective, Format('%s:%d', [Address, Port]));
end;

end.
