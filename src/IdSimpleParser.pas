{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit IdSimpleParser;

interface

uses
  Classes, SysUtils;

type
  TCharSet = set of Char;

  // for IdCore or whatever. Indy10 defines these but Indy9 doesn't
  TIdIPVersion = (Id_IPv4, Id_IPv6, Id_IPUnknown);

  // Indy10 defines this in IdStackBSDBase
  TIdIPv6AddressRec = array[0..7] of Word;

  TIdIPAddressParser = class(TObject)
    class function  AddressToMask(Netmask: String; IPVersion: TIdIPVersion): Cardinal;
    class function  ExpandIPv6Address(const IPAddress: String): String;
    class function  IncIPAddress(const IPAddress: String;
                                 N: Cardinal = 1): String;
    class function  IncIPv4Address(const IPAddress: String;
                                   N: Cardinal = 1): String;
    class procedure IncIPv6Address(var Address: TIdIPv6AddressRec;
                                   N: Cardinal = 1);
    class function  InetAddr(const IPv4Address: String): Cardinal;
    class function  IPv4AddressToStr(Address: Cardinal): String;
    class function  IPv6AddressToStr(Address: TIdIPv6AddressRec; ShowEncapsulatedIPv4: Boolean = false): String;
    class function  IPVersion(Address: String): TIdIPVersion;
    class function  IsIPAddress(IpVersion: TIdIPVersion;
                                const Token: String): Boolean; overload;
    class function  IsIPAddress(const Token: String): Boolean; overload;
    class function  IsIPv4Address(const Token: String): Boolean;
    class function  IsIPv6Address(const Token: String): Boolean;
    class function  IsIPv6Reference(const Token: String): Boolean;
    class function  IsNumericAddress(const Token: String): Boolean;
    class function  MaskToAddress(NumberSignificantBits: Cardinal; IPVersion: TIdIPVersion): String;
    class function  NetworkForIPv4(Address: String; Mask: String): String;
    class function  NetworkForIPv6(Address: String; Mask: String): String;
    class function  NetworkFor(Address: String; Mask: String): String; overload;
    class function  NetworkFor(Address: String; NumSignificantBits: Cardinal): String; overload;
    class procedure ParseIPv6Address(const IPv6Address: String;
                                     var Address: TIdIPv6AddressRec);
  end;

  // I provide a base class for line-oriented grammars.
  TIdSimpleParser = class(TObject)
  private
    fCurrentLine: Cardinal;
    fSource:      TStream;
    procedure IncCurrentLine;
  protected
    procedure Assert(Condition: Boolean; Msg: String);
    procedure ResetCurrentLine;
  public
    class function IsAlphaNumeric(const Token: String): Boolean;
    class function IsByte(const Token: String): Boolean;
    class function IsDigit(C: Char): Boolean;
    class function IsFQDN(const Token: String): Boolean;
    class function IsHexNumber(const Number: String): Boolean;
    class function IsLetter(C: Char): Boolean;
    class function IsNumber(const Number: String): Boolean;

    constructor Create; virtual;

    function  CurrentLine: Cardinal;
    function  Eof: Boolean;
    function  Peek: Char;
    function  PeekLine: String;
    function  ReadOctet: Char;
    function  ReadOctets(Count: Cardinal): String;
    function  ReadLn: String;
    function  ReadFirstNonBlankLine: String;
    procedure SkipBlankLines;

    property  Source: TStream read fSource write fSource;
  end;

  EBadParameter = class(Exception);

const
  BadSyntax        = 'Bad syntax';
  DamagedLineEnd   = 'Damaged line end at line %d, expected %s but was %s';
  EmptyInputStream = 'Empty input stream';
  HexToIntError    = '''%s'' is not a valid hex value';
  IPAddrError      = '''%s'' is not a valid IP address';
  IPv4AddrError    = '''%s'' is not a valid IPv4 address';
  IPv6AddrError    = '''%s'' is not a valid IPv6 address';
  MalformedToken   = 'Malformed %s: ''%s''';

const
  Alphabet        = ['a'..'z', 'A'..'Z'];
  Digits          = ['0'..'9'];
  HexDigits       = Digits + ['a'..'f', 'A'..'F'];
  MarkChars       = ['-', '_', '.', '!', '~', '*', '''', '(', ')'];
  UnreservedChars = Alphabet + Digits + MarkChars;

const
  FetchDefaultDelete = true;
  FetchDefaultDelimiter = ' '; // Do not localise
  BitsInIPv6Address = SizeOf(TIdIPv6AddressRec)*SizeOf(Word);

function BitsInCardinal: Cardinal;
function ContainsOnly(const Token: String; LegalChars: TCharSet): Boolean;
function EncodeNonLineUnprintableChars(S: String): String;
function EncodeQuotedStr(const S: String): String;
function EndsWith(const S, Suffix: String): Boolean;
function Fetch(var Source: String;
               const Delimiter: String = FetchDefaultDelimiter;
               Delete: Boolean = FetchDefaultDelete): String; overload;
function Fetch(var Source: String;
               Delimiters: TCharSet;
               Delete: Boolean = FetchDefaultDelete): String; overload;
function FirstOf(Needles: String; Haystack: String): Integer; overload;
function FirstOf(Needles: TCharSet; Haystack: String): Integer; overload;
function HexDigitToInt(Digit: Char): Cardinal;
function HexToInt(const HexValue: String): Cardinal;
function LastPos(const Needle, Haystack: String; Start: Integer = -1): Integer;
function Localhost(IPType: TIdIPVersion): String;
function StartsWith(const S, Prefix: String): Boolean;
function StreamToStr(Data: TStream): String;
function StripLeadingZeroes(const S: String): String;
function WithoutFirstAndLastChars(const S: String): String;

implementation

uses
  Math, StrUtils;

//******************************************************************************
//* Unit Private Functions and Procedures                                      *
//******************************************************************************

function FetchCopy(var Source: String;
                   StartPos: Integer;
                   DelimiterLength: Integer;
                   Delete: Boolean): String;
begin
  // FetchCopy contains the guts of the two Fetch functions.

  if (StartPos = 0) then begin
    Result := Source;

    if Delete then
      Source := '';
  end
  else begin
    Result := Copy(Source, 1, StartPos - 1);

    if Delete then
      Source := Copy(Source, StartPos + DelimiterLength, Length(Source));
  end;
end;

//******************************************************************************
//* Unit Public Functions and Procedures                                       *
//******************************************************************************

function BitsInCardinal: Cardinal;
begin
  Result := SizeOf(Cardinal)*8;
end;

function ContainsOnly(const Token: String; LegalChars: TCharSet): Boolean;
var
  I: Integer;
begin
  Result := true;
  for I := 1 to Length(Token) do begin
    Result := Result and (Token[I] in LegalChars);
    if not Result then Break;
  end;
end;

function EncodeNonLineUnprintableChars(S: String): String;
var
  I: Integer;
begin
  // Given a multi-line string, return a string that preserves end-of-line
  // characters while encoding all other control characters as '#nn'.

  Result := '';

  for I := 1 to Length(S) do
    if (S[I] in [#10, #13, #32..#126]) then
      Result := Result + S[I]
    else
      Result := Result + '#' + IntToHex(Ord(S[I]), 2);
end;

function EncodeQuotedStr(const S: String): String;
begin
  Result := StringReplace(S,      '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll, rfIgnoreCase]);
end;

function EndsWith(const S, Suffix: String): Boolean;
begin
  // Returns true iff a string ends with a specified suffix.

  Result := Copy(S, Length(S) - Length(Suffix) + 1, Length(Suffix)) = Suffix;
end;

function Fetch(var Source: String;
               const Delimiter: String = FetchDefaultDelimiter;
               Delete: Boolean = FetchDefaultDelete): String;
begin
  // This version of Fetch returns the contents of Source up to the Delimiter.
  // If Delete = true then the returned characters are deleted from the Source.

  Result := FetchCopy(Source,
                      Pos(Delimiter, Source),
                      Length(Delimiter),
                      Delete);
end;

function Fetch(var Source: String;
               Delimiters: TCharSet;
               Delete: Boolean = FetchDefaultDelete): String;
begin
  // This version of Fetch returns the contents of Source up to the first
  // occurence of the Delimiters. If Delete = true then the returned characters
  // are deleted from the Source.

  Result := FetchCopy(Source,
                      FirstOf(Delimiters, Source),
                      1,
                      Delete);
end;

function FirstOf(Needles: String; Haystack: String): Integer;
var
  CharSet: TCharSet;
  I:       Integer;
begin
  CharSet := [];

  for I := 1 to Length(Needles) do
    CharSet := CharSet + [Needles[I]];

  Result := FirstOf(CharSet, Haystack);
end;

function FirstOf(Needles: TCharSet; Haystack: String): Integer;
begin
  // FirstOf is a generalisation of Pos: it returns the index of the first
  // occurence of any member of Needles in Haystack. If no occurence is found,
  // FirstOf returns 0.

  if (Haystack = '') or (Needles = []) then begin
    Result := 0;
    Exit;
  end;

  Result := 1;
  while (Result <= Length(Haystack)) do begin
    if (Haystack[Result] in Needles) then
      Break
    else
      Inc(Result);
  end;

  if (Result > Length(Haystack)) then
    Result := 0;
end;

function HexDigitToInt(Digit: Char): Cardinal;
begin
  case Digit of
    '0':      Result := 0;
    '1':      Result := 1;
    '2':      Result := 2;
    '3':      Result := 3;
    '4':      Result := 4;
    '5':      Result := 5;
    '6':      Result := 6;
    '7':      Result := 7;
    '8':      Result := 8;
    '9':      Result := 9;
    'a', 'A': Result := 10;
    'b', 'B': Result := 11;
    'c', 'C': Result := 12;
    'd', 'D': Result := 13;
    'e', 'E': Result := 14;
    'f', 'F': Result := 15;
  else
    raise EConvertError.Create(Format(HexToIntError, [Digit]));
  end;
end;

function HexToInt(const HexValue: String): Cardinal;
var
  I:     Cardinal;
  Shift: Cardinal;
begin
  if (HexValue = '') then
    raise EConvertError.Create(Format(HexToIntError, [HexValue]));

  try
    Result := 0;
    Shift  := 0;

    for I := Length(HexValue) downto 1 do begin
      Result := Result + (HexDigitToInt(HexValue[I]) shl Shift);
      Inc(Shift, 4);
    end;
  except
    on EConvertError do
      raise EConvertError.Create(Format(HexToIntError, [HexValue]));
  end;
end;

function LastPos(const Needle, Haystack: String; Start: Integer = -1): Integer;
var
  I:        Integer;
  StartPos: Integer;
  TokenLen: Integer;
begin
  // Consider the substring of Haystack from the Start'th index to the right.
  // What is the rightmost occurence of Needle?

  // This function uses the exact same algorithm as IdUnicode's LastPosW. The
  // sole difference between the two is that LastPosW uses WideStrings and this
  // uses Strings. It would be nice to be able to pull the algorithm out, but
  // I don't think that's possible with Strings and WideStrings not being
  // objects.

  Result := 0;
  TokenLen := Length(Needle);

  // Get starting position
  if Start < 0 then
    Start := Length(Haystack);

  StartPos := Length(Haystack) - TokenLen + 1;
  if (Start < StartPos) then
    StartPos := Start;

  // Search for the string
  for I := StartPos downto 1 do begin
    if (Copy(Haystack, I, TokenLen) = Needle) then begin
      Result := I;
      Break;
    end;
  end;
end;

function Localhost(IPType: TIdIPVersion): String;
begin
  case IPType of
    Id_IPv4: Result := '127.0.0.1';
    Id_IPv6: Result := '::1';
  else
    Result := '';
    raise Exception.Create('Unknown TIdIPVersion');
  end;
end;

function StartsWith(const S, Prefix: String): Boolean;
begin
  // Returns true iff a string starts with a specified prefix.

  Result := Copy(S, 1, Length(Prefix)) = Prefix;
end;

function StreamToStr(Data: TStream): String;
var
  OriginalPosition: Int64;
  S:                TStringStream;
begin
  if not Assigned(Data) then begin
    Result := '';
    Exit;
  end;

  OriginalPosition := Data.Position;
  Data.Seek(0, soFromBeginning);
  try
    S := TStringStream.Create('');
    try
      S.CopyFrom(Data, 0);
      Result := S.DataString;
    finally
      S.Free;
    end;
  finally
    Data.Seek(OriginalPosition, soFromBeginning);
  end;
end;

function StripLeadingZeroes(const S: String): String;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = '0') do Inc(I);

  Result := Copy(S, I, Maxint);
end;

function WithoutFirstAndLastChars(const S: String): String;
begin
  Result := Copy(S, 2, Length(S) - 2);
end;

//******************************************************************************
//* TIdIPAddressParser                                                         *
//******************************************************************************
//* TIdIPAddressParser Public methods ******************************************

class function TIdIPAddressParser.AddressToMask(Netmask: String; IPVersion: TIdIPVersion): Cardinal;
var
  Addr4: Cardinal;
  Addr6: TIdIPv6AddressRec;
  I:     Integer;
begin
  // Given a netmask ("255.0.0.0", "ffff::", etc.), return the number of
  // significant bits, such as one might use to describe a CIDR block. For
  // instance, this function will convert something like "255.255.255.0" to
  // "24", or "ffff:ffff:ffff::" to "96".
  //
  // See MaskToAddress for the inverse function.

  Result := 0;

  if TIdSimpleParser.IsNumber(Netmask) then begin
    // Netmask is already a number of significant bits, like you might get in
    // "192.168.0.0/24".
    Result := StrToInt(Netmask);
    Exit;
  end;

  if Self.IsIPv4Address(Netmask) then begin
    Addr4 := Self.InetAddr(Netmask);

    while ((Addr4 and $80000000) <> 0) do begin
      Inc(Result);
      Addr4 := (Addr4 and $7fffffff) shl 1;
    end;
  end
  else begin
    try
      Self.ParseIPv6Address(Netmask, Addr6);

      for I := Low(Addr6) to High(Addr6) do begin
        if ((Addr6[I] and $8000) = 0) then Break;
        
        while ((Addr6[I] and $8000) <> 0) do begin
          Inc(Result);
          Addr6[I] := (Addr6[I] and $7fff) shl 1;
        end;
      end;
    except
      on EConvertError do
        raise EBadParameter.Create(Format('''%s'' is not an IPv4 or IPv6 netmask', [Netmask]));
    end;
  end;
end;

class function TIdIPAddressParser.ExpandIPv6Address(const IPAddress: String): String;
var
  Address: TIdIPv6AddressRec;
  I:       Integer;
begin
  // Take an address like "2002:DEAD:BEEF::1" and return
  // "2002:DEAD:BEEF:0:0:0:0:1".

  if not TIdIPAddressParser.IsIPv6Address(IPAddress) then
    raise EConvertError.Create(Format(IPv6AddrError, [IPAddress]));

  Self.ParseIPv6Address(IPAddress, Address);

  Result := '';
  for I := Low(Address) to High(Address) do
    Result := Result + Format('%x', [Address[I]]) + ':';
  Delete(Result, Length(Result), 1);
end;

class function TIdIPAddressParser.IncIPAddress(const IPAddress: String;
                                               N: Cardinal = 1): String;
var
  IPv6: TIdIPv6AddressRec;
begin
  if TIdIPAddressParser.IsIPv4Address(IPAddress) then
    Result := IncIPv4Address(IPAddress, N)
  else if TIdIPAddressParser.IsIPv6Address(IPAddress) then begin
    Self.ParseIPv6Address(IPAddress, IPv6);
    IncIPv6Address(IPv6, N);
    Result := Self.IPv6AddressToStr(IPv6);
  end
  else
    raise EConvertError.Create(Format(IPAddrError, [IPAddress]));
end;

class function TIdIPAddressParser.IncIPv4Address(const IPAddress: String;
                                                 N: Cardinal = 1): String;
var
  Addy: Cardinal;
begin
  Addy := Self.InetAddr(IPAddress);

  if (Addy > High(Cardinal) - N) then
    Addy := N - (High(Cardinal) - Addy) - 1
  else
    Inc(Addy, N);

  Result := IPv4AddressToStr(Addy);
end;

class procedure TIdIPAddressParser.IncIPv6Address(var Address: TIdIPv6AddressRec;
                                               N: Cardinal = 1);
var
  Carry: Cardinal;
  I:     Integer;
  Max:   Cardinal;
begin
  // [0]  [1]  [2]  [3]  [4]  [5]  [6]  [7] <-- indices
  // f00d:f00d:f00d:f00d:f00d:f00d:f00d:f00d
  Max := High(Word) + 1;

  Carry := Address[7] + (N and $ffff);
  Address[7] := Carry mod Max;
  Carry := Carry div Max;

  Carry := Carry + Address[6] + ((N and $ffff0000) shr 16);
  Address[6] := Carry mod Max;
  Carry := Carry div Max;

  for I := 5 downto 0 do begin
    Carry := Carry + Address[I];
    Address[I] := Carry mod Max;
    Carry := Carry div Max;
  end;

  if (Carry > 0) then
    Address[7] := Carry - 1;
end;

class function TIdIPAddressParser.InetAddr(const IPv4Address: String): Cardinal;
var
  B1, B2, B3, B4: Cardinal;
  Work:           String;
begin
  // Return a Cardinal representing a IPv4 address. The most significant bits of
  // the result represent the first number of the IPv4 address. Thus, "1.2.3.4"
  // returns $01020304

  Work := IPv4Address;

  try
    B1 := StrToInt(Fetch(Work, '.'));
    B2 := StrToInt(Fetch(Work, '.'));
    B3 := StrToInt(Fetch(Work, '.'));
    B4 := StrToInt(Work);
  except
    raise EConvertError.Create(Format(IPv4AddrError, [IPv4Address]));
  end;

  if (B1 > 255) or (B2 > 255) or (B3 > 255) or (B4 > 255) then
    raise EConvertError.Create(Format(IPv4AddrError, [IPv4Address]));

  Result := (B1 shl 24)
         or (B2 shl 16)
         or (B3 shl 8)
         or  B4;
end;

class function TIdIPAddressParser.IPv4AddressToStr(Address: Cardinal): String;
begin
  Result := IntToStr((Address shr 24) and $ff) + '.'
          + IntToStr((Address shr 16) and $ff) + '.'
          + IntToStr((Address shr 8) and $ff) + '.'
          + IntToStr(Address and $ff);
end;

class function TIdIPAddressParser.IPv6AddressToStr(Address: TIdIPv6AddressRec; ShowEncapsulatedIPv4: Boolean = false): String;
  const
    NS_INT16SZ   =   2;       // #/bytes of data in a u_int16_t
    NS_IN6ADDRSZ =   16;      // IPv6 T_AAAA
    EndOfAddress = NS_IN6ADDRSZ / NS_INT16SZ;
  type
    TPlace = record
      Base: Integer;
      Len:  Integer;
    end;
  function BestRunOfZeroes(Address: TIdIPv6AddressRec): TPlace;
  var
    Cur: TPlace;
    I: Integer;
  begin
    Result.Base := -1;
    Cur.Base := -1;
    for I := Low(Address) to High(Address) do begin
      if (Address[I] = 0) then begin
        if (Cur.Base = -1) then begin
          Cur.Base := I;
          Cur.Len := 1;
        end
        else
          Inc(Cur.Len);
      end
      else begin
        if (Cur.Base <> -1) then begin
          if (Result.Base = -1) or (Cur.Len > Result.Len) then
            Result := Cur;
          Cur.Base := -1;
        end;
      end;
    end;
    if (Cur.Base <> -1) then begin
      if (Result.Base = -1) or (Cur.Len > Result.Len) then
        Result := Cur;
    end;
    if (Result.Base <> -1) and (Result.Len < 2) then
      Result.Base := -1;
  end;
  function InsideRunOfZeroes(Index: Integer; Best: TPlace): Boolean;
  begin
    Result := (Best.Base <> -1)
          and (Index >= Best.Base) and (Index < (Best.Base + Best.Len))
  end;
  function TrailingRunOfZeroes(Place: TPlace): Boolean;
  begin
    Result := (Place.Base <> -1) and ((Place.Base + Place.Len) = EndOfAddress)
  end;
  function MayBeEncapsulatedIPv4(Address: TIdIPv6AddressRec; Index: Integer; Best: TPlace): Boolean;
  begin
    // Return true if Address is of the form ::13.1.68.3 or ::FFFF:129.144.52.38
    Result := (Index = 6)
          and (Best.Base = 0)
          and ((Best.Len = 6) or ((Best.Len = 5) and (Address[5] = $ffff)));
  end;
var
  Best: TPlace;
  I: Integer;
begin
  // This method is a translation of FreeBSD's inet_ntop6, in libc. A notable
  // departure is you can choose to show encapsulated IPv4 addresses or not.
  // That is, if ShowEncapsulatedIPv4 is true then this method will return
  // "::0.1.0.0" (like inet_ntop6 does), but by default will return "::1:0", a
  // "pure" IPv6 address.
  Best := BestRunOfZeroes(Address);

  Result := '';
  for I := Low(Address) to High(Address) do begin
    // Are we inside the best run of $00s?
    if InsideRunOfZeroes(I, Best) then begin
      if (I = Best.Base) then
        Result := Result + ':';
      Continue;
    end;

    // Are we following an initial run of $00s or any real hex?
    if (I <> Low(Address)) then
      Result := Result + ':';

    // Is this address an encapsulated IPv4?
    if ShowEncapsulatedIPv4 and MayBeEncapsulatedIPv4(Address, I, Best) then begin
      Result := Result + Self.IPv4AddressToStr((Address[6] shl 16) or Address[7]);
      Break;
    end;

    // Otherwise print the word
    Result := Result + Format('%x', [Address[I]]);
  end;

  // Was it a trailing run of $00's?
  if TrailingRunOfZeroes(Best) then
    Result := Result + ':'
end;

class function TIdIPAddressParser.IPVersion(Address: String): TIdIPVersion;
begin
  // Return the IP version of the Address parameter: Address = '127.0.0.1'
  // returns Id_IPv4; Address = '::1' returns Id_IPv6; Address = 'foo' returns
  // Id_IPUnknown

  if TIdIPAddressParser.IsIPv4Address(Address) then
    Result := Id_IPv4
  else if TIdIPAddressParser.IsIPv6Address(Address) then
    Result := Id_IPv6
  else
    Result := Id_IPUnknown;
end;

class function TIdIPAddressParser.IsIPAddress(IpVersion: TIdIPVersion;
                                              const Token: String): Boolean;
begin
  case IpVersion of
    Id_IPv4: Result := Self.IsIPV4Address(Token);
    Id_IPv6: Result := Self.IsIPv6Address(Token);
  else
    raise EParserError.Create('Unknown TIdIPVersion in IsIPAddress');
  end;
end;

class function TIdIPAddressParser.IsIPAddress(const Token: String): Boolean;
begin
  Result := Self.IsIPV4Address(Token) or Self.IsIPv6Address(Token);
end;

class function TIdIPAddressParser.IsIPv4Address(const Token: String): Boolean;
var
  Address: String;
begin
  Address := Token;
  Result := TIdSimpleParser.IsByte(Fetch(Address, '.'))
        and TIdSimpleParser.IsByte(Fetch(Address, '.'))
        and TIdSimpleParser.IsByte(Fetch(Address, '.'))
        and TIdSimpleParser.IsByte(Address);
end;

class function TIdIPAddressParser.IsIPv6Address(const Token: String): Boolean;
var
  Addy: TIdIPv6AddressRec;
begin
  try
    ParseIPv6Address(Token, Addy);
    Result := true;
  except
    Result := false;
  end;
end;

class function TIdIPAddressParser.IsIPv6Reference(const Token: String): Boolean;
begin
  Result := (Token <> '') and (Token[1] = '[') and (Token[Length(Token)] = ']');

  Result := Result
        and Self.IsIPv6Address(WithoutFirstAndLastChars(Token));
end;

class function TIdIPAddressParser.IsNumericAddress(const Token: String): Boolean;
begin
  Result := Self.IsIPv6Reference(Token)
         or Self.IsIPv6Address(Token)
         or Self.IsIPv4Address(Token);
end;

class function TIdIPAddressParser.MaskToAddress(NumberSignificantBits: Cardinal; IPVersion: TIdIPVersion): String;
  procedure MaskWord(var NumberSignificantBits: Cardinal; var AddressPart: Word);
  const
    BitsInWord = 16;
  var
    I:           Integer;
    BitsForWord: Cardinal;
    Shift:       Integer;
  begin
    if (NumberSignificantBits = 0) then Exit;

    AddressPart := 0;
    Shift       := BitsInWord - 1;
    BitsForWord := Min(BitsInWord, NumberSignificantBits);
    for I := 1 to BitsForWord do begin
      AddressPart := AddressPart or (1 shl Shift);
      Dec(Shift);
    end;
    Dec(NumberSignificantBits, BitsForWord);
  end;
var
  Addr:  Cardinal;
  Addr6: TIdIPv6AddressRec;
  I:     Cardinal;
  Shift: Integer;
begin
  // Convert something like "24" to "255.255.255.0", or "96" to "ffff:ffff:ffff::"
  //
  // See AddressToMask for the inverse function.

  Result := '';
  if (IPVersion = Id_IPv4) then begin
    if (NumberSignificantBits > BitsInCardinal) then
      raise EBadParameter.Create('Too many significant bits for an IPv4 mask');

    Addr  := 0;
    Shift := BitsInCardinal - 1;
    for I := 1 to NumberSignificantBits do begin
      Addr := Addr or (1 shl Shift);
      Dec(Shift);
    end;
    Result := Self.IPv4AddressToStr(Addr);
  end
  else if (IPVersion = Id_IPv6) then begin
    if (NumberSignificantBits > (BitsInCardinal*4)) then
      raise EBadParameter.Create('Too many significant bits for an IPv6 mask');

    FillChar(Addr6, SizeOf(TIdIPv6AddressRec), 0);
    for I := Low(TIdIPv6AddressRec) to High(TIdIPv6AddressRec) do
      MaskWord(NumberSignificantBits, Addr6[I]);

    Result := IPv6AddressToStr(Addr6);
  end
  else
    raise EBadParameter.Create('Cannot convert "' + IntToStr(NumberSignificantBits) + '" into an address of unknown IP version');
end;

class function TIdIPAddressParser.NetworkForIPv4(Address: String; Mask: String): String;
var
  Addr:  Cardinal;
  M:     Cardinal;
begin
  Addr := Self.InetAddr(Address);
  M    := Self.InetAddr(Mask);

  Result := Self.IPv4AddressToStr(Addr and M);
end;

class function TIdIPAddressParser.NetworkForIPv6(Address: String; Mask: String): String;
var
  Addr6: TIdIPv6AddressRec;
  I:     Integer;
  M6:    TIdIPv6AddressRec;
  Net6:  TIdIPv6AddressRec;
begin
  FillChar(Net6, SizeOf(Addr6), 0);
  Self.ParseIPv6Address(Address, Addr6);
  Self.ParseIPv6Address(Mask, M6);

  for I := Low(TIdIPv6AddressRec) to High(TIdIPv6AddressRec) do
    Net6[I] := Addr6[I] and M6[I];

  Result := Self.IPv6AddressToStr(Net6);
end;

class function TIdIPAddressParser.NetworkFor(Address: String; Mask: String): String;
begin
  case Self.IPVersion(Address) of
    Id_IPv4: Result := Self.NetworkForIPv4(Address, Mask);

    Id_IPv6: Result := Self.NetworkForIPv6(Address, Mask);
  else
    raise EBadParameter('''' + Address + ''' not an IPv4 or IPv6 address');
  end;
end;

class function TIdIPAddressParser.NetworkFor(Address: String; NumSignificantBits: Cardinal): String;
begin
  Result := Self.NetworkFor(Address, Self.MaskToAddress(NumSignificantBits, Self.IPVersion(Address)))
end;

class procedure TIdIPAddressParser.ParseIPv6Address(const IPv6Address: String;
                                                    var Address: TIdIPv6AddressRec);
  const
    IPv6Delim    = ':';
    IPv4Delim    = '.';
    IPv6ZeroAbbr = IPv6Delim + IPv6Delim; // The short-hand way of writing a
                                          // large sequence of zeroes

  procedure ParseChunk(Chunk: String;
                       var Address: TIdIPv6AddressRec;
                       var WordCount: Integer;
                       AllowTrailingIPv4: Boolean);
  var
    C: Cardinal;
    W: String;
    I: Cardinal;
  begin
    FillChar(Address, SizeOf(Address), 0);
    I         := 0;
    WordCount := 0;

    if (Chunk = '') then Exit;

    while (Chunk <> '') and (Pos(IPv6Delim, Chunk) > 0) do begin
      W := Fetch(Chunk, IPv6Delim);

      if (Length(W) > 4) or not TIdSimpleParser.IsHexNumber(W) then
        raise EConvertError.Create('Invalid IPv6 double byte');

      Address[I] := HexToInt(W);
      Inc(WordCount);
      Inc(I);
    end;

    if AllowTrailingIPv4 and (Pos(IPv4Delim, Chunk) > 0) then begin
      if (Chunk <> '') then begin
        C := (StrToInt(Fetch(Chunk, IPv4Delim)) shl 8)
           or StrToInt(Fetch(Chunk, IPv4Delim));
        if (C > High(Address[I])) then
          raise EConvertError.Create('Invalid IPv4 byte');
        Address[I] := C and $0000FFFF;

        Inc(I);

        C := (StrToInt(Fetch(Chunk, IPv4Delim)) shl 8)
           or StrToInt(Chunk);
        if (C > High(Address[I])) then
          raise EConvertError.Create('Invalid IPv4 byte');
        Address[I] := C and $0000FFFF;
        Inc(WordCount, 2);
      end
    end
    else begin
      if (Length(Chunk) > 4) or not TIdSimpleParser.IsHexNumber(Chunk) then
        raise EConvertError.Create('Invalid IPv6 double byte');

      Address[I] := HexToInt(Chunk);
      Inc(WordCount);
    end;
  end;
var
  FirstAddy:       TIdIPv6AddressRec;
  FirstChunk:      String;
  FirstChunkSize:  Integer;
  I, J:            Integer;
  SecondAddy:      TIdIPv6AddressRec;
  SecondChunk:     String;
  SecondChunkSize: Integer;
begin
    // There are 5 possible kinds of IPv6 addresses:
    // no ::
    // ::
    // ::xxx
    // xxx::
    // xxx::yyy
    // Additionally, there may be an IPv4 address as the last 2 words, like
    // 2002:dead:beef:feed:babe:f00d:192.168.0.1 or
    // ::127.0.0.1

  if (IPv6Address = '') then
    raise EConvertError.Create(Format(IPv6AddrError, [IPv6Address]));

  if Self.IsIPv4Address(IPv6Address) then
    raise EConvertError.Create(Format(IPv6AddrError, [IPv6Address]));

  if (Pos(IPv6Delim, IPv6Address) = 0) and (Pos(IPv6ZeroAbbr, IPv6Address) = 0) then
    raise EConvertError.Create(Format(IPv6AddrError, [IPv6Address]));

  FillChar(Address, SizeOf(Address), 0);
  try
    // We split the address into two chunks - the "pre-::" and the "post-::"
    // chunks.
    SecondChunk := IPv6Address;
    FirstChunk := Fetch(SecondChunk, IPv6ZeroAbbr);

    // We parse the chunks - since SecondChunk could well be empty, we
    // conditionally allow FirstChunk to contain a trailing IPv4 address.
    // SecondChunk can always contain a trailing IPv4 address.
    ParseChunk(FirstChunk, FirstAddy, FirstChunkSize, Pos(IPv4Delim, FirstChunk) > 0);
    ParseChunk(SecondChunk, SecondAddy, SecondChunkSize, true);

    // And finally we copy the two chunks into the answer.
    for I := 0 to FirstChunkSize - 1 do
      Address[I] := FirstAddy[I];

    J := Low(SecondAddy);
    for I := High(Address) - SecondChunkSize + 1 to High(Address) do begin
      Address[I] := SecondAddy[J];
      Inc(J);
    end;
  except
    on EConvertError do
      raise EConvertError.Create(Format(IPv6AddrError, [IPv6Address]));
  end;
end;

//******************************************************************************
//* TIdSimpleParser                                                            *
//******************************************************************************
//* TIdSimpleParser Public methods *********************************************

class function TIdSimpleParser.IsAlphaNumeric(const Token: String): Boolean;
begin
  Result := Token <> '';

  if Result then
    Result := ContainsOnly(Token, Alphabet + Digits);
end;

class function TIdSimpleParser.IsByte(const Token: String): Boolean;
var
  N: Integer;
  E: Integer;
begin
  Result := Token <> '';

  if Result then begin
    Val(Token, N, E);
    Result := Result and (E = 0) and (N >= 0) and (N < 256);
  end;
end;

class function TIdSimpleParser.IsDigit(C: Char): Boolean;
begin
  Result := C in Digits;
end;

class function TIdSimpleParser.IsFQDN(const Token: String): Boolean;
var
  I:    Integer;
  Name: String;
  Labl: String;
begin
  Result := Token <> '';

  if Result then begin
    Name := Token;
    while (Name <> '') do begin
      Labl := Fetch(Name, '.');

      for I := 1 to Length(Labl) do
        Result := Result and (Labl[I] in (Alphabet + Digits + ['-']));

      if Result then
        Result := Result and (Labl <> '')
                         and (Labl[1] in Alphabet)
                         and Self.IsAlphaNumeric(Labl[Length(Labl)])
                         and (Length(Labl) < 64);
    end;
  end;
end;

class function TIdSimpleParser.IsHexNumber(const Number: String): Boolean;
begin
  Result := Number <> '';

  if Result then
    Result := ContainsOnly(Number, HexDigits);
end;

class function TIdSimpleParser.IsLetter(C: Char): Boolean;
begin
  Result := C in Alphabet;
end;

class function TIdSimpleParser.IsNumber(const Number: String): Boolean;
begin
  Result := Number <> '';

  if Result then
    Result := ContainsOnly(Number, Digits);
end;

constructor TIdSimpleParser.Create;
begin
  inherited Create;

  Self.ResetCurrentLine;
end;

function TIdSimpleParser.CurrentLine: Cardinal;
begin
  Result := fCurrentLine;
end;

function TIdSimpleParser.Eof: Boolean;
var
  C: Char;
  N: Integer;
begin
  N := Self.Source.Read(C, 1);
  Result := (N = 0);

  if not Result then
    Self.Source.Seek(-1, soFromCurrent);
end;

function TIdSimpleParser.Peek: Char;
begin
  Result := Self.ReadOctet;
  Self.Source.Seek(-1, soFromCurrent);
end;

function TIdSimpleParser.PeekLine: String;
var
  C: Char;
begin
  Result := '';
  while not Self.Eof and not (Self.Peek = #13) do begin
    C := Self.ReadOctet;
    Result := Result + C;
  end;

  Self.Source.Seek(-Length(Result), soFromCurrent);
end;

function TIdSimpleParser.ReadOctet: Char;
begin
  Self.Source.Read(Result, 1);
end;

function TIdSimpleParser.ReadOctets(Count: Cardinal): String;
begin
  Result := '';
  while not Self.Eof and (Count > 0) do begin
    Result := Result + Self.ReadOctet;
    Dec(Count);
  end;
end;

function TIdSimpleParser.ReadLn: String;
var
  C: Char;
begin
  Result := Self.PeekLine;
  Self.Source.Seek(Length(Result), soFromCurrent);

  if (not Self.Eof) then begin
    C := Self.ReadOctet;
    Self.Assert(C = #13, Format(DamagedLineEnd,
                                [Self.CurrentLine,
                                 '#$0D',
                                 '#$' + IntToHex(Ord(C), 2)]));
    C := Self.ReadOctet;
    Self.Assert(C = #10, Format(DamagedLineEnd,
                                [Self.CurrentLine,
                                 '#$0A',
                                 '#$' + IntToHex(Ord(C), 2)]));
  end;

  Self.IncCurrentLine;
end;

function TIdSimpleParser.ReadFirstNonBlankLine: String;
begin
  Self.SkipBlankLines;
  Result := Self.ReadLn;
end;

procedure TIdSimpleParser.SkipBlankLines;
begin
  while not Self.Eof and (Self.PeekLine = '') do
    Self.ReadLn;
end;

//* TIdSimpleParser Protected methods ******************************************

procedure TIdSimpleParser.Assert(Condition: Boolean; Msg: String);
begin
  if not Condition then
    raise EParserError.Create(Msg);
end;

procedure TIdSimpleParser.ResetCurrentLine;
begin
  fCurrentLine := 1;
end;

//* TIdSimpleParser Private methods ********************************************

procedure TIdSimpleParser.IncCurrentLine;
begin
  Inc(fCurrentLine);
end;

end.
