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
  TIdIPv6AddressRec = packed array[0..7] of Word;

  TIdIPAddressParser = class(TObject)
    class function  ExpandIPv6Address(const IPAddress: String): String;
    class function  IncIPAddress(const IPAddress: String;
                                 N: Cardinal = 1): String;
    class function  IncIPv4Address(const IPAddress: String;
                                   N: Cardinal = 1): String;
    class procedure IncIPv6Address(var Address: TIdIPv6AddressRec;
                                   N: Cardinal = 1);
    class function  InetAddr(const IPv4Address: String): Cardinal;
    class function  IPv4AddressToStr(Address: Cardinal): String;
    class function  IPv6AddressToStr(Address: TIdIPv6AddressRec): String;
    class function  IsIPAddress(IpVersion: TIdIPVersion;
                                const Token: String): Boolean;
    class function  IsIPv4Address(const Token: String): Boolean;
    class function  IsIPv6Address(const Token: String): Boolean;
    class function  IsIPv6Reference(const Token: String): Boolean;
    class function  IsNumericAddress(const Token: String): Boolean;
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
  MarkChars       = ['-', '_', '.', '!', '~', '*', '''', '(', ')'];
  UnreservedChars = Alphabet + Digits + MarkChars;

const
  FetchDefaultDelete = true;
  FetchDefaultDelimiter = ' '; // Do not localise

function EncodeNonLineUnprintableChars(S: String): String;
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
function WithoutFirstAndLastChars(const S: String): String;

implementation

uses
  StrUtils;

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

function WithoutFirstAndLastChars(const S: String): String;
begin
  Result := Copy(S, 2, Length(S) - 2);
end;

//******************************************************************************
//* TIdIPAddressParser                                                         *
//******************************************************************************
//* TIdIPAddressParser Public methods ******************************************

class function TIdIPAddressParser.ExpandIPv6Address(const IPAddress: String): String;
var
  Address: TIdIPv6AddressRec;
begin
  if not TIdIPAddressParser.IsIPv6Address(IPAddress) then
    raise EConvertError.Create(Format(IPv6AddrError, [IPAddress]));

  Self.ParseIPv6Address(IPAddress, Address);
  Result := Self.IPv6AddressToStr(Address);
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
  B1, B2, B3, B4: Byte;
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

    Result := (B1 shl 24)
           or (B2 shl 16)
           or (B3 shl 8)
           or  B4;
  except
    raise EConvertError.Create(Format(IPv4AddrError, [IPv4Address]));
  end;
end;

class function TIdIPAddressParser.IPv4AddressToStr(Address: Cardinal): String;
begin
  Result := IntToStr((Address shr 24) and $ff) + '.'
          + IntToStr((Address shr 16) and $ff) + '.'
          + IntToStr((Address shr 8) and $ff) + '.'
          + IntToStr(Address and $ff);
end;

class function TIdIPAddressParser.IPv6AddressToStr(Address: TIdIPv6AddressRec): String;
  function StripLeadingZeroes(Digits: String): String;
  var
    I: Integer;
  begin
    I := 1;
    while (I <= Length(Digits)) and (Digits[I] = '0') do
      Inc(I);
    Result := Copy(Digits, I, Length(Digits));

    if (Result = '') then Result := '0';
  end;
var
  I: Integer;
begin
  Result := '';
  for I := Low(Address) to High(Address) do
    Result := Result + StripLeadingZeroes(IntToHex(Address[I], 4)) + ':';

  Result := Copy(Result, 1, Length(Result) - 1);
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

class procedure TIdIPAddressParser.ParseIPv6Address(const IPv6Address: String;
                                                    var Address: TIdIPv6AddressRec);
  const
    IPv6Delim    = ':';
    IPv4Delim    = '.';
    IPv6ZeroAbbr = IPv6Delim + IPv6Delim; // The short-hand way of writing a
                                          // large sequence of zeroes
  procedure Swap(var Address: TIdIPv6AddressRec; X, Y: Cardinal);
  begin

    Address[X] := Address[X] xor Address[Y];
    Address[Y] := Address[X] xor Address[Y];
    Address[X] := Address[X] xor Address[Y];
  end;

  procedure Reverse(var Address: TIdIPv6AddressRec);
  var
    Temp: Word;
  begin
//    Swap(Address, 0, 7);
//    Swap(Address, 1, 6);
//    Swap(Address, 2, 5);
//    Swap(Address, 3, 4);
    Temp := Address[0];
    Address[0] := Address[1];
    Address[1] := Address[2];
    Address[2] := Address[3];
    Address[3] := Address[4];
    Address[4] := Address[5];
    Address[5] := Address[6];
    Address[6] := Address[7];
    Address[7] := Temp;
  end;

  procedure ParseChunk(Chunk: String;
                       var Address: TIdIPv6AddressRec;
                       var WordCount: Integer;
                       AllowTrailingIPv4: Boolean);
  var
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
        Address[I] := (StrToInt(Fetch(Chunk, IPv4Delim)) shl 8)
                    or StrToInt(Fetch(Chunk, IPv4Delim));
        Inc(I);
        Address[I] := (StrToInt(Fetch(Chunk, IPv4Delim)) shl 8)
                    or StrToInt(Chunk);
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
var
  I: Integer;
begin
  Result := Token <> '';

  if (Result) then
    for I := 1 to Length(Token) do begin
      Result := Result and (Self.IsDigit(Token[I])
                        or (Token[I] in Alphabet));
      if not Result then Break;
    end;
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
var
  I: Integer;
begin
  Result := Number <> '';

  if (Result) then
    for I := 1 to Length(Number) do begin
      Result := Result and (Self.IsDigit(Number[I]) or (Number[I] in ['a'..'f', 'A'..'F']));

      if not Result then Break;
    end;
end;

class function TIdSimpleParser.IsLetter(C: Char): Boolean;
begin
  Result := C in Alphabet;
end;

class function TIdSimpleParser.IsNumber(const Number: String): Boolean;
var
  I: Integer;
begin
  Result := Number <> '';

  if (Result) then
    for I := 1 to Length(Number) do begin
      Result := Result and Self.IsDigit(Number[I]);

      if not Result then Break;
    end;
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
