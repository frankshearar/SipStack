unit IdSimpleParser;

interface

uses
  Classes, SysUtils;

type
  // for IdCore or whatever. Indy10 defines these but Indy9 doesn't
  TIdIPVersion = (Id_IPv4, Id_IPv6);

  TIdSimpleParser = class(TObject)
  private
    fCurrentLine: Cardinal;
    fSource:      TStream;
    procedure IncCurrentLine;
  protected
    procedure ResetCurrentLine;
  public
    class function IsAlphaNumeric(const Token: String): Boolean;
    class function IsByte(const Token: String): Boolean;
    class function IsDigit(const C: Char): Boolean;
    class function IsFQDN(const Token: String): Boolean;
    class function IsHexNumber(const Number: String): Boolean;
    class function IsLetter(const C: Char): Boolean;
    class function IsIPAddress(const IpVersion: TIdIPVersion; const Token: String): Boolean;
    class function IsIPv4Address(const Token: String): Boolean;
    class function IsIPv6Address(const Token: String): Boolean;
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

    property  Source: TStream read fSource write fSource;
  end;

  EParser = class(Exception);

const
  BadSyntax        = 'Bad syntax';
  DamagedLineEnd   = 'Damaged line end at line %d, expected %s but was %s';
  EmptyInputStream = 'Empty input stream';
  MalformedToken   = 'Malformed %s: ''%s''';

const
  Alphabet        = ['a'..'z', 'A'..'Z'];
  Digits          = ['0'..'9'];
  MarkChars       = ['-', '_', '.', '!', '~', '*', '''', '(', ')'];
  UnreservedChars = Alphabet + Digits + MarkChars;

implementation

uses
  IdGlobal;

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

class function TIdSimpleParser.IsDigit(const C: Char): Boolean;
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

class function TIdSimpleParser.IsIPAddress(const IpVersion: TIdIPVersion; const Token: String): Boolean;
begin
  case IpVersion of
    Id_IPv4: Result := Self.IsIPV4Address(Token);
    Id_IPv6: Result := Self.IsIPv6Address(Token);
  else
    raise EParser.Create('Unknown TIdIPVersion in IsIPAddress');
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

class function TIdSimpleParser.IsLetter(const C: Char): Boolean;
begin
  Result := C in Alphabet;
end;

class function TIdSimpleParser.IsIPv4Address(const Token: String): Boolean;
var
  Address: String;
begin
  Address := Token;
  Result := Self.IsByte(Fetch(Address, '.'))
        and Self.IsByte(Fetch(Address, '.'))
        and Self.IsByte(Fetch(Address, '.'))
        and Self.IsByte(Address);
end;

class function TIdSimpleParser.IsIPv6Address(const Token: String): Boolean;
  function IsIPv6Chunk(Words: String): Boolean;
  var
    Word: String;
  begin
    Result := Words <> '';

    while (Words <> '') and (IndyPos(':', Words) > 0) do begin
      Word := Fetch(Words, ':');
      Result := Result and Self.IsHexNumber(Word) and (Length(Word) <= 4);
    end;

    if (Words <> '') then begin
      if (IndyPos('.', Words) = 0) then
        Result := Result and Self.IsHexNumber(Words) and (Length(Words) <= 4)
      else
        Result := Result and Self.IsIPv4Address(Words);
    end;
  end;
var
  S:     String;
  Words: String;
begin
  Result := Token <> '';

  if Result then begin
    Result := true;
    S := Token;

    // There are 5 possible kinds of IPv6 addresses:
    // no ::
    // ::
    // ::xxx
    // xxx::
    // xxx::yyy

    Words := Fetch(S, '::');
    if (Words <> '') then
      Result := Result and IsIPv6Chunk(Words);

    if (S <> '') then
      Result := Result and IsIPv6Chunk(S);
  end;
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
    Assert(C = #13, Format(DamagedLineEnd, [Self.CurrentLine, '#$0D', '#$' + IntToHex(Ord(C), 2)]));
    C := Self.ReadOctet;
    Assert(C = #10, Format(DamagedLineEnd, [Self.CurrentLine, '#$0A', '#$' + IntToHex(Ord(C), 2)]));
  end;

  Self.IncCurrentLine;
end;

function TIdSimpleParser.ReadFirstNonBlankLine: String;
begin
  Result := Self.ReadLn;
  while (Result = '') and not Self.Eof do
    Result := Self.ReadLn;
end;

//* TIdSimpleParser Protected methods ******************************************

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
