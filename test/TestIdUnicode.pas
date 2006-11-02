{
  (c) 2004 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIdUnicode;

interface

uses
  IdUnicode, TestFrameworkEx;

type
  TestUnicodeFunctions = class(TThreadingTestCase)
  published
    procedure TestCodePointToUTF8;
    procedure TestHighSurrogate;
    procedure TestIsHighSurrogate;
    procedure TestIsLowSurrogate;
    procedure TestLowSurrogate;
    procedure TestLastPosW;
    procedure TestPosW;
    procedure TestStringReplaceW;
    procedure TestSurrogateToCodePoint;
    procedure TestUTF16LEToUTF8;
    procedure TestUTF8ToUTF16LE;
  end;

implementation

uses
  SysUtils, TestFramework, Types;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('IdUnicode unit tests');
  Result.AddTest(TestUnicodeFunctions.Suite);
end;

//******************************************************************************
//* TestUnicodeFunctions                                                       *
//******************************************************************************
//* TestUnicodeFunctions Published methods *************************************

procedure TestUnicodeFunctions.TestCodePointToUTF8;
var
  I: DWord;
begin
  // 0000 0000-0000 007F
  for I := 0 to $7F do
    CheckEquals(Chr(I),
                CodePointToUTF8(I),
                Chr(I));

  // 0000 0080-0000 07FF
  CheckEquals(#$C2#$80,
              CodePointToUTF8($80),
              '$80');

  CheckEquals(#$DF#$BF,
              CodePointToUTF8($7FF),
              '$7FF');

  // 0000 0800-0000 FFFF
  CheckEquals(#$E0#$A0#$80,
              CodePointToUTF8($800),
              '$800');
  CheckEquals(#$E2#$98#$BA,
              CodePointToUTF8($263A),
              '$263A');
  CheckEquals(#$E7#$BF#$BF,
              CodePointToUTF8($7FFF),
              '$7FFF');
  CheckEquals(#$E8#$AA#$9E,
              CodePointToUTF8($8A9E),
              '$8A9E');
  CheckEquals(#$EF#$BF#$BF,
              CodePointToUTF8($FFFF),
              '$FFFF');

  // 0001 0000-001F FFFF
  CheckEquals(#$F0#$90#$80#$80,
              CodePointToUTF8($10000),
              '$10000');
  CheckEquals(#$F7#$BF#$BF#$BF,
              CodePointToUTF8($1FFFFF),
              '$1FFFFF');

  // 0020 0000-03FF FFFF
  CheckEquals(#$F8#$88#$80#$80#$80,
              CodePointToUTF8($200000),
              '$200000');
  CheckEquals(#$FB#$BF#$BF#$BF#$BF,
              CodePointToUTF8($3FFFFFF),
              '$3FFFFFF');

  // 0400 0000-7FFF FFFF
  CheckEquals(#$FC#$84#$80#$80#$80#$80,
              CodePointToUTF8($4000000),
              '$4000000');
  CheckEquals(#$FC#$87#$BF#$BF#$BF#$BF,
              CodePointToUTF8($07FFFFFF),
              '$07FFFFFF');
  CheckEquals(#$FD#$BF#$BF#$BF#$BF#$BF,
              CodePointToUTF8($7FFFFFFF),
              '$7FFFFFFF');
end;

procedure TestUnicodeFunctions.TestHighSurrogate;
begin
  CheckEquals(IntToHex($D950, 4),
              IntToHex(HighSurrogate($00064321), 4),
              '$00064321');
end;

procedure TestUnicodeFunctions.TestIsHighSurrogate;
var
  I: Word;
begin
  for I := 0 to UnicodeHighSurrogateStart - 1 do
    Check(not IsHighSurrogate(I), IntToStr(I));

  for I := UnicodeHighSurrogateStart to UnicodeHighSurrogateEnd do
    Check(IsHighSurrogate(I), IntToStr(I));

  for I := UnicodeHighSurrogateEnd + 1 to High(I) do
    Check(not IsHighSurrogate(I), IntToStr(I));
end;

procedure TestUnicodeFunctions.TestIsLowSurrogate;
var
  I: Word;
begin
  for I := 0 to UnicodeLowSurrogateStart - 1 do
    Check(not IsLowSurrogate(I), IntToStr(I));

  for I := UnicodeLowSurrogateStart to UnicodeLowSurrogateEnd do
    Check(IsLowSurrogate(I), IntToStr(I));

  for I := UnicodeLowSurrogateEnd + 1 to High(I) do
    Check(not IsLowSurrogate(I), IntToStr(I));
end;

procedure TestUnicodeFunctions.TestLowSurrogate;
begin
  CheckEquals(IntToHex($DF21, 4),
              IntToHex(LowSurrogate($00064321), 4),
              '$00064321');
end;

procedure TestUnicodeFunctions.TestLastPosW;
var
  Needle, Haystack: WideString;
begin
  Needle := 'abc';

  CheckEquals(0, LastPosW(Needle, Haystack), 'Empty string');

  Haystack := 'xxx';
  CheckEquals(0, LastPosW(Needle, Haystack), 'No Needle in the Haystack');

  Haystack := 'abc';
  CheckEquals(1, LastPosW(Needle, Haystack), 'Needle = Haystack');
  CheckEquals(1,
              LastPosW(Needle, Haystack, Length(Haystack) + 1),
              'Needle = Haystack, starting beyond end of Haystack');
  CheckEquals(1,
              LastPosW(Needle, Haystack, -Length(Haystack)),
              'Needle = Haystack, starting before beginning of Haystack');

  Haystack := 'abcdef';
  CheckEquals(1, LastPosW(Needle, Haystack), 'Needle at beginning of Haystack');

  Haystack := 'defabc';
  CheckEquals(4, LastPosW(Needle, Haystack), 'Needle at end of Haystack');

  Haystack := 'defabcghi';
  CheckEquals(4, LastPosW(Needle, Haystack), 'Needle inside Haystack');

  Haystack := 'abcabc';
  CheckEquals(4, LastPosW(Needle, Haystack), 'Haystack = 2xNeedle');

  Haystack := 'abcabc';
  CheckEquals(4, LastPosW(Needle, Haystack, 4), 'Haystack = 2xNeedle, starting in the middle');

  Haystack := 'abcabcabc';
  CheckEquals(1, LastPosW(Needle, Haystack, 3), 'Haystack = 3xNeedle, starting at 3');
  CheckEquals(4, LastPosW(Needle, Haystack, 4), 'Haystack = 3xNeedle, starting at 4');
  CheckEquals(4, LastPosW(Needle, Haystack, 5), 'Haystack = 3xNeedle, starting at 5');
  CheckEquals(7, LastPosW(Needle, Haystack, 7), 'Haystack = 3xNeedle, starting at 7');
  CheckEquals(7, LastPosW(Needle, Haystack),    'Haystack = 3xNeedle');

  // _aa_aa ( "_" = ZeroWidthNonBreakingSpaceChar)
  Needle := ZeroWidthNonBreakingSpaceChar;
  Haystack := ZeroWidthNonBreakingSpaceChar; // Character 1
  Haystack := Haystack + 'aa';
  Haystack := Haystack + ZeroWidthNonBreakingSpaceChar; // Character 4
  Haystack := Haystack + 'aa';
  CheckEquals(1, LastPosW(Needle, Haystack, 3), 'UCS-2, starting at 3');
  CheckEquals(4, LastPosW(Needle, Haystack, 4), 'UCS-2, starting at 4');
  CheckEquals(4, LastPosW(Needle, Haystack, 5), 'UCS-2, starting at 5');
  CheckEquals(4, LastPosW(Needle, Haystack),    'UCS-2, starting at left');
end;

procedure TestUnicodeFunctions.TestPosW;
var
  SearchString: WideString;
  Substring:    WideString;
begin
  // The kanji for what, romanised, reads "nihongo".
  SearchString := WideChar($65E5);
  SearchString := SearchString + WideChar($672C) + WideChar($8A9E);

  // Substrings not in the search string
  CheckEquals(0, PosW('', ''), 'Empty string in the empty string');
  CheckEquals(0, PosW('foo', ''), '"foo" in the empty string');
  CheckEquals(0, PosW('', 'foo'), 'Empty string in "foo"');
  CheckEquals(0, PosW('foo', 'bar'), '"foo" in "bar"');
  CheckEquals(0, PosW(SearchString, 'foo'), '"nihongo" in "foo"');
  CheckEquals(0, PosW('foo', SearchString), '"foo" in "nihongo"');

  // Substrings at the beginning of the search string
  CheckEquals(1, PosW('foo', 'foo'), '"foo" in "foo"');
  CheckEquals(1, PosW('foo', 'foobar'), '"foo" in "foobar"');
  CheckEquals(1, PosW('f', 'foo'), '"f" in "foo"');

  // Position in repeated substrings
  CheckEquals(1, PosW('foo', 'foofoo'), '"foo" in "foofoo"');

  // Substrings near or at the end of the search string
  CheckEquals(4, PosW('bar', 'foobar'), '"bar" in "foobar"');
  CheckEquals(4, PosW('b',   'foobar'), '"b" in "foobar"');
  CheckEquals(4, PosW('ba',  'foobar'), '"ba" in "foobar"');
  CheckEquals(5, PosW('a',   'foobar'), '"a" in "foobar"');
  CheckEquals(5, PosW('ar',  'foobar'), '"ar" in "foobar"');
  CheckEquals(6, PosW('r',   'foobar'), '"r" in "foobar"');

  // Unicode substrings at the beginning of the search string
  Substring := Copy(SearchString, 1, 1);
  CheckEquals(1, PosW(Substring, SearchString), '"ni" in "nihongo"');
  Substring := Copy(SearchString, 1, 2);
  CheckEquals(1, PosW(Substring, SearchString), '"nihon" in "nihongo"');
  Substring := Copy(SearchString, 1, 3);
  CheckEquals(1, PosW(Substring, SearchString), '"nihongo" in "nihongo"');

  // Unicode Substrings near or at the end of the search string
  Substring := Copy(SearchString, 3, 1);
  CheckEquals(3, PosW(Substring, SearchString), '"go" in "nihongo"');
  Substring := Copy(SearchString, 2, 2);
  CheckEquals(2, PosW(Substring, SearchString), '"hongo" in "nihongo"');
  Substring := Copy(SearchString, 2, 1);
  CheckEquals(2, PosW(Substring, SearchString), '"hon" in "nihongo"');
end;

procedure TestUnicodeFunctions.TestStringReplaceW;
begin
  CheckEquals('', StringReplaceW('', '', '', false), 'Empty strings');
  CheckEquals('', StringReplaceW('', '', '', true), 'Empty strings, replace all');

  CheckEquals('', StringReplaceW('foo', 'foo', '', false), 'Replace entire string with empty string');
  CheckEquals('', StringReplaceW('foo', 'foo', '', true), 'Replace entire string with empty string, replace all');

  CheckEquals('', StringReplaceW('', 'foo', '', false), 'Replace something in empty string with empty string');
  CheckEquals('', StringReplaceW('', 'foo', '', true), 'Replace something in empty string with empty string, replace all');

  CheckEquals('', StringReplaceW('', 'foo', 'bar', false), 'Replace something in empty string');
  CheckEquals('', StringReplaceW('', 'foo', 'bar', true), 'Replace something in empty string, replace all');

  CheckEquals('bar', StringReplaceW('foo', 'foo', 'bar', false), 'Replace foo with bar in "foo"');
  CheckEquals('bar', StringReplaceW('foo', 'foo', 'bar', true), 'Replace foo with bar in "foo", replace all');

  CheckEquals('barfoo not foo', StringReplaceW('foofoo not foo', 'foo', 'bar', false), 'Replace foo with bar in "foofoo not foo"');
  CheckEquals('barbar not bar', StringReplaceW('foofoo not foo', 'foo', 'bar', true), 'Replace foo with bar in "foofoo not foo", replace all');

  CheckEquals('barFOO not foo', StringReplaceW('FOOFOO not foo', 'FOO', 'bar', false), 'Replace FOO with bar in "FOOFOO not foo"');
  CheckEquals('barbar not foo', StringReplaceW('FOOFOO not foo', 'FOO', 'bar', true), 'Replace FOO with bar in "FOOFOO not foo", replace all');
end;

procedure TestUnicodeFunctions.TestSurrogateToCodePoint;
begin
  CheckEquals(IntToHex($00064321, 4),
              IntToHex(SurrogateToCodePoint($D950, $DF21), 4),
              '$00064321');
end;

procedure TestUnicodeFunctions.TestUTF16LEToUTF8;
var
  C:     Char;
  UTF16: WideString;
  UTF8:  String;
begin
  for C := Chr(32) to Chr(127) do
    CheckEquals(C, UTF16LEToUTF8(C), C);

  UTF8 := 'A multi-character string';
  CheckEqualsW(UTF8,
               UTF16LEToUTF8(UTF8),
               UTF8);

  UTF8 := #$41#$E2#$89#$A2#$CE#$91#$2E;
  UTF16 := WideChar($0041);
  UTF16 := UTF16 + WideChar($2262) + WideChar($0391) + WideChar($002E);
  CheckEqualsW(UTF8,
               UTF16LEToUTF8(UTF16),
               '"A<NOT IDENTICAL TO><ALPHA>."');

  UTF8 := #$48#$69#$20#$4D#$6F#$6D#$20#$E2#$98#$BA#$21;
  UTF16 := WideChar($0048);
  UTF16 := UTF16
         + WideChar($0069) + WideChar($0020) + WideChar($004D) + WideChar($006F)
         + WideChar($006D) + WideChar($0020) + WideChar($263A) + WideChar($0021);
  CheckEqualsW(UTF8,
               UTF16LEToUTF8(UTF16),
               '"Hi Mom <WHITE SMILING FACE>!"');

  UTF8 := #$E6#$97#$A5#$E6#$9C#$AC#$E8#$AA#$9E;
  UTF16 := WideChar($65E5);
  UTF16 := UTF16 + WideChar($672C) + WideChar($8A9E);
  CheckEqualsW(UTF8,
               UTF16LEToUTF8(UTF16),
               '"nihongo" in kanji');

  UTF8 := #$EF#$BB#$BF;
  UTF16 := WideChar($FEFF);
  CheckEqualsW(UTF8,
               UTF16LEToUTF8(UTF16),
               '$FEFF');

  // Character 0006 4321
  UTF8 := #$F1#$A4#$8C#$A1;
  UTF16 := WideChar($D950);
  UTF16 := UTF16 + WideChar($DF21);
  CheckEqualsW(UTF8,
               UTF16LEToUTF8(UTF16),
               '$0006 4321');
end;

procedure TestUnicodeFunctions.TestUTF8ToUTF16LE;
var
  C:     Char;
  UTF16: WideString;
  UTF8:  String;
begin
  // This test needs more test points, esp for characters
  // 0020 0000-03FF FFFF and 0400 0000-7FFF FFFF

  for C := Chr(32) to Chr(127) do
    CheckEquals(C, UTF8ToUTF16LE(C), C);

  UTF8 := 'A multi-character string';
  CheckEqualsW(UTF8,
               UTF8ToUTF16LE(UTF8),
               UTF8);

  UTF8 := #$41#$E2#$89#$A2#$CE#$91#$2E;
  UTF16 := WideChar($0041);
  UTF16 := UTF16 + WideChar($2262) + WideChar($0391) + WideChar($002E);
  CheckEqualsW(UTF16,
               UTF8ToUTF16LE(UTF8),
               '"A<NOT IDENTICAL TO><ALPHA>."');

  UTF8 := #$48#$69#$20#$4D#$6F#$6D#$20#$E2#$98#$BA#$21;
  UTF16 := WideChar($0048);
  UTF16 := UTF16
         + WideChar($0069) + WideChar($0020) + WideChar($004D) + WideChar($006F)
         + WideChar($006D) + WideChar($0020) + WideChar($263A) + WideChar($0021);
  CheckEqualsW(UTF16,
               UTF8ToUTF16LE(UTF8),
               '"Hi Mom <WHITE SMILING FACE>!"');


  UTF8 := #$E6#$97#$A5#$E6#$9C#$AC#$E8#$AA#$9E;
  UTF16 := WideChar($65E5);
  UTF16 := UTF16 + WideChar($672C) +WideChar($8A9E);
  CheckEqualsW(UTF16,
               UTF8ToUTF16LE(UTF8),
               '"nihongo" in kanji');

  UTF8 := #$EF#$BB#$BF;
  CheckEqualsW(WideChar($FEFF),
               UTF8ToUTF16LE(UTF8),
               '$FEFF');

  // Character 0006 4321
  UTF8 := #$F1#$A4#$8C#$A1;
  UTF16 := WideChar($D950);
  UTF16 := UTF16 + WideChar($DF21);
  CheckEqualsW(UTF16,
               UTF8ToUTF16LE(UTF8),
               '$0006 4321');
end;

initialization
  RegisterTest('Unicode', Suite);
end.
