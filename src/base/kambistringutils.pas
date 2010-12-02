{
  Copyright 2000-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ String utilities.

  Also some operations on chars and PChars.
  And various convertions strings<->numbers.

  General comments for all procedures that have parameter like IgnoreCase:
  @unorderedList(
    @item(
      If such parameter has some default value, this default value should be
      @definitionList(
        @itemLabel @true
        @item for procedures that only read processed string
        @itemLabel @false
        @item(for procedures that can modify processed string (for safety,
          so that accidental modification should be harder))
      ))

    @item(
      If I don't write in docs for this procedure whether this
      procedure takes current locale into account (as current locale
      can change the meaning of "ignoring case"), then it means it
      @italic(does) take current locale into account.)
  )
}

unit KambiStringUtils;

interface

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  Variants, SysUtils, KambiUtils;

type
  { }
  TSearchOptions = set of (soMatchCase, soWholeWord, soBackwards);
  { A set of chars. }
  TSetOfChars = SysUtils.TSysCharSet;

const
  AllChars = [Low(Char) .. High(Char)];
  DefaultWordBorders = AllChars - ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  WhiteSpaces = [' ', #9, #10, #13];

function RandomString: string;

{ Replace all occurrences of FromPattern string to ToPattern string,
  within another string S.

  @code(StringReplaceAllTo1st(s, from, to)) is actually equivalent to
  simply @code(s := StringReplace(s, from, to, [rfReplaceAll, rfIgnoreCase])).
  So StringReplaceAllTo1st is just a wrapper for very common use case of
  StringReplace. }
procedure StringReplaceAllTo1st(var S: string;
  const FromPattern, ToPattern: string;
  IgnoreCase: boolean = true); overload;

{ Insert newline characters into string S, such that each line
  has at most MaxCol chars. Newline characters inserted is @link(NL).

  It tries to insert NL at the last character in OnBreakChars but still
  before MaxCol limit, and the character in OnBreakChars is deleted in this case.
  In other words, in most typical situation it simply breaks the string
  where the whitespace is, trying to make the line as long as possible within
  MaxCol limit. If no such character in OnBreakChars is found (e.g., you
  put a long line of non-white characters), it will still break the string
  at MaxCol position (so in this exceptional case, it will cause a break
  in the middle of the word).

  While breaking the string in the middle
  of the word in not nice, this allows us a safe feeling that this
  will always break the string into MaxCol chunks.

  This intelligently recognizes already
  existing newline characters (#13, #10, #13#10 or #10#13) in the string,
  so e.g. it will not insert more newline characters when they are not
  necessary. }
function BreakLine(const s: string; MaxCol: integer;
  onbreakChars: TSetOfChars = WhiteSpaces): string; overload;

{ Returns S with all chars in ExcludedChars deleted. }
function SDeleteChars(const s: string; const excludedChars: TSetOfChars): string;

{ Replace all occurrences of characters in FromChars with
  the new string / character. There are three overloaded versions:

  @orderedList(
    @item(SReplaceChars(string, string, string) looks in S for characters within
      FromChars, and replaces them with characters on appropriate position
      in ToChars. For example, SReplaceChars(S, 'ab', 'cd') replaces
      all occurrences of 'a' into 'c' and all occurrences of 'b' into 'd'.
      It must always be Length(FromChars) <= Length(ToChars).)

    @item(SReplaceChars(string, TSetOfChars, char) replaces all occurrences
      of any character in given set with the one specified character.)

    @item(SReplaceChars(string, char, char) simply replaces all occurrences
      of one character into another.))

  @groupBegin
}
function SReplaceChars(const s, FromChars, ToChars: string): string; overload;
function SReplaceChars(const s: string; FromChars: TSetOfChars; ToChar: char): string; overload;
function SReplaceChars(const s: string; FromChar, ToChar: char): string; overload;
{ @groupEnd }

{ Pad (fill from the left with character C) string S, until length
  of resulting string is at least Len.

  For example, @code(SPad('29', 4, '0')) gives '0029' }
function SPad(const s: string; len: integer; c: char = ' '): string; overload;

{ Pad (fill from the left)  with zeros string S, until length
  of resulting string is at least Len. It's actually just a shortcut for SPad
  with padding character set to '0'. }
function SZeroPad(const s: string; len: integer): string;

{ Convert uppercase letters to lowercase. Analogous to UpCase.
  Doesn't change other characters. Just like UpCase, this doesn't
  take current locale into account, and works only on English
  A-Z -> a-z letters. }
function LoCase(c: char): char;

function CharPos(c: char; const s: string; Offset: Integer = 1): integer;

{ Find first occurence of any character in Chars in string S.
  This is quite like FirstDelimiter but it takes parameter as TSetOfChars
  and has much more sensible name.

  BackCharsPos does the same, but from
  the end of the string (i.e. finds the last occurence).

  CharsPosEx searches starting from Offset char.

  They all return 0 if not found.

  @groupBegin }
function CharsPos(const chars: TSetOfChars; const s: string): integer;
function CharsPosEx(const chars: TSetOfChars; const s: string;
  Offset: Integer): integer;
function BackCharsPos(const chars: TSetOfChars; const s: string): integer;
{ @groupEnd }

{ Find @bold(last) occurence of SubString within S.
  0 if not found. Overloaded version is optimized for searching for
  single character. }
function BackPos(const SubString, S: string): Integer; overload;
function BackPos(const SubString: char; const S: string): Integer; overload;

{ Find first occurence of character in Delimiters. Name is analogous to
  LastDelimiter. Returns 0 if not found. }
function FirstDelimiter(const Delimiters, S: string): Integer;

{ Returns suffix of S starting from position P.
  Returns '' if P > length(S).
  Yes, this is simply equivalent to Copy(S, P, MaxInt). }
function SEnding(const s: string; P: integer): string;

function IsPrefix(const Prefix, S: string;
  IgnoreCase: boolean = true): boolean; overload;
function IsSuffix(const Suffix, S: string;
  IgnoreCase: boolean = true): boolean; overload;

{ Removes the prefix, if it is present. More precisely, if
  IsPrefix(Prefix, S, IgnoreCase) then returns S with this prefix
  removed. Else returns S. }
function PrefixRemove(const Prefix, S: string; IgnoreCase: boolean): string;

{ Like PrefixRemove, but checks for and removes Suffix. }
function SuffixRemove(const Suffix, S: string; IgnoreCase: boolean): string;

{ Appends to a string S DataSize bytes from Data. }
procedure SAppendData(var s: string; const Data; DataSize: integer);

{ A pointer to S[CharNum], that is just @@S[CharNum],
  avoiding range checking. }
function SChar(const s: string; CharNum: integer): PChar;

{ Check whether S[Index] = C, also checking is Index within S length.
  Return false if S is too short, or the chatacter differs.

  @groupBegin }
function SCharIs(const s: string; index: integer; c: char): boolean; overload;
function SCharIs(const s: string; index: integer; const chars: TSetOfChars): boolean; overload;
{ @groupEnd }

{ Replace typically unreadable characters in string S with #number notation.
  Useful for printing strings with some unprintable chars for
  debugging purposes. }
function SReadableForm(const s: string): string;

{ Return S[StartPosition..EndPosition].
  This is similar to standard Copy procedure,
  but last parameter is EndPosition instead of Count, which is more comfortable
  sometimes. }
function CopyPos(const s: string; StartPosition, EndPosition: integer): string;

{ Delete from S range of characters [StartPosition..EndPosition].
  Analogous to standard Delete but with EndPosition parameter (while
  standard Delete takes Count). }
procedure DeletePos(var S: string; StartPosition, EndPosition: Integer);

(*Find next part in the string S separated by delimiters
  TokenDelims. More precisely: search S, starting from position
  SeekPos, for the first character that is @italic(not in TokenDelims).
  Then, all subsequent characters that are not in TokenDelims are
  appended to the Result, until any character @italic(is in TokenDelims)
  is found. In effect, Result contains the whole part that was in TokenDelims.

  SeekPos is advanced to the position of the next character, i.e. the character
  right after the ending character that was in TokenDelims. In other words,
  SeekPos points to the position of the next "unprocessed" character in
  string S. Often you will want to make another call to NextToken, passing
  this SeekPos, and this way you can split your string S into parts
  delimited by TokenDelims.

  Returns '' if no more tokens available (SeekPos value at the end is
  unspecified).

  Typical use scenario (iterate over all tokens in the string) :

@longCode(#
  SeekPos := 1;
  repeat
    Token := NextToken(S, SeekPos);
    if Token = '' then break;
    { ... process_next_token (Token) ... }
  until false;
#)

  The above example will split the string into parts separated by whitespace.

  Note: it's much easier to use CreateTokens instead of this procedure.
  But this procedure gives you quite more flexibility. *)
function NextToken(const S: string; var SeekPos: Integer;
  const TokenDelims: TSetOfChars = WhiteSpaces): string;

{ NextTokenOnce works just like NextToken, but doesn't advance the SeekPos
  position. This means that it's quite useless when you're interested
  in @italic(all) tokens inside some string, but it's also more comfortable
  when you're interested in only @italic(one) token inside some string.
  When SeekPos = 1, this is the first token. }
function NextTokenOnce(const s: string; SeekPos: integer = 1;
  const TokenDelims: TSetOfChars = WhiteSpaces): string;
  overload;

{ Returns TDynStringArray with tokens extracted from S.
  Token is something delimited by TokenDelims.
  TokenDelims are not contained in resulting items.
  E.g. CreateTokens('foo, bar', [' ', ',']) returns TDynStringArray
  with 2 items: 'foo' and 'bar'. }
function CreateTokens(const s: string;
  const TokenDelims: TSetOfChars = WhiteSpaces): TDynStringArray;

{ Advanced version of NextToken, that avoids splitting string inside
  pairs of "restricted" characters, like quotes.

  Basically, just like NextToken, this finds the next part
  in the string S separated by delimiters TokenDelims.
  Looks for the first character not in TokenDelims
  (starting from position SeekPos), and then reads the string up to
  the character in TokenDelims.
  @italic(But the characters surrounded by a pair of same characters
  from RestrAreas are never treated like TokenDelims.)

  This way you can e.g. split a string by whitespaces,
  but still request that whitespaces inside ' and ' or between " and "
  be ignored (not split on them).

  For example, with default values for TokenDelims and RestrAreas,
  you can reliably split XML attributes like

@preformatted(
  <foo val1='value in single quotes'
       val2="value in double quotes"
       val3='value in single quotes, double quote inside (") ignored'
       val4=value_without_the_quotes
  />
)

  Parsing such string with NextTokenRestr will result in six tokens returned:
  '<foo', then one token for each valX=..., then '/>'. }
function NextTokenRestr(const s: string; var SeekPos: integer;
  const TokenDelims: TSetOfChars = WhiteSpaces;
  const RestrAreas: TSetOfChars = ['''','"']): string; overload;

{ Find substring SubText within Text. Returns 0 if not found.
  Similar to a standard Pos function, with some improvements.

  @param(StartPosition Starts searching for SubText starting from this position.
    Note that the resulting position is still returned with respect
    to the string beginning. Just like standard PosEx.)

  @param(Count Looks only at Count characters from Text.
    You can say that the search is done only within Copy(Text, StartPosition, Count).)

  @param(Options Various searching options:

    @unorderedList(
      @item(soMatchCase: makes searching case-sensitive (by default,
        case is ignored, taking locale into account).)

      @item(soWholeWord: looks only for SubText occurrences surrounded
        by characters from WordBorders (or the beginning/end of Text).

        Note that, while the beginning/end of Text is always treated like a word border,
        but the mere beginning/end of the searching range (StartPosition, Count)
        is not a word border.
        For example FindPos('cat', 'foocat dog', 4, MaxInt, [soWholeWord])
        will answer 0 (not found), because the only 'cat' occurence is not
        surrounded by default word borders.)

      @item(soBackwards: search from the end, that is return rightmost
        found occurence.)
    )
  ) }
function FindPos(const SubText, Text: string; StartPosition, Count: integer;
  Options: TSearchOptions;
  const WordBorders: TSetOfChars = DefaultWordBorders): integer; overload;

{ Check is given match (MatchStart, MatchLength) good result of
  FindPos call for the same arguments.

  In other words, this checks something more than just the equality of
  Copy(Text, MatchStart, MatchLength) with SubText.
  This precisely checks if FindPos(SubText, Text, Options, WordBorders)
  would return given MatchStart (and MatchLength is equal to length
  of SubText).
  For example if soMatchCase in Options, then SubText is compared
  case-sensitive. If soWholeWords in Options, then given match
  must be surrounded by WordBorders in Text. And so on.

  The typical usage for this is when you make an interactive text editor
  application, and you have some text seleected, and you have to check
  could this selection be done by previous FindPos successful search.

  @groupBegin }
function MatchingFind(const SubText, Text: string;
  MatchStart, MatchLength: integer; Options: TSearchOptions;
  const WordBorders: TSetOfChars): boolean; overload;
function MatchingFind(const SubText, Text: string;
  MatchStart, MatchLength: integer; matchCase, wholeWord: boolean;
  const WordBorders: TSetOfChars): boolean; overload;
{ @groupEnd }

{ Find substring SubText within Text, requiring SubText to be surrounded
  by WordBorders. Always case-sensitive. Returns 0 if not found.

  This is equivalent to FindPos(SubText, Text, 1, Length(Text),
  [soWholeWord, soMatchCase], WordBorders). The only difference
  between standard Pos function is that this looks only for "words"
  --- that is, occurrences surrounded by WordBorders. }
function FindWordPos(const SubText, Text: string;
  const WordBorders: TSetOfChars = DefaultWordBorders): integer; overload;

{ Return word surrounding Position inside Text.
  A "word" is determined by looking around Text[Position], as far as you
  can, until the beginning/end of Text is found or character in WordBorders. }
function GetWordAtPos(const Text: string; Position: integer;
  const WordBorders: TSetOfChars = DefaultWordBorders): string; overload;

{ Return rightmost RPart characters from S.
  If RPart > Length(S) then returns S. }
function SRight(const s: string; const rpart: integer): string;

{ If S = '' then returns NextPart, else returns S + PartSeparator + NextPart. }
function SAppendPart(const s, PartSeparator, NextPart: string): string;

{ Read whole file contents to string.

  If AllowStdIn, then FileName = '-' (one dash) is treated specially:
  we will read stdin whole (Pascal Input) stream. Note that the current
  implementation of this always changes newline into NL (current OS newline),
  and may add additional newline at the end of the file (this may be fixed,
  to return more accurately stdin contents; for usual text file reading,
  this doesn't matter). }
function FileToString(const FileName: string;
  const AllowStdIn: boolean = false): string;

procedure StringToFile(const FileName, contents: string);

type
  EDeformatError = class(Exception);

{ Parse a string according to the given format, returning the
  values corresponding to placeholders %x in format string.

  Format parameter is a sequence of white spaces, placeholders like %d or %f,
  and other characters. More precisely:

  @unorderedList(
    @item(If RelaxedWhitespaceChecking = @true (that's the default value)
      then 1 or more white spaces in Format must correspond to 1 or more
      any whitespace characters in Data. I.e., the actual number and kind
      of whitespace in Format and Data doesn't have to match --- it's
      only important that @italic(some whitespace in Format) correspond
      to @italic(some whitespace in Data).)

    @item(@code(%d) in Format means an integer value (possibly signed) in Data.
      Args should have a pointer to Integer variable on the appropriate
      position.)

    @item(@code(%f) in Format means a float value (possibly signed, possibly
      with a dot) in Data. Args should have a pointer to Float variable
      on the appropriate position.)

    @item(@code(%.single.), @code(%.double.), @code(%.extended.) are like
      @code(%f), but they
      specify appropriate variable type in Args.
      Since DeFormat can't check the type validity of your pointers,
      always be sure to pass in Args pointers to appropriate types.)

    @item(@code(%s) in Format means a string (will end on the first whitespace)
      in Data. Args should contain a pointer to an AnsiString
      on the appropriate position. Note that I mean it --- a pointer
      to an AnsiString, not just a string typecasted into a pointer.
      I.e., if S is AnsiString, Args should contain @@S, not Pointer(S).

      Note that a string may be empty in some cases, e.g. Format = '%d %s'
      and Data = '123 ' will result in the empty string as second Args.)

    @item(@code(%%) in Format means a one % sign in Data.)

    @item(All the other characters (non-white, not %x sequences above)
      should be present in Data exactly like they are specified in Format.
      IgnoreCase controls is the letter case checked. When
      RelaxedWhitespaceChecking = @false then white-space characters
      are treated just like non-white chars: they must match exactly
      between Format and Data.)
  )

  Format must always match the whole Data --- in other words, when
  we finished reading the Format, Data should be finished too.
  The exception is at the beginning and end of Data, if
  RelaxedWhitespaceChecking = @true : then at the beginning and end of Data
  any number of white-space is allowed.

  For DeFormat, the opposite must also be true: when we finished reading
  Data, Format should be finished too. However, for TryDeFormat, it's
  allowed for Data to end prematurely. TryDeFormat returns how many Args
  were initialized.

  Note that while usually you will want RelaxedWhitespaceChecking = @true,
  sometimes it can be needed to set this to @false not only to get
  strickter checking, but also to get some things matching that otherwise
  wouldn't match. For example, consider Data = 'first  second apple'
  and Format = 'first %s second %s'. With RelaxedWhitespaceChecking
  these things @italic(do not match) --- because the 1st space character
  in the Format string "consumes" the 1st and 2nd space characters
  in the Data. Then '%s' is matched to the word 'second', and the
  word 'second' is compared with 'apple' and they do not match.
  If you want such Data and Format to match, you must pass
  RelaxedWhitespaceChecking = @true. Then the first '%s' will be matched
  to '' (empty string).

  This was written because both JclSscanf and scanf units were buggy.
  (see openGL.testy/nehe10).

  @raises(EDeformatError In case of any error --- mismatch between Format
    and Data. Note that in case of error, some of Args may be initialized,
    and some not --- no guarantees here, sorry.) }
procedure DeFormat(Data: string; const Format: string;
  const args: array of pointer;
  const IgnoreCase: boolean = true;
  const RelaxedWhitespaceChecking: boolean = true); overload;
function TryDeFormat(Data: string; const Format: string;
  const args: array of pointer;
  const IgnoreCase: boolean = true;
  const RelaxedWhitespaceChecking: boolean = true): integer; overload;

{ Extract file extensions from a file filter usually specified
  a TOpenDialog.Filter value.

  More precisely: expects FileFilter to be in the form of
  @code('xxxx|name1.ext1;name2.ext2'). Where "xxxx" is just about anything
  (it is ignored), and in fact whole "xxxx|" (with bar) may be omitted.
  The rest (after "|") is treated as a filename list, separated by semicolon ";".

  As Extensions, we set an array of all extensions extracted from these
  filenames. For example above, we would set Extensions to array
  with two items: @code(['.ext1', '.ext2']). }
procedure GetFileFilterExts(const FileFilter: string; var Extensions: TDynStringArray);

{ Extract file filter name, from a file filter usually specified
  a TOpenDialog.Filter value.

  More precisely: if we do not see bar "|" character, then this is
  the filter name. Otherwise, everything on the right of "|" is "extensions"
  and everything on the left is "filter name".

  Additionally, if filter name ends with extensions value in parenthesis,
  they are removed. In other words, for 'Pascal files (*.pas)|*.pas',
  this will return just 'Pascal files'. The '(*.pas)' was removed
  from the filter name, because we detected this just repeats the extensions
  on the right of "|". Extensions on the right of "|" must be separated by
  semicolons, extensions within parenthesis on the left of "|" may
  be separated by semicolons ";" or colons ",". }
function GetFileFilterName(const FileFilter: string): string;

{ Search in FileFilter for the bar character "|", and return everything
  after it. This is a simple basis for GetFileFilterExts.

  If no "|" found, we return an empty string (in other words,
  file filter without "|" is treated as just a filter name, without
  any extensions). }
function GetFileFilterExtsStr(const FileFilter: string): string;

{ Replace all strings in Patterns with corresponding strings in Values.
  This is similar to standard StringReplace, but this does many
  replaces at once.

  Patterns and Values arrays must have equal length.
  Patterns[0] will be replaced with Values[0], Patterns[1] with Values[0] etc.
  Patterns are scanned from left to right, that is if two pattern occurrences
  overlap --- we will detect the leftmost one. If both patterns start
  at the same place (this means that one pattern is a prefix of the other),
  we will choose the first pattern in Patterns table.

  Using this avoids a common trap at repeated search-replace operations.
  A naive implementation of doing many search-replace over the same string
  is like

@longCode(#
  Result := S;
  Result := StringReplace(Result, Patterns[0], Values[0], [rfReplaceAll]);
  Result := StringReplace(Result, Patterns[1], Values[1], [rfReplaceAll]);
  etc.
#)

  But the above fails badly when inserting some Values[] creates
  an occurence of Pattern checked later. For example, when Values[0]
  contains inside whole Patterns[1]. More exotic situations involve
  when some Values[] glues with previous string contents to make
  a pattern detected later. This means that you could replace the same
  content many times, which is usually not what you want.

  That's why you should instead use this function for such situations.

  Options cannot contain soBackwards flag. }
function SReplacePatterns(const s: string; const patterns, values: array of string; Options: TSearchOptions): string;

function SCharsCount(const s: string; c: char): Cardinal; overload;
function SCharsCount(const s: string; const Chars: TSetOfChars): Cardinal; overload;

{ Remove from the string S everything after the first hash "#" character.
  Removes also this very "#" character.

  If string doesn't contain hash character, it's simply returned.

  Useful for interpreting simple text files when you want to treat
  things after "#" like a comment. }
function STruncateHash(const s: string): string;

{ Return the value to reproduce exactly string S by Format procedure.
  Saying simply, this doubles the "%" characters inside the string.
  The intention is to make such string that
  @code(Format(SUnformattable(S), []) = S). In other words, "quote"
  any suspicious "%" characters in S for Format. }
function SUnformattable(const s: string): string;

{ Compare strings, taking into account current locale.
  This simply does AnsiCompareStr or AnsiCompareText, depending on IgnoreCase.

  Returns value < 0 when S1 < S2, returns 0 when S1 = S2 and value > 0
  when S1 > S2. }
function SAnsiCompare(const s1, s2: string; IgnoreCase: boolean): Integer;

{ Check if strings are equal, taking into account current locale.
  Shortcut for SAnsiCompare(S1, S2) = 0 }
function SAnsiSame(const s1, s2: string; IgnoreCase: boolean): boolean;

type
  TPercentReplace = record
    { @noAutoLinkHere }
    c: char;
    { @noAutoLinkHere }
    s: string;
  end;

  EUnknownPercentFormat = class(Exception);

{ Searches for %x patterns and replaces them with specified strings.
  Something like a more generalized Format routine.

  More precisely: every two-char sequence that starts with PercentChar
  and then is followed by one of Replaces[I].c characters is replaced
  with appropriate Replaces[i].s. Moreover, a pair of two PercentChar
  characters is replaced with one PercentChar character.

  @italic(For example), assume that Replaces contains two items:
  @code((c: 'B'; s: '<bold>'), (c: 'b'; s: '</bold>')).
  Then @code(SPercentReplace('100%% of cats are %Bcute%b', Replaces)) will return
  string @code('100% of cats are <bold>cute</bold>').

  EUnknownPercentFormat is raised if we will see two-char sequence
  that starts with PercentChar and then is followed by character that
  is not any Replaces[i].c and is not PercentChar. Also, a single PercentChar
  at the end of the string is an error.

  @italic(For example), assume that Replaces contains the same two items as
  previously. Following calls will result in EUnknownPercentFormat being raised:
  @code(SPercentReplace('Unknown sequence %x', Replaces)),
  @code(SPercentReplace('Unterminated sequence %', Replaces)).

  If ErrorOnUnknownPercentFormat is @false, then EUnknownPercentFormat will
  not be raised. Instead, incorrect sequence (like %x or unterminated % in
  examples above) will simply be left in the string.

  Of course, replacing is done intelligently. Which means that
  e.g. sequence of four % characters will be correctly transformed into
  two % characters.

  Note that IgnoreCase is used to match characters for Replaces[I].c.
  IgnoreCase is not used when it comes to comparing with PercentChar character,
  i.e. even when PercentChar will be set to some letter, it will always
  be compared in case-sensitive manner, regardless of IgnoreCase value.

  It is undefined (meaning: don't do it) what happens if Replaces array
  contains more than once the same character C, or if any character C
  in Replaces array is equal to PercentChar.

  ReplacementsDone, if passed, will return how many replacements were done.
  Not counting "meaningless" replacements of pair of PercentChar to one
  PercentChar (that is, we count only actual replacements from Replaces
  array).

  @raises(EUnknownPercentFormat In case of error in InitialFormat string,
    if ErrorOnUnknownPercentFormat is @true.)

  @groupBegin }
function SPercentReplace(const InitialFormat: string;
  const Replaces: array of TPercentReplace;
  out ReplacementsDone: Cardinal;
  ErrorOnUnknownPercentFormat: boolean = true;
  PercentChar: char ='%';
  IgnoreCase: boolean = false): string; overload;

function SPercentReplace(const InitialFormat: string;
  const Replaces: array of TPercentReplace;
  ErrorOnUnknownPercentFormat: boolean = true;
  PercentChar: char ='%';
  IgnoreCase: boolean = false): string; overload;
{ @groupEnd }

{ Replace %d in the NamePattern with Index.

  This is something like a more specialized Format (sprintf for you, C folks),
  working similar to SPercentReplace.

  @unorderedList(
    @item(%d is replaced with Index.

      You can insert a non-negative number between % and d, to pad
      the counter with zeros to desired length. For example, with Counter = 2,
      %d is replaced with just "2", %2d is replaced with "02",
      %4d is replaced with "0002".)

    @item(%% is replaced with single percent char %.)

    @item(Everything else is just copied to resulting string.
      Not recognized %-patterns are also just copied.
      Much like SPercentReplace with ErrorOnUnknownPercentFormat = false
      (since FormatIndexedName main use is to replace end-user
      supplied filenames on command-line, it tries to be tolerant to errors).)
  )

  @groupBegin }
function FormatIndexedName(const NamePattern: string;
  const Index: Integer; out ReplacementsDone: Cardinal): string; overload;
function FormatIndexedName(const NamePattern: string;
  const Index: Integer): string; overload;
{ @groupEnd }

function AnsiUpperCaseChar(C: char): char;
function AnsiLowerCaseChar(C: char): char;

{ Returns S with S[1] character replaced with AnsiUpperCaseChar(S[1])
  (unless S does not have 1st char, i.e. S = '') }
function SAnsiUpperFirstChar(const S: string): string;

{ convertions ------------------------------------------------------------ }

const
  { I should restrain from adding more similiar BoolToStrXxx constants
    below, since there are *so* many possibilities here (on/off, ON/OFF,
    On/Off, yes/no, YES/NO etc.) that it's quite useless trying to
    gather them all here. }

  { Convert boolean to string, using a simple table lookup.

    I don't use BoolToStr function from SysUtils unit,
    since there are differences in FPC implementations:
    @unorderedList(
      @item(In FPC <= 2.0.4, BoolToStr takes one param
        and returns 'FALSE' or 'TRUE' string.)
      @item(In FPC > 2.0.4 (trunk (2.3.1 currently), and fixes_2_2 (2,1.3)),
        BoolToStr was changed for Delphi compat. Now when passed only 1 param
        it returns 0 or -1 (who the hell needs such BoolToStr interpretation ?).

        You have to pass 2nd param to BoolToStr as @true
        to get strings 'False' and 'True'. But this makes it non-compileable
        in FPC <= 2.0.4. So to call BoolToStr like I want to, I would have
        to use ugly $ifdefs...))

    So I decided to use my BoolToStr table throughout my units.
    When I'll switch fully to FPC > 2.0.4, I'll drop this and use
    BoolToStr function from SysUtils unit. }
  BoolToStr: array[boolean] of string=('FALSE','TRUE');
  BoolToStrYesNo: array[boolean]of string = ('No','Yes');

{ Convert digit (like number 0) to character (like '0').
  Use only for arguments within 0..9 range. }
function DigitAsChar(b: byte): char;

{ Convert digit character (like '0') to a number (like 0).
  Use only for characters in '0'...'9' range. }
function DigitAsByte(c: char): byte;

{ Convert integer to string, padding string with zeros if needed. }
function IntToStrZPad(n: integer; minLength: integer): string;

{ Convert integer to string, in base-Base (like base-16) numeral system.
  For digits above '9', we will use upper letters 'A', 'B'...  etc.
  That's also why Base cannot be larger than 'Z'-'A' + 1 + 10
  (we would not have enough digits then).

  Overloaded versions with MinLength pad result with zeros to have
  at least MinLength.

  @groupBegin }
function IntToStrBase(const n: Int64; Base: Byte): string; overload;
function IntToStrBase(      n: QWord; Base: Byte): string; overload;
function IntToStrBase(const n: Int64; Base: Byte; minLength: Cardinal): string; overload;
function IntToStrBase(const n: QWord; Base: Byte; minLength: Cardinal): string; overload;
{ @groupEnd }

{ Convert integer to binary (base-2 numeral system).
  MinLength means to left-pad result with zeros if necessary. }
function IntToStr2(n: Int64;
  const MinLength: Cardinal = 1;
  const ZeroDigit: char = '0';
  const OneDigit: char = '1';
  const MinusSign: char = '-'): string; overload;

{ Convert integer to hexadecimal (base-16 numeral system).
  @groupBegin }
function IntToStr16(const n: Int64; const minLength: Cardinal = 1): string; overload;
function IntToStr16(const n: QWord; const minLength: Cardinal = 1): string; overload;
{ @groupEnd }

function ToStr(const args: array of const): string;
function VarRecToStr(const v: TVarRec): string;

{ Returns Ptr as 0xXXX... hexadecimal value. "0x" is not a Pascal standard
  for coding hex values, but it's so popular that users are more likely
  to "get" 0x notation. }
function PointerToStr(Ptr: Pointer): string;

{ This returns IntToStr(Value) with ThousandSeparator inserted to
  separate thousands (only if ThousandSeparator <> #0). }
function IntToStrThousandSep(const Value: Int64): string;

{ Convert string representing binary number to an integer.
  String must contain only '0', '1' (digits) and start with an optional sign
  (+ or -).
  @raises EConvertError On problems with conversion. }
function Str2ToInt(const s: string): integer;

{ Convert string with hexadecimal number to an integer.
  String must contain only digits (0-9, a-z, A-Z), and with an optional
  sign (+ or -).
  @raises EConvertError On problems with conversion. }
function StrHexToInt(const s: string): Int64;

function StrToFloatDef(const s: string; DefValue: Extended): Extended;

{ Convert a set to a string representation, in somewhat hacky way.
  This assumes that given SetVariable is a set value, and the set type
  is "set of [NumStart .. NumEnd]".

  @italic(Implementation is heavily dependent on how the sets are internally
  stored.)
  For now, we depend that a set of [NumStart .. NumEnd] behaves like a set
  of Byte, shifted to the left (i.e., NumStart corresponds to a 0 in set of Byte).
  This is not necessarily true ! For example in Delphi 5 (as far as I remember
  --- I don't have this Delphi now, and I don't remember on which Delphi
  version I observed this) set of 1..16 uses first three bytes, and
  the first bit (that would correspond to 0) is simply wasted. In fact,
  SizeOf such set is still 4, which means that internally sets eat 4 bytes anyway.
  But SizeOf set 200..216 is also 4, which means that the compiler is smart
  and doesn't waste too much space to store only 17 bits.

  This all is not a rant on internal set handling by Delphi. On the contrary,
  Delphi does it for speed reasons, and that's very good. This is just
  a warning that SetToStr is not really reliable, and you may need to experiment
  a little with NumStart / NumEnd values to get sensible results.
  Although if your set is like "set of [0 ... something]", this should usually
  work OK,

  Still: @italic(this function should be used only for debug purposes.
  Don't depend on it working 100% correctly always --- it can't, because we
  can't depend on how compiler stores sets.) }
function SetToStr(const SetVariable; NumStart, NumEnd: byte): string;

{ PCharOrNil simply returns a Pointer(S), you can think of it as a NO-OP.
  If string is empty, this returns @nil, otherwise it works just like
  PChar(S): returns a Pointer(S) with appropriate type cast. }
function PCharOrNil(const s: string): PChar;

{ some ASCII funcs / codes -------------------------------------------------- }

const
  { }
  CtrlA = Chr(Ord('a') - Ord('a') + 1); { = #1 } { }
  CtrlB = Chr(Ord('b') - Ord('a') + 1); { = #2 } { }
  CtrlC = Chr(Ord('c') - Ord('a') + 1); { ... etc. } { }
  CtrlD = Chr(Ord('d') - Ord('a') + 1);
  CtrlE = Chr(Ord('e') - Ord('a') + 1);
  CtrlF = Chr(Ord('f') - Ord('a') + 1);
  CtrlG = Chr(Ord('g') - Ord('a') + 1);
  CtrlH = Chr(Ord('h') - Ord('a') + 1); { = CharBackspace } { }
  CtrlI = Chr(Ord('i') - Ord('a') + 1); { = CharTab } { }
  CtrlJ = Chr(Ord('j') - Ord('a') + 1);
  CtrlK = Chr(Ord('k') - Ord('a') + 1);
  CtrlL = Chr(Ord('l') - Ord('a') + 1);
  CtrlM = Chr(Ord('m') - Ord('a') + 1); { = CharEnter } { }
  CtrlN = Chr(Ord('n') - Ord('a') + 1);
  CtrlO = Chr(Ord('o') - Ord('a') + 1);
  CtrlP = Chr(Ord('p') - Ord('a') + 1);
  CtrlQ = Chr(Ord('q') - Ord('a') + 1);
  CtrlR = Chr(Ord('r') - Ord('a') + 1);
  CtrlS = Chr(Ord('s') - Ord('a') + 1);
  CtrlT = Chr(Ord('t') - Ord('a') + 1);
  CtrlU = Chr(Ord('u') - Ord('a') + 1);
  CtrlV = Chr(Ord('v') - Ord('a') + 1);
  CtrlW = Chr(Ord('w') - Ord('a') + 1);
  CtrlX = Chr(Ord('x') - Ord('a') + 1);
  CtrlY = Chr(Ord('y') - Ord('a') + 1);
  CtrlZ = Chr(Ord('z') - Ord('a') + 1); { = #26 } { }

  CharBackSpace = #8;
  CharTab = #9;
  CharEnter = #13;
  CharEscape = #27;
  CharDelete = #127;

{ Return a nice very short description of the character.

  For normal readable characters just returns them, for special
  characters returns short string like "Ctrl+something" or "Escape".

  The returned string doesn't contain any quotes around, doesn't
  contain any word merely stating "character" (for example argument 'c' just
  generates 'c', not 'character "c"').

  BackSpaceTabEnterString determines behavior on three special values:
  #8, #9, #13. These may be either described as Backspace/Tab/Enter
  (if BackSpaceTabEnterString = true)
  or as Ctrl+H, Ctrl+I, Ctrl+M (if BackSpaceTabEnterString = false). }
function CharToNiceStr(c: char; BackSpaceTabEnterString: boolean = true): string;

{ Replace any number of consecutive whitespace (including newlines)
  with a single whitespace. This is nice when you have a string
  (possibly multiline) supplied by user, and you want to use this
  for some UI item (like window's caption or menu item) --- this
  "sanitizes" whitespace inside such string. }
function SCompressWhiteSpace(const S: string): string;

implementation

uses KambiFilesUtils;

function RandomString: string;
var i: integer;
begin
 result := '';
 for i := 1 to random(10) do result := result+char(byte('A')+Random(26));
 for i := 1 to 3 do result := result+char(byte('0')+Random(10));
end;

procedure StringReplaceAllTo1st(var S: string;
  const FromPattern, ToPattern: string;
  IgnoreCase: boolean);
(*
 { NAIWNA IMPLEMENTACJA : zawsze szuka w nowym s od subs_orig od poczatku
   (w rezultacie poczatek stringa przeszukajac wiele razy niepotrzebnie).
   No i moze sie zapetlic gdy subs_repl zawiera w sobie subs_orig. }
var p: integer;
begin
 {assert( Pos(subs_orig, subs_repl) = 0 , 'blad w ReplaceSubstr !');}
 p := Pos(subs_orig, s); (tutaj powinna byc sterowana IgnoreCase) }
 while p > 0 do
 begin
  Delete(s, p, length(subs_Orig));
  Insert(subs_repl, s, p);
  p := Pos(subs_orig, s); (tutaj powinna byc sterowana IgnoreCase)
 end;
*)
begin
 if IgnoreCase then
  s := StringReplace(s, FromPattern, ToPattern, [rfReplaceAll, rfIgnoreCase]) else
  s := StringReplace(s, FromPattern, ToPattern, [rfReplaceAll]);
end;

function BreakLine(const s: string; MaxCol: integer; onbreakChars: TSetOfChars): string;
var done: integer;
    nowcol, i, brk: integer;
label brokenSuccess;
const breakingstr = nl;
begin
 Done := 0;
 Result := '';

 i := 1;
 while i <= Length(s) do
 begin
  if s[i] in [#10, #13] then
  begin
   { niech i obejmie cale zakonczenie linii ktore moze byc 2-znakowe #13#10 lub #10#13 }
   case s[i] of
    #13 : if SCharIs(s, i+1, #10) then Inc(i);
    #10 : if SCharIs(s, i+1, #13) then Inc(i);
   end;
   Result := Result + CopyPos(s, Done+1, i);
   Done := i;
  end else
  begin
   NowCol := i - Done;
   if NowCol > MaxCol then
   begin
    { we got line s[done+1..i] that we have to break somewhere. }
    for brk := i downto Done + 1 do
     if s[brk] in OnBreakChars then
     begin
      Result := Result + CopyPos(s, Done+1, Brk-1) + BreakingStr;
      Done := brk; { we left the rest : s[brk+1..i] to be done }
      goto brokenSuccess;
     end;
    { ups ! it can't be broken - no onbreakChars found ! so we break after
      done+maxcol position. }
    Result := Result + Copy(s, Done+1, MaxCol) + BreakingStr;
    Done := Done + MaxCol;
    brokenSuccess:;
   end;
  end;

  Inc(i);
 end;

 if Done < Length(S) then
  Result := Result + SEnding(S, Done+1);
end;

function SDeleteChars(const s: string; const excludedChars: TSetOfChars): string;
var i, j: integer;
begin
 SetLength(result, length(s));
 j := 1;
 for i := 1 to length(s) do
  if not (s[i] in excludedChars) then
   begin result[j] := s[i]; Inc(j); end;
 SetLength(result, j-1);
end;

function SReplaceChars(const s, FromChars, ToChars: string): string;
var i, p: integer;
begin
 result := s;
 for i := 1 to Length(result) do
 begin
  p := CharPos(result[i], FromChars);
  if p > 0 then result[i] := ToChars[p];
 end;
end;

function SReplaceChars(const s: string; FromChars: TSetOfChars; ToChar: char): string;
var i: integer;
begin
 result := s;
 for i := 1 to Length(result) do
  if result[i] in FromChars then result[i] := ToChar;
end;

function SReplaceChars(const s: string; FromChar, ToChar: char): string;
var i: Integer;
begin
 Result := S;
 for i := 1 to Length(Result) do
  if Result[i] = FromChar then Result[i] := ToChar;
end;

function SPad(const s: string; len: integer; c: char): string;
var lnow: integer;
begin
 lnow := length(s);
 if lnow < len then
  Result := StringOfChar(c, len-lnow) + s else
  Result := s;
end;

function SZeroPad(const s: string; len: integer): string;
begin result := SPad(s, len, '0') end;

function LoCase(c: char): char;
begin
 if c in ['A'..'Z'] then
  result := chr(ord(c)-ord('A')+ord('a')) else
  result := c;
end;

function CharPos(c: char; const s: string; Offset: Integer): integer;
var i: integer;
begin
 for i := Offset to length(s) do
  if s[i] = c then begin result := i; exit end;
 result := 0;
end;

function CharsPos(const chars: TSetOfChars; const s: string): integer;
begin
 for result := 1 to Length(s) do
  if s[result] in chars then exit;
 result := 0;
end;

function CharsPosEx(const Chars: TSetOfChars; const S: string;
  Offset: Integer): integer;
begin
 for Result := Offset to Length(S) do
   if S[Result] in Chars then Exit;
 Result := 0;
end;

function BackCharsPos(const chars: TSetOfChars; const s: string): integer;
begin
 for result := Length(s) downto 1 do
  if s[result] in chars then exit;
 result := 0;
end;

function BackPos(const SubString, S: string): integer;
begin
  for Result := Length(S) - Length(SubString) + 1 downto 1 do
    if SubString = Copy(S, Result, Length(SubString)) then Exit;
  Result := 0;
end;

function BackPos(const SubString: char; const S: string): Integer;
begin
  for Result := Length(S) downto 1 do
    if S[Result] = SubString then Exit;
  Result := 0;
end;

function FirstDelimiter(const Delimiters, S: string): Integer;
begin
 for result := 1 to Length(s) do
  if CharPos(S[result], Delimiters) <> 0 then exit;
 result := 0;
end;

function SEnding(const S: string; P: integer): string;
begin
 result := Copy(S, P, MaxInt)
end;

function IsPrefix(const Prefix, S: string; IgnoreCase: boolean): boolean;
begin
 if IgnoreCase then
  result := AnsiCompareText(Copy(S, 1, Length(Prefix)), Prefix) = 0 else
  result := AnsiCompareStr(Copy(S, 1, Length(Prefix)), Prefix) = 0;
end;

function IsSuffix(const Suffix, S: string; IgnoreCase: boolean): boolean;
begin
 if IgnoreCase then
  result := AnsiCompareText(SRight(S, Length(Suffix)), Suffix) = 0 else
  result := AnsiCompareStr(SRight(S, Length(Suffix)), Suffix) = 0;
end;

function PrefixRemove(const Prefix, S: string; IgnoreCase: boolean): string;
begin
 if IsPrefix(Prefix, S, IgnoreCase) then
  Result := SEnding(S, Length(Prefix) + 1) else
  Result := S;
end;

function SuffixRemove(const Suffix, S: string; IgnoreCase: boolean): string;
begin
 Result := S;
 if IsSuffix(Suffix, S, IgnoreCase) then
 begin
  { doing assignment and SetLength should be a little faster
    than doing Result := Copy(S, 1, ...) }
  SetLength(Result, Length(s) - Length(Suffix));
 end;
end;

procedure SAppendData(var s: string; const Data; DataSize: integer);
var OldLen: integer;
begin
 OldLen := Length(s);
 SetLength(s, OldLen+DataSize);
 Move(Data, SChar(s, OldLen+1)^ , DataSize);
end;

{$Include NoRQCheckBegin.inc}
function SChar(const s: string; CharNum: integer): PChar;
begin Result := @s[CharNum] end;
{$Include NoRQCheckEnd.inc}

function SCharIs(const s: string; index: integer; c: char): boolean;
begin result:=(index <= Length(s)) and (s[index] = c) end;

function SCharIs(const s: string; index: integer; const chars: TSetOfChars): boolean;
begin result:=(index <= Length(s)) and (s[index] in chars) end;

function SReadableForm(const s: string): string;
var i: integer;
begin
 result := '';
 for i := 1 to Length(s) do
  if ( Ord(s[i]) < Ord(' ') ) then
   result := result+'#'+IntToStr(Ord(s[i])) else
   result := result+s[i];
end;

function CopyPos(const s: string; StartPosition, EndPosition: integer): string;
begin
 result := Copy(s, StartPosition, EndPosition - StartPosition + 1);
end;

procedure DeletePos(var S: string; StartPosition, EndPosition: Integer);
begin
 Delete(S, StartPosition, EndPosition - StartPosition + 1);
end;

function NextToken(const S: string; var SeekPos: Integer;
  const TokenDelims: TSetOfChars): string;
var
  TokStart: Integer;
begin
  repeat
    if SeekPos > Length(s) then begin Result := ''; Exit end;
    if S[SeekPos] in TokenDelims then Inc(SeekPos) else Break;
  until false;
  TokStart := SeekPos; { TokStart := first character not in TokenDelims }

  while (SeekPos <= Length(s)) and not(S[SeekPos] in TokenDelims) do Inc(SeekPos);

  { Calculate result := s[TokStart, ... , SeekPos-1] }
  result := Copy(s, TokStart, SeekPos-TokStart);

  { We don't have to do Inc(seekPos) below. But it's obvious that searching
    for next token can skip SeekPos, since we know S[SeekPos] is TokenDelim. }
  Inc(SeekPos);
end;

function NextTokenOnce(const s: string; SeekPos: integer;
  const TokenDelims: TSetOfChars): string;
begin
 result := Nexttoken(S, SeekPos, TokenDelims);
end;

function CreateTokens(const s: string;
  const TokenDelims: TSetOfChars): TDynStringArray;
var SeekPos: Integer;
    Token: string;
begin
 Result := TDynStringArray.Create;
 try
  SeekPos := 1;
  repeat
   Token := NextToken(s, SeekPos, TokenDelims);
   if Token = '' then break;
   Result.Add(Token);
  until false;
 except Result.Free; raise end;
end;

{$WARNINGS OFF}
function NextTokenRestr(const s: string; var SeekPos: integer;
  const TokenDelims: TSetOfChars; const RestrAreas: TSetOfChars): string;
var TokStart: integer;
    InRestr: boolean;     { czy jestesmy w restricted area }
    RestrBeginChar: char; { znak ktory rozpoczal restricted area w ktorym jestesmy }
begin
 repeat
  if SeekPos > Length(s) then begin result := ''; exit end;
  if S[SeekPos] in TokenDelims then Inc(SeekPos) else break;
 until false;
 TokStart := SeekPos; {TokStart := pierwszy znak not in TokenDelims}

 InRestr := false;
 while (SeekPos <= Length(s)) and ( not(S[SeekPos] in TokenDelims) or InRestr) do
 begin
  if InRestr then
  begin
   if S[SeekPos] = RestrBeginChar then InRestr := false;
  end else
  begin
   if S[SeekPos] in RestrAreas then begin InRestr := true; RestrBeginChar := S[SeekPos] end;
  end;

  Inc(SeekPos);
 end;

 result := Copy(s, TokStart, SeekPos-TokStart); {result := s[TokStart, ... , SeekPos-1] }
 Inc(SeekPos); { moglibysmy nie robic tu Inc(seekPos) ale wiadomo ze szukania nastepnego
                 tokenu nie warto zaczynac od SeekPos bo przeciez wiemy ze s[SeekPos] to
                 TokenDelim ! }
end;
{$WARNINGS ON}

function FindPos(const SubText, Text: string; StartPosition, Count: integer; Options: TSearchOptions; const WordBorders: TSetOfChars): integer;
var S, SubS: string;

  function MatchingPos(i: integer): boolean;
  { sprawdz czy i jest dobra Position wystapienia SubS w S.
    Uwzglednij przy tym czy soWholeWord in Options, zachowuj sie zawsze
    jakby bylo soMatchCase in Options. }
  var realI: integer;
  begin
   result := false;
   if Copy(S, i, Length(SubS)) = SubS then
   begin
    if soWholeWord in Options then
    begin
     realI := i+StartPosition-1;
     if ( (realI = 1) or (Text[realI-1] in wordBorders) ) and
        ( (realI+length(subS)-1 = length(Text)) or (Text[realI+length(subS)] in WordBorders) )
     then result := true
    end else result := true;
   end;
  end;

var i: integer;
begin
 S := copy(Text, StartPosition, Count);
 SubS := SubText;
 if not (soMatchCase in Options) then
 begin
  S := AnsiUpperCase(S);
  SubS := AnsiUpperCase(SubS);
 end;
 result := 0;
 if soBackwards in Options then
 begin
  for i := Count-Length(SubS)+1 downto 1 do
   if MatchingPos(i) then begin result := i; break end;
 end else
 begin
  for i := 1 to Count-Length(SubS)+1 do
   if MatchingPos(i) then begin result := i; break end;
 end;
 if result > 0 then result := result+StartPosition-1;
end;

function MatchingFind(const SubText, Text: string; MatchStart, MatchLength: integer; matchCase, wholeWord: boolean; const WordBorders: TSetOfChars): boolean;
var Match: string;
    start, stop: integer;
begin
 result := false;

 { ponizsze sprawdzenie nie jest konieczne ale czesto pozwoli bardzo szybko
   odrzucic nieprawidlowe matching }
 if Length(SubText) <> MatchLength then exit;

 Match := Copy(Text, MatchStart, MatchLength);
 if MatchCase then
 begin
  if not AnsiSameStr(SubText, Match) then exit;
 end else
 begin
  if not AnsiSameText(SubText, Match) then exit;
 end;

 if WholeWord then
 begin
  start := MatchStart-1;
  if (start > 0) and not (Text[start] in WordBorders) then exit;
  stop := MatchStart+MatchLength;
  if (stop <= Length(Text)) and not (Text[stop] in WordBorders) then exit;
 end;

 result := true;
end;

function MatchingFind(const SubText, Text: string; MatchStart, MatchLength: integer;
  Options: TSearchOptions; const WordBorders: TSetOfChars): boolean;
begin
 result := MatchingFind(SubText, Text, MatchStart, MatchLength,
   soMatchCase in Options, soWholeWord in Options, WordBorders);
end;

function FindWordPos(const SubText, Text: string; const WordBorders: TSetOfChars {DefaultWordBorders}): integer;
var i: integer;
begin
 for i := 1 to Length(Text) - Length(SubText) +1 do
  if (Copy(Text, i, Length(SubText)) = SubText) and
     ( (i = 1) or (Text[i-1] in WordBorders) ) and
     ( (i+Length(SubText)-1 = Length(Text)) or (Text[i+Length(SubText)] in WordBorders) )
    then begin result := i; exit end;
 result := 0;
end;

function GetWordAtPos(const Text: string; Position: integer; const WordBorders: TSetOfChars): string;
var pozStart, dlug, TextLen: integer;
begin
 pozStart := Position;
 dlug := 0;
 TextLen := length(Text);
 while (pozStart > 1) and (not (Text[pozStart-1] in wordBorders)) do
  begin Dec(pozStart); Inc(dlug); end;
 while (Position < TextLen) and (not (Text[Position] in wordBorders)) do
  begin Inc(Position); Inc(dlug); end;
 result := copy(Text, pozStart, dlug);
end;

function SRight(const s: string; const rpart: integer): string;
begin
 if Length(s) < rpart then
  result := s else
  result := Copy(s, Length(s)-rpart+1, rpart);
end;

function SAppendPart(const s, PartSeparator, NextPart: string): string;
begin
 if s = '' then
  result := NextPart else
  result := s+PartSeparator+NextPart;
end;

function FileToString(const FileName: string; const AllowStdIn: boolean): string;
var
  F: file;
  S: string;
begin
  if AllowStdIn and (FileName = '-') then
  begin
    Result := '';
    while not Eof(Input) do
    begin
      Readln(S);
      Result += S + NL;
    end;
  end else
  begin
    SafeReset(f, FileName, true);
    try
      SetLength(Result, FileSize(f));
      BlockRead(f, PChar(Result)^, Length(Result));
    finally CloseFile(F) end;
  end;
end;

procedure StringToFile(const FileName, contents: string);
var F: File;
begin
 SafeRewrite(F, FileName);
 try
   BlockWrite(F, PChar(contents)^, Length(contents));
 finally
   CloseFile(F);
 end;
end;

procedure DeFormat(Data: string; const Format: string;
  const args: array of pointer;
  const IgnoreCase: boolean;
  const RelaxedWhitespaceChecking: boolean);
begin
 if TryDeFormat(Data, Format, args, IgnoreCase,
   RelaxedWhitespaceChecking) < High(args)+1 then
  raise EDeformatError.CreateFmt(
    'Unexpected end of Data (%s) - format (%s) not fully evaluated',
    [Data, Format]);
end;

function TryDeFormat(Data: string; const Format: string;
  const args: array of pointer;
  const IgnoreCase: boolean;
  const RelaxedWhitespaceChecking: boolean): integer;
var datapos, formpos: integer;

  function ReadExtendedData: Extended;
  var dataposstart: integer;
  begin
   {pierwszy znak liczby moze byc + lub -. Potem musza byc same cyfry.}
   if not (data[datapos] in ['0'..'9', '+', '-']) then
    raise EDeformatError.CreateFmt('float not found in data ''%s'' on position %d', [data, datapos]);
   dataposstart := datapos;
   Inc(datapos);
   while (datapos <= Length(data)) and (data[datapos] in ['0'..'9','.', 'e','E', '-', '+']) do
    Inc(datapos);
   {ponizsze StrToFloat tez moze spowodowac blad jesli np.
    wyszedl nam string '-' lub '+' lub string z dwoma kropkami}
   result := StrToFloat(CopyPos(data, dataposstart, datapos-1));
  end;

  function ReadIntegerData: Integer;
  var dataposstart: integer;
  begin
   {pierwszy znak integera moze byc + lub -. Potem musza byc same cyfry.}
   if not (data[datapos] in ['0'..'9', '+', '-']) then
    raise EDeformatError.CreateFmt('integer not found in data ''%s'' on position %d', [data, datapos]);
   dataposstart := datapos;
   Inc(datapos);
   while (datapos <= Length(data)) and (data[datapos] in ['0'..'9']) do
    Inc(datapos);
   {ponizszy StrToInt tez moze spowodowac blad jesli np.
    wyszedl nam string '-' lub '+'}
   result := StrToInt(CopyPos(data, dataposstart, datapos-1));
  end;

  function ReadStringData: string;
  var dataposstart: integer;
  begin
   dataposstart := datapos;
   while (datapos <= Length(data)) and
         (not (data[datapos] in WhiteSpaces)) do Inc(datapos);
   result := CopyPos(data, dataposstart, datapos-1);
  end;

  function ReadTypeSpecifier: string;
  {odczytaj type specifier z kropka z format. Przesun formpos}
  var formposstart: integer;
  begin
   formposstart := formpos;
   repeat
    if formpos > Length(format) then
     raise EDeformatError.Create('type specifier incorrect in  format '''+format+'''');
    if format[formpos] = '.' then
     break else
     Inc(formpos);
   until false;
   result := CopyPos(format, formposstart, formpos-1);
   Inc(formpos); { omin kropke '.' w format }
  end;

  procedure CheckBlackChar(formatchar: char);
  var BlackCharsCheck: boolean;
  begin
   if IgnoreCase then
    BlackCharsCheck := SameText(Data[datapos], format[formpos]) else
    BlackCharsCheck := Data[datapos] = format[formpos];
   if not BlackCharsCheck then
    raise EDeformatError.CreateFmt('data (%s) and format (%s) don''t match', [data, format]);
  end;

  procedure CheckFormatNotEnd;
  begin
    if formpos > Length(format) then
      raise EDeformatError.Create('Unexpected end of format : "'+format+'"');
  end;

begin
 datapos := 1;
 formpos := 1;
 result := 0; { no args done yet }

 { Skip whitespace and the beginning of data }
 if RelaxedWhitespaceChecking then
   while SCharIs(Data, DataPos, WhiteSpaces) do Inc(DataPos);

 while formpos <= Length(Format) do
 begin
  {datapos > Length(data) -> means Data has ended but Format not.
   OK, so we can exit, because we are doing only TryDeFormat.
   Real DeFormat should check our result if it wishes to check that we parsed
   whole Format.}
  if datapos > Length(data) then
  begin
    { Actually, if next thing in format is %s, we can parse it too
      (string will just be '') }
    if Format[FormPos] = '%' then
    begin
      Inc(formpos);
      CheckFormatNotEnd;
      if Format[FormPos] = 's' then
      begin
        PString(args[result])^ := ReadStringData;
        Inc(formpos);
        Inc(result);
      end;
    end;
    Exit;
  end;

  {1 or more whitespace in format means 1 or more whitespaces in data}
  if RelaxedWhitespaceChecking and (format[formpos] in WhiteSpaces) then
  begin
   if not SCharIs(Data, datapos, WhiteSpaces) then
    raise EDeformatError.Create('Whitespace not found in data "' + data +
      '" as requested by format "' + format + '"');
   repeat Inc(formpos) until not SCharIs(format, formpos, WhiteSpaces);
   repeat Inc(datapos) until not SCharIs(data, datapos, WhiteSpaces);
  end else

  {%+something means "read this from data", %% means "read %"}
  if format[formpos] = '%' then
  begin
   Inc(formpos);
   CheckFormatNotEnd;
   try
    case format[formpos] of
     '%':begin
          CheckBlackChar('%');
          Inc(formpos);
          Inc(datapos);
         end;
     's':begin
          PString(args[result])^:=ReadStringData;
          Inc(formpos);
          Inc(result);
         end;
     'd':begin
          PInteger(args[result])^:=ReadIntegerData;
          Inc(formpos);
          Inc(result);
         end;
     'f':begin
          PFloat(args[result])^:=ReadExtendedData;
          Inc(formpos);
          Inc(result);
         end;
     '.':begin
          Inc(formpos);
          case ArrayPosStr(ReadTypeSpecifier, ['single', 'double', 'extended']) of
           0: PSingle(args[result])^:=ReadExtendedData;
           1: PDouble(args[result])^:=ReadExtendedData;
           2: PExtended(args[result])^:=ReadExtendedData;
          end;
          Inc(result);
         end;
     else raise EDeformatError.Create('incorrect format specifier after "%" sign : '''+format+'''');
    end;
   except
    on E: EConvertError do raise EDeformatError.Create('convert error - '+E.Message)
   end;
  end else

  begin
   CheckBlackChar(format[formpos]);
   Inc(datapos);
   Inc(formpos);
  end;
 end;

 if RelaxedWhitespaceChecking then
   while SCharIs(Data, DataPos, WhiteSpaces) do Inc(DataPos);

 if datapos <= Length(data) then
  raise EDeformatError.CreateFmt(
    'data ''%s'' too long - unexpected end of format ''%s''', [Data, Format]);
end;

procedure GetFileFilterExts(const FileFilter: string; var Extensions: TDynStringArray);
var p, SeekPos: integer;
    ExtsStr, filemask: string;
begin
 Extensions.SetLength(0);
 ExtsStr := GetFileFilterExtsStr(FileFilter);
 SeekPos := 1;
 repeat
  filemask := NextToken(ExtsStr, SeekPos,[';']);
  if filemask = '' then break;
  p := CharPos('.', filemask);
  if p > 0 then
   Delete(filemask, 1, p-1) else { delete name from filemask }
   filemask := '.'+filemask; { it means there was no name and dot in filemask. So prepend dot. }
  Extensions.Add(filemask);
 until false;
end;

function GetFileFilterName(const FileFilter: string): string;
var ffLeft, ffRight: string;
    p, len: integer;
begin
 p := CharPos('|', FileFilter);
 if p = 0 then result := Trim(FileFilter) else
 begin
  ffLeft := Trim(Copy(FileFilter, 1, p-1));
  ffRight := Trim(SEnding(FileFilter, p+1));
  if ffRight = '' then
  begin
   result := ffLeft;
   { if FileFilter = 'xxx()|' then it matches to pattern 'xxx(exts)|exts'
     so we should return 'xxx', not 'xxx()'.
     This is often really useful when FileFilter was constructed in an
     automatic way (e.g. as in mine edytorek). }
   if IsSuffix('()', Result) then
   begin
    SetLength(Result, Length(Result)-2);
    { trim once again to delete rightmost whitespace (as in 'xxx ()|') }
    Result := TrimRight(Result);
   end;
  end else
  begin
   p := FindPos(ffRight, ffLeft, 1, Length(ffLeft), [soBackwards]);
   if p = 0 then
    p := FindPos(SReplaceChars(ffRight, ';', ','), ffLeft, 1, Length(ffLeft), [soBackwards]);
   if p = 0 then result := ffLeft else
   begin
    len := Length(ffRight);
    {zwieksz len tak zeby objelo biale znaki az do ')'}
    while p+len <= Length(ffLeft) do
    begin
     if ffLeft[p+len] = ')' then
      begin Inc(len); break end else
     if ffLeft[p+len] in WhiteSpaces then
      Inc(len) else
      break;
    end;
    {zmniejsz p tak zeby objelo biale znaki az do '('}
    while p-1 >= 1 do
    begin
     if ffLeft[p-1] = '(' then
      begin Dec(p); Inc(len); break end else
     if ffLeft[p-1] in WhiteSpaces then
      begin Dec(p); Inc(len) end else
      break;
    end;
    {koniec; wypieprz p, len}
    Delete(ffLeft, p, len);
    result := Trim(ffLeft);
   end;
  end;
 end;
end;

function GetFileFilterExtsStr(const FileFilter: string): string;
var p: integer;
begin
 p := CharPos('|', FileFilter);
 if p > 0 then
  result := SEnding(FileFilter, p+1) else
  result := '';
end;

function SReplacePatterns(const s: string;
  const patterns, values: array of string; Options: TSearchOptions): string;
var i, poz, minpoz, minind, sinresult: integer;
begin
 Result := '';

 Assert(High(patterns) = High(values));
 Assert(not (soBackwards in Options));
 sinresult := 0; { ile znakow z s zostalo juz przekopiowanych do result ? (lub, w przypadku
   wystapien pattern, ominietych) }

 repeat
  {licz najwczesniejsza pozycje patterns w pozostalej czesci s}
  minind := -1;
  minpoz := 0;
  for i := 0 to High(patterns) do
  begin
   poz := FindPos(patterns[i], s, sinresult+1, Length(s), Options);
   if (poz > 0) and ((minind = -1) or (poz < minpoz)) then
   begin
    minind := i;
    minpoz := poz;
   end;
  end;
  if minind = -1 then break; { wszystkie poz sa rowne 0, a wiec wszystko zamienione }

  {skopiuj do result wszystko z s przed wystapieniem pattern}
  result := result + CopyPos(s, sinresult+1, minpoz-1);
  sinresult := minpoz-1;
  {omin pattern[] w s, dolacz value[] do result}
  sinresult := sinresult + Length(patterns[minind]);
  result := result + values[minind];
 until false;

 result := result + SEnding(s, sinresult+1);
end;

function SCharsCount(const S: string; C: char): Cardinal;
var i: Integer;
begin
 Result := 0;
 for I := 1 to Length(s) do if S[i] = C then Inc(Result);
end;

function SCharsCount(const s: string; const Chars: TSetOfChars): Cardinal;
var i: Integer;
begin
 Result := 0;
 for I := 1 to Length(s) do if S[i] in Chars then Inc(Result);
end;

function STruncateHash(const s: string): string;
var p: integer;
begin
 p := CharPos('#', s);
 result := s;
 if p > 0 then SetLength(result, p-1);
end;

function SUnformattable(const s: string): string;
begin
 result := StringReplace(s, '%', '%%', [rfReplaceAll]);
end;

function SAnsiCompare(const s1, s2: string; IgnoreCase: boolean): Integer;
begin
 if IgnoreCase then
  result := AnsiCompareText(s1, s2) else
  result := AnsiCompareStr(s1, s2);
end;

function SAnsiSame(const s1, s2: string; IgnoreCase: boolean): boolean;
begin
 result := SAnsiCompare(s1, s2, IgnoreCase) = 0;
end;

function SPercentReplace(const InitialFormat: string;
  const Replaces: array of TPercentReplace;
  out ReplacementsDone: Cardinal;
  ErrorOnUnknownPercentFormat: boolean;
  PercentChar: char;
  IgnoreCase: boolean): string;

  function ReplaceWithC(c: char): Integer;
  var
    I: Integer;
  begin
    if IgnoreCase then
    begin
      for i := 0 to High(Replaces) do
        if AnsiSameText(c, Replaces[i].c) then begin result := i; Exit end;
    end else
    begin
      for i := 0 to High(Replaces) do
        if c = Replaces[i].c then begin result := i; Exit end;
    end;
    result := -1;
  end;

  procedure UnknownPercentFormat(const WrongSequence: string);
  begin
    raise EUnknownPercentFormat.Create('Unknown format pattern in format "'
      +InitialFormat+'", wrong sequence is : ' +WrongSequence);
  end;

var
  P, ReplNum: Integer;
  Format: string;
begin
  { Result zawiera czesciowy wynik. Od Format bedziemy odcinac zrobione juz kawalki.
    Bedziemy caly czas doklejac kolejne wyniki do Result (bedziemy starali sie,
    dla szybkosci, doklejac mozliwie duze kawalki do Result na raz, np. nie chcemy
    przepisywac do Result po jednym znaku). }
  Result := '';
  Format := InitialFormat;
  ReplacementsDone := 0;

  while Format <> '' do
  begin
    P := Pos(PercentChar, Format);
    if P = 0 then begin Result := Result + Format; Exit end;

    Result := Result + Copy(Format, 1, P - 1);
    if P + 1 <= Length(Format) then
    begin
      { zwieksz Result o element wynikajacy z formatu Format[p+1] }
      if Format[P + 1] = PercentChar then
        Result := Result + PercentChar else
      begin
        ReplNum := ReplaceWithC(Format[P + 1]);
        if ReplNum = -1 then
        begin
          if ErrorOnUnknownPercentFormat then
            UnknownPercentFormat('"'+PercentChar+Format[P + 1]+'"');
          Result := Result + PercentChar + Format[P + 1];
        end else
        begin
          Result := Result + Replaces[ReplNum].s;
          Inc(ReplacementsDone);
        end;
      end;
      { obetnij wykonana czesc z Format }
      Delete(Format, 1, P + 1);
    end else
    begin
      { mamy PercentChar na koncu stringa }
      if ErrorOnUnknownPercentFormat then
       UnknownPercentFormat(PercentChar+' at the end of the format string');
      Result := Result + PercentChar;
      Exit;
    end;
  end;
end;

function SPercentReplace(const InitialFormat: string;
  const Replaces: array of TPercentReplace;
  ErrorOnUnknownPercentFormat: boolean;
  PercentChar: char;
  IgnoreCase: boolean): string;
var
  ReplacementsDone: Cardinal;
begin
  Result := SPercentReplace(InitialFormat, Replaces, ReplacementsDone,
    ErrorOnUnknownPercentFormat, PercentChar, IgnoreCase);
  { returned ReplacementsDone will simply be ignored }
end;

function FormatIndexedName(const NamePattern: string;
  const Index: Integer; out ReplacementsDone: Cardinal): string;
const
  PercentChar = '%';
var
  StartP, P, MinLength: Integer;
  Format: string;
begin
  { Result zawiera czesciowy wynik. Od Format bedziemy odcinac zrobione juz kawalki.
    Bedziemy caly czas doklejac kolejne wyniki do Result (bedziemy starali sie,
    dla szybkosci, doklejac mozliwie duze kawalki do Result na raz, np. nie chcemy
    przepisywac do Result po jednym znaku). }
  Result := '';
  Format := NamePattern;
  ReplacementsDone := 0;

  while Format <> '' do
  begin
    P := Pos(PercentChar, Format);
    if P = 0 then begin Result := Result + Format; Exit end;

    Result := Result + Copy(Format, 1, P - 1);
    if P + 1 <= Length(Format) then
    begin
      { zwieksz Result o element wynikajacy z formatu Format[P + 1] }
      if Format[P + 1] = PercentChar then
        Result := Result + PercentChar else
      if Format[P + 1] = 'd' then
      begin
        Result := Result + IntToStr(Index);
        Inc(ReplacementsDone);
      end else
      if Format[P + 1] in ['0'..'9'] then
      begin
        Inc(P);
        StartP := P;
        while SCharIs(Format, P, ['0'..'9']) do Inc(P);
        if SCharIs(Format, P, 'd') then
        begin
          { valid % + number + d sequence, do the replace }
          MinLength := StrToInt(Copy(Format, StartP, P - StartP));
          Result := Result + IntToStrZPad(Index, MinLength);
          Inc(ReplacementsDone);
        end else
        begin
          { invalid %-pattern, just copy it (including leading PercentChar
            and following character <> 'd') }
          Result := Result + Copy(Format, StartP - 1, P - StartP + 2);
        end;
        { decrement P just so that Delete(Format, ...) below will work Ok }
        Dec(P);
      end else
      begin
        { unknown %-pattern, just copy it }
        Result := Result + PercentChar + Format[P + 1];
      end;
      { obetnij wykonana czesc z Format }
      Delete(Format, 1, P + 1);
    end else
    begin
      { mamy PercentChar na koncu stringa }
      Result := Result + PercentChar;
      Exit;
    end;
  end;
end;

function FormatIndexedName(const NamePattern: string;
  const Index: Integer): string;
var
  ReplacementsDone: Cardinal;
begin
  Result := FormatIndexedName(NamePattern, Index, ReplacementsDone);
  { simple ignore ReplacementsDone value }
end;

function AnsiUpperCaseChar(C: char): char;
begin
 Result :=
   {$ifdef MSWINDOWS} Chr( PtrUInt( Windows.CharUpper(
     Windows.LPSTR(PtrUInt(Ord(C))) ) ) )
   {$else} AnsiUpperCase(C)[1]
   {$endif};
end;

function AnsiLowerCaseChar(C: char): char;
begin
 Result :=
   {$ifdef MSWINDOWS} Chr( PtrUInt( Windows.CharLower(
     Windows.LPSTR(PtrUInt(Ord(C))) ) ) )
   {$else} AnsiLowerCase(C)[1]
   {$endif};
end;

function SAnsiUpperFirstChar(const S: string): string;
begin
 Result := S;
 if Result <> '' then
  Result[1] := AnsiUpperCaseChar(Result[1]);
end;

{ convertions ------------------------------------------------------------ }

function DigitAsChar(b: byte): char;
begin Result := char(b+byte('0')) end;

function DigitAsByte(c: char): byte;
begin Result := byte(c)-byte('0') end;

function IntToStrZPad(n: integer; minLength: integer): string;
begin result := SZeroPad(IntToStr(n), minLength) end;

function IntToStrBase(n: QWord; Base: Byte): string;

  function TablZnakow(cyfra: Byte): char;
  { result := symbol cyfry 'cyfra'. Zawsze cyfra < Base }
  begin
   if cyfra < 10 then
    result := DigitAsChar(cyfra) else
    result := Chr( cyfra-10+Ord('A') ); {'A'=10 , 'B'=11 itd.}
  end;

begin
 {Nasze symbole to 0..9, 'A' ..'Z'. Mamy wiec 10+'Z'-'A'+1 symboli na Base cyfr. }
 Assert(Base < 10+Ord('Z')-Ord('A')+1, 'too large Base in IntToStrBase');
 if n = 0 then result := '0' else
 begin
  result := '';
  while n <> 0 do
  begin
   result := TablZnakow(n mod Base)+result;
   n := n div Base;
  end;
 end;
end;

function IntToStrBase(const n: Int64; Base: Byte): string;
begin
  if N < 0 then
    Result := '-' + IntToStrBase(QWord(Abs(N)), Base) else
    Result := IntToStrBase(QWord(N), Base);
end;

function IntToStrBase(const n: Int64; Base: Byte; minLength: Cardinal): string;
{wywoluje IntToStrBase, dodatkowo wypelniajac zerami z lewej, jesli trzeba}
begin
 result := IntToStrBase(n, Base);
 if n < 0 then
  result := '-'+SZeroPad(SEnding(result, 2), minLength) else
  result := SZeroPad(result, minLength);
end;

function IntToStrBase(const n: QWord; Base: Byte; minLength: Cardinal): string;
{wywoluje IntToStrBase, dodatkowo wypelniajac zerami z lewej, jesli trzeba}
begin
 result := IntToStrBase(n, Base);
 result := SZeroPad(result, minLength);
end;

function IntToStr2(n: Int64;
  const MinLength: Cardinal;
  const ZeroDigit: char;
  const OneDigit: char;
  const MinusSign: char): string;
var Negative: boolean;
    i: Integer;
begin
 { Simple implementation : Result := IntToStrBase(n, 2, minLength) }

 { Negative := n < 0, n := Abs(n) }
 Negative := n < 0;
 if Negative then n := -n;

 Result := '';

 { from 0 .. SizeOf(n)*8-1 we have SizeOf(n)*8 values,
   all possible bits positions. So we're taking SizeOf(n)*8-2,
   to avoid most significant bit, the sign bit. }
 for i := SizeOf(n)*8-2 downto 0 do
  if ((Int64(1) shl i) and n) <> 0 then
   Result := Result + OneDigit else
  if Result <> '' then
   Result := Result + ZeroDigit;

 if Result = '' then Result := ZeroDigit;

 Result := SPad(Result, MinLength, ZeroDigit);

 if Negative then Result := MinusSign + Result;
end;

function IntToStr16(const n: Int64; const minLength: Cardinal): string;
begin result := IntToStrBase(n, 16, minLength) end;

function IntToStr16(const n: QWord; const minLength: Cardinal): string;
begin result := IntToStrBase(n, 16, minLength) end;

function IntToStrThousandSep(const Value: Int64): string;

  { Inserts ThousandSeparator to Result, where Result must be a sequence
    of digits (no '-' sign allowed !) }
  procedure InsertThousandSep;
  var i, SeparatorsCount, ResultPos, SeparatorPos: Integer;
      NewResult: string;
  begin
   if ThousandSeparator <> #0 then
   begin
    SeparatorsCount := (Length(Result)-1) div 3;

    { We already know the length of NewResult, so we set it now,
      this may we avoid many ReallocMems if length of NewResult would
      be changing. }
    SetLength(NewResult, Length(Result) + SeparatorsCount);

    { calculate initial SeparatorPos }
    SeparatorPos := Length(Result) mod 3;
    if SeparatorPos = 0 then SeparatorPos := 3;
    Inc(SeparatorPos);

    { calculate initial ResultPos }
    ResultPos := SeparatorPos;

    Move(Result[1], NewResult[1], SeparatorPos-1);

    for i := 1 to SeparatorsCount do
    begin
     NewResult[SeparatorPos] := ThousandSeparator;
     Move(Result[ResultPos], NewResult[SeparatorPos+1], 3);
     SeparatorPos := SeparatorPos + 4;
     ResultPos := ResultPos + 3;
    end;

    Result := NewResult;
   end;
  end;

begin
 if Value < 0 then
 begin
  Result := IntToStr(-Value);
  InsertThousandSep;
  Result := '-' + Result;
 end else
 begin
  Result := IntToStr(Value);
  InsertThousandSep;
 end;
end;

function Str2ToInt(const s: string): integer;
  function BinInt(c: char): integer;
  begin
   case c of
    '0': result := 0;
    '1': result := 1;
    else raise EConvertError.Create('Nieprawidlowy argument dla StrBinToInt : '+s);
   end;
  end;

var NextChar: integer;
begin
 if s = '' then
  raise EConvertError.Create('Argument StrBinToInt ma zerowa length.');
 if s[1] = '-' then
 begin
  if Length(s) = 1 then
   raise EConvertError.Create('StrBinToInt cannot convert ''-'' to int.');
  result := -BinInt(s[2]);
  NextChar := 3;
 end else
 begin
  result := BinInt(s[1]);
  NextChar := 2;
 end;
 while NextChar <= Length(s) do
 begin
  result := result*2+binInt(s[NextChar]);
  Inc(NextChar);
 end;
end;

function StrHexToInt(const s: string): Int64;
var ScanStart: integer;

  procedure Scan;
  var digit: Int64;
      i: integer;
  begin
   if ScanStart > Length(s) then
    raise EConvertError.Create('Unexpected end of string : no digits');
   result := 0;
   for i := ScanStart to Length(s) do
   begin
    case s[i] of
     '0'..'9':digit := Ord(s[i])-Ord('0');
     'a'..'f':digit := Ord(s[i])-Ord('a')+10;
     'A'..'F':digit := Ord(s[i])-Ord('A')+10;
     else raise EConvertError.Create('Character "'+s[i]+
       '" is not a hexadecimal digit');
    end;
    result := result*16 + digit;
   end;
  end;

begin
 if SCharIs(s, 1, '-') then
 begin
  ScanStart := 2;
  Scan;
  Result := -Result;
 end else
 begin
  if SCharIs(s, 1, '+') then ScanStart := 2 else ScanStart := 1;
  Scan;
 end;
end;

function ToStr(const Args: array of const): string;
var i: Integer;
begin
 Result := '';
 for i := 0 to High(Args) do
  Result := Result + VarRecToStr(Args[i]);
end;

function VarRecToStr(const v: TVarRec): string;
begin
 with v do
 case VType of
   vtInteger:    Result := IntToStr(VInteger);
   vtBoolean:    Result := BoolToStr[VBoolean];
   vtChar:       Result := VChar;
   vtExtended:   Result := FloatToStr(VExtended^);
   vtString:     Result := VString^;
   vtPChar:      Result := VPChar;
   vtWideChar:   Result := WideCharToString(@vWideChar);
   vtPWideChar:  Result := WideCharToString(vPWideChar);
   vtAnsiString: Result := AnsiString(vAnsiString);
   vtCurrency:   Result := CurrToStr(VCurrency^);
   vtVariant:    Result := VarToStr(VVariant^);
   vtPointer:    Result := 'Pointer('+IntToStr(PtrUInt(vPointer))+')';
   vtObject:     Result := 'Object('+VObject.ClassName+')';
   vtClass:      Result := 'ClassRef('+VClass.ClassName+')';
   else raise Exception.CreateFmt('Wrong argument for VarRecToStr (v.vType = %d)', [vType]);
 end;
end;

function PointerToStr(Ptr: Pointer): string;
begin
  Result := '0x' + IntToStr16(PtrUInt(Ptr),
    {$ifdef CPU32} 8 {$endif}
    {$ifdef CPU64} 16 {$endif} );
end;

function SetToStr(const SetVariable; NumStart, NumEnd: byte): string;
var BSet: set of byte absolute SetVariable;
    i: byte;
begin
 result := '[';
 for i := 0 to NumEnd-NumStart do
  if i in BSet then
   if result = '[' then
    result := '['+IntToStr(i+NumStart) else
    result := result+','+IntToStr(i+NumStart);
 result := result+']';
end;

function StrToFloatDef(const s: string; DefValue: Extended): Extended;
begin
 try
  result := StrToFloat(s);
 except
  on EConvertError do result := DefValue
 end;
end;

function PCharOrNil(const s: string): PChar;
begin if s = '' then result := nil else result := PChar(s); end;

function CharToNiceStr(c: char; BackSpaceTabEnterString: boolean): string;

  function DescribeCtrlKey(c: char): string;
  begin result := 'Ctrl+'+Chr(Ord(c)-1+Ord('a')) end;

begin
  if BackSpaceTabEnterString then
  begin
    case C of
      CharBackSpace: begin Result := 'BackSpace'; Exit; end;
      CharTab      : begin Result := 'Tab'      ; Exit; end;
      CharEnter    : begin Result := 'Enter'    ; Exit; end;
    end;
  end;

  case c of
    #0: Result := '#0';
    CharEscape: Result := 'Esc';
    ' ' : Result := 'Space';
    CtrlA..CtrlZ: Result := DescribeCtrlKey(c);
    else
      Result := c;
  end;
end;

function SCompressWhiteSpace(const S: string): string;
var
  ResultPos: Integer; { this is always next free result position }
  SPos: Integer; { this is always next unhandled S position }
  NextSPos: Integer;
begin
  ResultPos := 1;
  SPos := 1;
  SetLength(Result, Length(S)); { resulting string is at most as long as S }

  if SCharIs(S, 1, WhiteSpaces) then
  begin
    Result[1] := ' ';
    Inc(ResultPos);
    while SCharIs(S, SPos, WhiteSpaces) do Inc(SPos);
  end;

  while SPos <= Length(S) do
  begin
    Assert(not (S[SPos] in WhiteSpaces));

    { read next non-white-space chunk }

    NextSPos := SPos + 1;
    while (NextSPos <= Length(S)) and
          not (S[NextSPos] in WhiteSpaces) do
      Inc(NextSPos);

    Move(S[SPos], Result[ResultPos], NextSPos - SPos);

    ResultPos += NextSPos - SPos;
    SPos := NextSPos;

    { omit next white-space chunk }

    if SCharIs(S, SPos, WhiteSpaces) then
    begin
      Result[ResultPos] := ' ';
      Inc(ResultPos);
      while SCharIs(S, SPos, WhiteSpaces) do Inc(SPos);
    end;
  end;

  { assert we didn't do buffer overflow just now }
  Assert(ResultPos - 1 <= Length(Result));

  SetLength(Result, ResultPos - 1);
end;

end.
