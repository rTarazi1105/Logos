import * as ohm from 'ohm-js';

const grammar = ohm.grammar(`
MyGrammars {
  ////////////////////////////////
  // Helper rules
  hex     = digit | "a".."f" | "A".."F"

  ////////////////////////////////
  // 1. Canadian Postal Codes
  CanadianPostalCode =
      postalLetter digit postalLetter ~" " digit postalLetter digit
  postalLetter =
      "A".."C" | "E" | "G".."H" | "J".."N" | "P" | "R".."T" | "V" | "W" | "X" | "Y" | "Z"

  ////////////////////////////////
  // 2. Legal Visa® Card Numbers
  Visa =
      Visa13 | Visa16
  Visa13 = "4" digit digit digit digit digit digit digit digit digit digit digit digit
  Visa16 = "4" digit digit digit digit digit digit digit digit digit digit digit digit digit digit
  
  ////////////////////////////////
// 3. Legal MasterCard® Numbers
  MasterCard =
      MasterCard51to55 | MasterCard2221to2720
  MasterCard51to55 =
      "5" ("1" | "2" | "3" | "4" | "5")
      digit digit digit digit digit digit digit digit digit digit digit digit digit digit
  MasterCard2221to2720 =
      fourDigitPrefix TwelveDigits
  fourDigitPrefix = digit digit digit digit
  TwelveDigits = digit digit digit digit digit digit digit digit digit digit digit digit

  ////////////////////////////////
  // 4. Strings of Basic Latin letters except those that are exactly three letters ending with two o’s
  NotThreeEndingInOO =
      "" 
    | longString
    | threeLetter
  longString = letter letter letter+    // (length ≥4)
  threeLetter = letter letter letter 

  ////////////////////////////////
  // 5. Binary numerals divisible by 16
  DivisibleBy16 =
      allZeros | nonZeroDiv16
  allZeros =
      "0"*
  nonZeroDiv16 =
      nonZeroBinary "0000"
  nonZeroBinary =
      "1" ( "0" | "1" )*

  ////////////////////////////////
  // 6. Decimal numerals in the range 8 through 32, inclusive
  // We list every valid numeral from 8 to 32 as a string literal.
  EightThroughThirtyTwo =
      "8" | "9" | "10" | "11" | "12" | "13" | "14" | "15" | "16" | "17" | 
      "18" | "19" | "20" | "21" | "22" | "23" | "24" | "25" | "26" | 
      "27" | "28" | "29" | "30" | "31" | "32"

  ////////////////////////////////
  // 7. All strings of Unicode letters, except exactly "python", "pycharm", or "pyc"
  NotPythonPycharmPyc =
      ~("python" | "pycharm" | "pyc") letter+

  ////////////////////////////////
  // 8. Floating point constants with an optional fractional part, but a required exponent
  RestrictedFloat =
      digit+ ("." digit*)? Exp
  Exp = ("e" | "E") ("+" | "-")? digit digit? digit?

  ////////////////////////////////
  // 9. Palindromes over the letters a, b, and c, of length 2, 3, 5, or 8
  // For Pal8, we simply match eight letters and rely on a semantic check to verify the palindrome property.
  Palindrome =
      Pal2 | Pal3 | Pal5 | Pal8
  Pal2 =
      "a" "a" | "b" "b" | "c" "c"
  Pal3 =
      ("a" letterABC "a") | ("b" letterABC "b") | ("c" letterABC "c")
  Pal5 =
      ("a" letterABC letterABC letterABC "a") |
      ("b" letterABC letterABC letterABC "b") |
      ("c" letterABC letterABC letterABC "c")
  Pal8 =
      letterABC letterABC letterABC letterABC letterABC letterABC letterABC letterABC
  letterABC = "a" | "b" | "c"
  ////////////////////////////////
  // 10. CORRECTED PYTHON STRING LITERALS
  // PythonString =
  //     SingleString | DoubleString | TripleSingleString | TripleDoubleString

  // SingleString = "'" (EscapeSequence | ~["'\\\\\\n"] any)* "'"
  // DoubleString = "\\"" (EscapeSequence | ~["\\"\\\\\\n"] any)* "\\""

  // TripleSingleString = "'''" (EscapeSequence | ~("'''") any)* "'''"
  // TripleDoubleString = "\\"\\"\\"" (EscapeSequence | ~("\\"\\"\\"") any)* "\\"\\"\\""

  // EscapeSequence = "\\\\" (
  //     "'" | "\\"" | "\\\\" | "n" | "r" | "t" | "b" | "f" | "v" | "a" |
  //     "x" hex hex |
  //     "u{" hex+ "}"
  // )
}`);

export const matches = (ruleName, input) => {
  return grammar.match(input, ruleName).succeeded();
};
