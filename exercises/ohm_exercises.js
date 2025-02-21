import { describe, it } from "node:test"
import assert from "assert"
import * as ohm from "ohm-js"

const grammars = {
  canadianPostalCode: String.raw`
    code = firstletter digit nonfirstletter " " digit nonfirstletter digit
    nonfirstletter = "A".."C" | "E" | "G".."H" | "J".."N" | "P" | "R".."T" | "V".."Z"
    firstletter = nonfirstletter | "D" | "W"
  `,

  visa: String.raw`
    visa = "4" digit12or15
    digit12or15 = d d d d d d d d d d d d (d d d)?
    d = digit
  `,

  masterCard: String.raw`
    masterCard = ("51" | "52" | "53" | "54" | "55") digit14 
                  | "2221" digit12 
                  | "22" digit14
                  | "24" digit14
                  | "25" digit14
                  | "26" digit14
                  | "270" digit13       
                  | "271" digit13       
                  | "2720" digit12          

    digit12 = d d d d d d d d d d d d
    digit13 = d d d d d d d d d d d d d
    digit14 = d d d d d d d d d d d d d d
    d = digit
  `,


//   notThreeEndingInOO: String.raw`
//     write your grammar here
//   `,

//   divisibleBy16: String.raw`
//     write your grammar here
//   `,

  eightThroughThirtyTwo: String.raw`
    main = "8" | "9" 
        | "10" | "11" | "12" | "13" | "14" | "15" | "16" | "17" | "18" | "19"
        | "20" | "21" | "22" | "23" | "24" | "25" | "26" | "27" | "28" | "29"
        | "30" | "31" | "32"
  `,


  notPythonPycharmPyc: String.raw`
    notPythonPycharmPyc = ~( "python" end | "pycharm" end | "pyc" end ) letter+ | ""
  `,

  restrictedFloats: String.raw`
    restrictedFloat = digit+ ("." digit*)? exp
    exp = ("e" | "E") ("+" | "-")? digit digit? digit?
  `,

  palindromes2358: String.raw`
palindromes2358 = pal8 | pal5 | pal3 | pal2

    // 2-letter palindromes: only "aa", "bb", or "cc"
    pal2 = "aa" | "bb" | "cc"

    // 3-letter palindromes: x letter x, with letter any of a,b,c.
    pal3 = "a" validLetter "a"
          | "b" validLetter "b"
          | "c" validLetter "c"

    // 5-letter palindromes: outer letters must match and the inner 3 letters form a palindrome.
    pal5 = "a" pal3 "a"
          | "b" pal3 "b"
          | "c" pal3 "c"

    // 8-letter palindromes: outer letter + 6-letter even palindrome + same outer letter.
    pal8 = "a" even6 "a"
          | "b" even6 "b"
          | "c" even6 "c"

    // even6: 6-letter palindrome is built as letter + 4-letter even palindrome + same letter.
    even6 = "a" even4 "a"
          | "b" even4 "b"
          | "c" even4 "c"

    // even4: 4-letter palindrome is letter + 2-letter palindrome + same letter.
    even4 = "a" pal2 "a"
          | "b" pal2 "b"
          | "c" pal2 "c"

    // the allowed letters
    validLetter = "a" | "b" | "c"
  `,


  pythonStringLiterals: String.raw`
    // A valid Python string literal, with an optional f-prefix.
    StringLiteral = fPrefix? ( tripleDoubleString | tripleSingleString | doubleString | singleString )
    fPrefix = "f"

    // Simple quoted strings (allow empty body)
    singleString = "'" singleBody "'" 
    doubleString = "\"" doubleBody "\"" 

    // Triple-quoted strings must have at least one character in the body.
    tripleSingleString = "'''" tripleSingleBody "'''"
    tripleDoubleString = "\"\"\"" tripleDoubleBody "\"\"\""

    // The bodies allow escapes or any non-delimiter, non-backslash character.
    singleBody = ( escapedChar | nonSingle )*
    doubleBody = ( escapedChar | nonDouble )*
    tripleSingleBody = ( escapedChar | (~"'''") any )+
    tripleDoubleBody = ( escapedChar | (~"\"\"\"") any )+

    // An escape is a backslash followed by any character.
    escapedChar = "\\" any

    // In a single-quoted string, any character except a single quote or backslash.
    nonSingle = ~("'" | "\\") any
    // In a double-quoted string, any character except a double quote or backslash.
    nonDouble = ~( "\"" | "\\" ) any
    // In a triple-single-quoted string, the sequence ''' or a backslash must not appear.
    nonTripleSingle = ~( "'''" | "\\" ) any
    // In a triple-double-quoted string, the sequence """ or a backslash must not appear.
    nonTripleDouble = ~( "\"\"\"" | "\\" ) any
                                 
  `,
}

function matches(name, string) {
  const grammar = `G {${grammars[name]}}`
  return ohm.grammar(grammar).match(string).succeeded()
}

const testFixture = {
  // canadianPostalCode: {
  //   good: ["A7X 2P8", "P8E 4R2", "K1V 9P2", "Y3J 5C0"],
  //   bad: [
  //     "A7X   9B2",
  //     "C7E 9U2",
  //     "",
  //     "Dog",
  //     "K1V\t9P2",
  //     " A7X 2P8",
  //     "A7X 2P8 ",
  //   ],
  // },
  // visa: {
  //   good: ["4128976567772613", "4089655522138888", "4098562516243"],
  //   bad: [
  //     "43333",
  //     "42346238746283746823",
  //     "7687777777263211",
  //     "foo",
  //     "π",
  //     "4128976567772613 ",
  //   ],
  // },
  // masterCard: {
  //   good: [
  //     "5100000000000000",
  //     "5294837679998888",
  //     "5309888182838282",
  //     "5599999999999999",
  //     "2221000000000000",
  //     "2720999999999999",
  //     "2578930481258783",
  //     "2230000000000000",
  //   ],
  //   bad: [
  //     "5763777373890002",
  //     "513988843211541",
  //     "51398884321108541",
  //     "",
  //     "OH",
  //     "5432333xxxxxxxxx",
  //   ],
  // },
//   notThreeEndingInOO: {
//     good: ["", "fog", "Tho", "one", "a", "ab", "food"],
//     bad: ["fOo", "gOO", "HoO", "zoo", "MOO", "123", "A15"],
//   },
//   divisibleBy16: {
//     good: [
//       "0",
//       "00",
//       "000",
//       "00000",
//       "00000",
//       "000000",
//       "00000000",
//       "1101000000",
//     ],
//     bad: ["1", "00000000100", "1000000001", "dog0000000"],
//   },
// eightThroughThirtyTwo: {
//   good: Array(25)
//     .fill(0)
//     .map((x, i) => i + 8),
//   bad: ["1", "0", "00003", "dog", "", "361", "90", "7", "-11"],
// },
//   notPythonPycharmPyc: {
//     good: [
//       "",
//       "pythons",
//       "pycs",
//       "PYC",
//       "apycharm",
//       "zpyc",
//       "dog",
//       "pythonpyc",
//     ],
//     bad: ["python", "pycharm", "pyc"],
//   },
//   restrictedFloats: {
//     good: ["1e0", "235e9", "1.0e1", "1.0e+122", "55e20"],
//     bad: ["3.5E9999", "2.355e-9991", "1e2210"],
//   },
  palindromes2358: {
    good: [
      "aa",
      "bb",
      "cc",
      "aaa",
      "aba",
      "aca",
      "bab",
      "bbb",
      "ababa",
      "abcba",
      "aaaaaaaa",
      "abaaaaba",
      "cbcbbcbc",
      "caaaaaac",
    ],
    bad: ["", "a", "ab", "abc", "abbbb", "cbcbcbcb"],
  },
//   pythonStringLiterals: {
//     good: String.raw`''
//       ""
//       'hello'
//       "world"
//       'a\'b'
//       "a\"b"
//       '\n'
//       "a\tb"
//       f'\u'
//       """abc"""
//       '''a''"''"'''
//       """abc\xdef"""
//       '''abc\$def'''
//       '''abc\''''`
//       .split("\n")
//       .map((s) => s.trim()),
//     bad: String.raw`
//       'hello"
//       "world'
//       'a'b'
//       "a"b"
//       'a''
//       "x""
//       """"""""
//       frr"abc"
//       'a\'
//       '''abc''''
//       """`
//       .split("\n")
//       .map((s) => s.trim()),
//   },
}

for (let name of Object.keys(testFixture)) {
  describe(`when matching ${name}`, () => {
    for (let s of testFixture[name].good) {
      it(`accepts ${s}`, () => {
        assert.ok(matches(name, s))
      })
    }
    for (let s of testFixture[name].bad) {
      it(`rejects ${s}`, () => {
        assert.ok(!matches(name, s))
      })
    }
  })
}
