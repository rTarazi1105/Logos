import { describe, it } from "node:test";
import assert from "node:assert/strict";
import parse from "../src/parser.js";
// https://cs.lmu.edu/~ray/notes/howtowriteacompiler/
// Programs expected to be syntactically correct
const syntaxChecks = [
  ["simplest syntactically correct program", "yes : true;"],
  ["multiple statements", "yes : true; no : false;"],
  ["value declaration", "value n0;"],
  ["relation", "is_zero : x : x = n0;"],
  ["statement", "zero_is_zero_but : xnor[is_zero[n0], a];"],
  ["assumption", "assume true : is_zero[n0];"],
  ["named assumption", "assume def_zero true : is_zero[n0];"],
  ["operation", "operation xnor : a,b : (a and b) or (not a and not b);"],
  ["infix", "infix b both;"],
  ["property", "property injection : f<2> : (f[x,z] and f[y,z]) ifthen (x = y);"],
  ["calling a property", "assume true : injection[succession];"],
  ["chained ors in data", "this_statement : a or b or c;"],
  ["chained ands in data", "operation all : a, b, c : a and b and c;"],
  ["chained ors in data 2", "operation all : a, b, c : a or b or c;"],
  [
    "struct declaration",
    "struct Coordinate {x: Numeral, y:int, type:CoordType}",
  ],
  [
    "class declaration",
    "class Summable { mod sum(self) -> int; }",
  ],
  [
    "enum declaration",
    "enum SomeCoord {Polar, c: Cartesian, other:CoordClass}",
  ],
  ["module with no params, no return type", "mod m() {}"],
  ["module with one param", "mod m(x: int) {}"],
  ["module with two params", "mod m(x: int, y: bool) {}"],
  ["module with no params + return type", "mod m() {}"],
  ["return function", "mod m(g: property)-> relation<2> {}"],
  ["array type for param", "mod m(x: [[[bool]]]) {}"],
  ["array type returned", "mod m() -> [[int]] {}"],
  [
    "type parameter and optional",
    "mod wrap(item: T) -> Option { return Option.new(item); } ",
  ],
  ["assignments", "mod m() { a -- ; c ++ ; abc=3; a=1; }"],
  [
    "complex var assignment",
    "mod m() { j = mut c(5); j = 100; k = mut c.p.r; k=1; l = mut c.q(8).f(1,1).z; l=1; }",
  ],
  ["complex var bumps", "mod m() { c(5).2++;c.p.r++;c.q(8).a(1,1).z--; }"],
  ["actions", "mod m() { x = 1;\nf(100);\nprint(1); }"],
  ["short if", "mod m() { if true then print(1); }"],
  ["longer if", "mod m() { if !true then { print(1); x = 2; } else x = 3; }"],
  [
    "even longer if",
    "mod m() { if 0 > 1 then print(1) else if false then {print(1); return 1; } else print(0); }",
  ],
  ["while with empty block", "mod m() { while true do {}; }"],
  ["while with one statement block", "mod m() { while true do { x = 1; }; }"],
  [
    "returns",
    "mod m() { x = for i in c do { if f(i) then {j ++; return j;} else return g(i); }; return x; }",
  ],
  [
    "returning up",
    "mod m(a: A, b: B) -> B { sum = 0; last = for i in [0, 1, 2] do { sum++; if i == 1 then return i;}; if sum != 3 then return b else { if last == 1 then return b.inverse(); }; }",
  ],
  [
    "chained ors",
    'mod m(a: bool, b: bool, c: bool) { if a | b | c then print("ok"); }',
  ],
  ["chained ands", 'mod m() { if a & b & c then print("ko"); }'],
  ["string literal", 'mod m() { s = "Hello \\n world"; }'],
];
// LINE 64
// Programs with syntax errors that the parser will detect
const syntaxErrors = [
  ["non-letter in an identifier", "mod m() { ab😭c = 2; }", /Line 1, col 13:/],
  [
    "starting with a number in a mod",
    "mod m() { 1abc = 2; }",
    /Line 1, col 11:/,
  ],
  [
    "unterminated string literal",
    'mod m() { s = "Hello world; }',
    /Line 1, col \d+:/,
  ],
  ["invalid operator", "mod m() { a ^^ 2; }", /Line 1, col \d+:/],
];
// LINE 101 = 64 + 37
describe("The parser", () => {
  for (const [scenario, source] of syntaxChecks) {
    it(`matches ${scenario}`, () => {
      assert(parse(source).succeeded());
    });
  }
  for (const [scenario, source, errorMessagePattern] of syntaxErrors) {
    it(`throws on ${scenario}`, () => {
      assert.throws(() => parse(source), errorMessagePattern);
    });
  }
});
