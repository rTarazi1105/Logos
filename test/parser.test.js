import { describe, it } from "node:test";
import assert from "node:assert/strict";
import parse from "../src/parser.js";
// https://cs.lmu.edu/~ray/notes/howtowriteacompiler/
// Programs expected to be syntactically correct
const syntaxChecks = [
  ["simplest syntactically correct program", "yes : true;"],
  ["multiple statements", "yes : true; no : false;"],
  ["value declaration", "value 0;"],
  ["relation", "is_zero : x : x = 0;"],
  ["statement", "zero_is_zero_but : xnor[is_zero[0], A]"],
  ["assumption", "is_zero[0] true;"],
  ["named assumption", "assume def_zero : is_zero[0] true;"],
  ["operation", "operation xnor : A,B : (A && B) || (not A && not B);"],
  ["infix", "infix b both;"],
  ["property", "property injection : f<1> : f[x] = f[y] == x = y;"],
  ["calling a property", "injection[succession] true;"],
  [
    "struct declaration",
    "struct Coordinate<Numeral> : Summable {x: Numeral, y:int, type:CoordType}",
  ],
  [
    "class declaration",
    "class Summable { mod sum(self) -> int; sum: Option<int> }",
  ],
  [
    "enum declaration",
    "enum SomeCoord {0: Polar, 2: Cartesian, other:CoordClass}",
  ],
  ["module with no params, no return type", "mod m() {}"],
  ["no return type 2", "function f()->none {}"],
  ["module with one param", "mod m(x: int) {}"],
  ["module with two params", "mod m(x: int, y: boolean) {}"],
  ["module with no params + return type", "mod m(): int {}"],
  ["return function", "mod m(g: property)-> relation<2> {}"],
  ["array type for param", "mod m(x: [[[bool]]]) {}"],
  ["array type returned", "mod m() -> [[int]] {}"],
  [
    "type parameter and optional",
    "mod wrap<T>(item: T) -> Option<T> { return Option.new(T);} ",
  ],
  ["assignments", "mod m() { a--; c++; abc=9*3; a=1; }"],
  [
    "complex var assignment",
    "mod m() { c(5)[2] = 100;c.p.r=1;c.q(8).f(1,1).z=1; }",
  ],
  ["complex var bumps", "mod m() { c(5).2++;c.p.r++;c.q(8).2(1,1).z--; }"],
  ["actions", "mod m() { x = 1;\nf(100);\nprint(1); }"],
  ["short if", "mod m() { if true then print(1); }"],
  ["longer if", "mod m() { if !true then { print(1); x = 2; } else x = 3; }"],
  [
    "even longer if",
    "mod m() { if 0 > 1 then print(1) else if false then {print(1); return 1 } else print(0)",
  ],
  ["while with empty block", "mod m() { while true do {}; }"],
  ["while with one statement block", "mod m() { while true do { x = 1; } }"],
  [
    "returns",
    "mod m() { x = for i in c { if f(i) then {j ++; return.. j;} else return.0 g(i); }; return x; }",
  ],
  [
    "returning up",
    "mod m<A: SomeClass, B>(a: A, b: B) -> B { sum = 0; last = for i in [0, 1, 2] do { sum++; if i == 1 then return.0 i}; if sum != 3 then return b else { if last == 1 return.. b.inverse(); }; }",
  ],
  [
    "chained ors",
    'mod m(a: bool, b: bool, c: bool) { if a and b and c then print("ok"); }',
  ],
  ["chained ands", 'mod m() { if a and b and c then print("ko") }'],
  ["chained ors in data", "this_statement = a || b || c"],
  ["chained ands in data", "operation all : A, b, C = A && b && C"],
  ["chained ors in data", "operation all : A, b, C = A || b || C"],
  ["arithmetic precedence", "mod m() { x = 1 + 2 * 3 - 4 / 5; }"],
  ["string literal", 'mod m() { s = "Hello \\n world"; }'],
  ["comment inside block", "mod m() { -- this is a comment\n a = 1; }"],
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
