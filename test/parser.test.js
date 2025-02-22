import { describe, it } from "node:test"
import assert from "node:assert/strict"
import parse from "../src/parser.js"

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
  ["struct declaration", "struct Coordinate<Numeral> : Summable {x: Numeral, y:int, type:CoordType}"],
  ["class declaration", "class Summable { mod sum(self) -> int; sum: Option<int> }"],
  ["enum declaration", "enum SomeCoord {0: Polar, 2: Cartesian, other:CoordClass}"],
  ["module with no params, no return type", "mod m() {}"],
  ["no return type 2", "function f()->none {}"],
  ["module with one param", "mod m(x: int) {}"],
  ["module with two params", "mod m(x: int, y: boolean) {}"],
  ["module with no params + return type", "mod m(): int {}"],
  ["return function", "mod m(g: property)-> relation<2> {}"],
  ["array type for param", "mod m(x: [[[bool]]]) {}"],
  ["array type returned", "mod m() -> [[int]] {}"],
  ["type parameter and optional", "mod wrap<T>(item: T) -> Option<T> { return Option.new(T);} "],
  ["assignments", "mod m() { a--; c++; abc=9*3; a=1; }"],
  ["complex var assignment", "mod m() { c(5)[2] = 100;c.p.r=1;c.q(8).f(1,1).z=1; }"],
  ["complex var bumps", "mod m() { c(5).2++;c.p.r++;c.q(8).2(1,1).z--; }"],
  ["actions", "mod m() { x = 1;\nf(100);\nprint(1); }"],
  ["short if", "mod m() { if true then print(1); }"],
  ["longer if", "mod m() { if true then { print(1); x = 2; } else x = 3; }"],
  ["even longer if", "mod m() { if true { print(1); } else if false { print(1);} }"],
  ["while with empty block", "mod m() { while true do {}; }"],
  ["while with one statement block", "mod m() { while true do { x = 1; } }"],
  ["returns", "mod m() { x = for i in c { if f(i) then {j ++; return.. j;} else return.0 g(i); }; return x; }"],
  ["returning up", "mod m<A: ClassA, B>(a: A, B: b) -> B { for i in [0,1,2] ; }"],
]
// LINE 64
// Programs with syntax errors that the parser will detect
const syntaxErrors = [
  ["non-letter in an identifier", "let ab😭c = 2;", /Line 1, col 7:/],
  ["malformed number", "let x= 2.;", /Line 1, col 10:/],
  ["a float with an E but no exponent", "let x = 5E * 11;", /Line 1, col 10:/],
  ["a missing right operand", "print(5 -);", /Line 1, col 10:/],
  ["a non-operator", "print(7 * ((2 _ 3));", /Line 1, col 15:/],
  ["an expression starting with a )", "return );", /Line 1, col 8:/],
  ["a statement starting with expression", "x * 5;", /Line 1, col 3:/],
  ["an illegal statement on line 2", "print(5);\nx * 5;", /Line 2, col 3:/],
  ["a statement starting with a )", "print(5);\n)", /Line 2, col 1:/],
  ["an expression starting with a *", "let x = * 71;", /Line 1, col 9:/],
  ["negation before exponentiation", "print(-2**2);", /Line 1, col 10:/],
  ["mixing ands and ors", "print(1 && 2 || 3);", /Line 1, col 15:/],
  ["mixing ors and ands", "print(1 || 2 && 3);", /Line 1, col 15:/],
  ["associating relational operators", "print(1 < 2 < 3);", /Line 1, col 13:/],
  ["while without braces", "while true\nprint(1);", /Line 2, col 1/],
  ["if without braces", "if x < 3\nprint(1);", /Line 2, col 1/],
  ["while as identifier", "let for = 3;", /Line 1, col 5/],
  ["if as identifier", "let if = 8;", /Line 1, col 5/],
  ["unbalanced brackets", "function f(): int[;", /Line 1, col 18/],
  ["empty array without type", "print([]);", /Line 1, col 8/],
  ["random used like a function", "print(random(1,2));", /Line 1, col 15/],
  ["bad array literal", "print([1,2,]);", /Line 1, col 12/],
  ["empty subscript", "print(a[]);", /Line 1, col 9/],
  ["true is not assignable", "true = 1;", /Line 1, col 5/],
  ["false is not assignable", "false = 1;", /Line 1, col 6/],
  ["numbers cannot be subscripted", "print(500[x]);", /Line 1, col 10/],
  ["numbers cannot be called", "print(500(x));", /Line 1, col 10/],
  ["numbers cannot be dereferenced", "print(500 .x);", /Line 1, col 11/],
  ["no-paren function type", "function f(g:int->int) {}", /Line 1, col 17/],
  ["string lit with unknown escape", 'print("ab\\zcdef");', /col 11/],
  ["string lit with newline", 'print("ab\\zcdef");', /col 11/],
  ["string lit with quote", 'print("ab\\zcdef");', /col 11/],
  ["string lit with code point too long", 'print("\\u{1111111}");', /col 17/],
]
// LINE 101 = 64 + 37
describe("The parser", () => {
  for (const [scenario, source] of syntaxChecks) {
    it(`matches ${scenario}`, () => {
      assert(parse(source).succeeded())
    })
  }
  for (const [scenario, source, errorMessagePattern] of syntaxErrors) {
    it(`throws on ${scenario}`, () => {
      assert.throws(() => parse(source), errorMessagePattern)
    })
  }
})
