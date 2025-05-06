import { describe, it } from "node:test";
import assert from "node:assert/strict";
import parse from "../src/parser.js";
import analyze from "../src/analyzer.js";
import { statementDeclaration, program, filledOperation, intrinsic } from "../src/core.js";

// Programs that are semantically correct
const semanticChecks = [
  [
    "if-else control flow",
    "mod main() { if true then { statementA: true; } else { statementA: false; }; }",
  ],
  ["loop structure", "mod main() { for x in [1,2,3] do {x++; }; }"],
  [
    "struct declaration",
    "struct S {x: int, y: bool, c: property<1,2>, d: property}",
  ],
  ["module usage", "mod math(x: int, y: int) -> int { a: true; return x;}"],
  
  [
    "enum declaration and usage",
    "struct Blue {} enum Color { red: value, green: None, Blue } ",
    // mod create(v: value) { c = Color { v }; }
  ],
  [
    "method declaration and call",
    "mod increment(x: int) -> int { x++; return x; } mod main() { a = increment(5); }",
  ],
];
const semanticErrors = [
  [
    "non-distinct struct fields",
    "struct S {x: boolean, x: int}",
    /Expected "}"$/,
  ],
  [
    "type mismatch in assignment",
    "mod main(a: value) { a = 5; }",
    /Cannot assign a int to a value/,
  ],
  [
    "redeclared variable in same scope",
    "value x; mod main() { value y; x = y; }",
    /Cannot assign to data/,
  ],
  [
    "invalid control flow",
    "mod main() { if 5 then x = 1; }",
    /Expected "=<", ">=", "!=", "==", ">", or "<"/,
  ],
  [
    "calling non-function as function",
    "mod main() { x = 0; x(); }",
    /Expected a module/,
  ],
  [
     "invalid function return type",
     "mod main() -> string { return 42; }",
     /Incorrect type returned from closure/,
  ],
  [
    "invalid loop variable",
    "mod main() { for 5 in [1,2,3] do x++; }",
    /Expected "_" or a letter/,
  ],
  [
    "Incrementing a non-integer variable",
    "mod main() { x++; x = 0; }",
    /Expected an integer/,
  ],
  [
    "using uninitialized variable",
    "mod main() { y = x; }",
    /No variable found/,
  ],
  [
    "operation on incompatible types",
    "mod main() { x = 0 | true; }",
    /Expected ";"/,
  ],
  [
    "method on incompatible types",
    "mod main() { x = 1; y = x.add(true); }",
    /No method found/,
  ],
  [
    "struct field assignment with wrong type",
    "struct S {x: int, y: bool} mod main() { s = S { 0, true }; j = mut s.x; j = true; }",
    /Cannot assign a bool to a int/,
  ],
  [
    "accessing non-existent struct field",
    "struct S {x: int} mod main() { s = S { 0 }; y = s.y; }",
    /No such field/,
  ],
];



describe("The analyzer", () => {
  for (const [scenario, source] of semanticChecks) {
    it(`recognizes ${scenario}`, () => {
      assert.ok(analyze(parse(source)));
    });
  }
  for (const [scenario, source, errorMessagePattern] of semanticErrors) {
    it(`throws on ${scenario}`, () => {
      assert.throws(() => analyze(parse(source)), errorMessagePattern);
    });
  }
  it("produces the expected representation for a trivial program", () => {
    assert.deepEqual(
      analyze(parse("incomprehensible: true and false;")),
      program([
        statementDeclaration(
          filledOperation(
            intrinsic,
            [true, false]
          )
        ),
      ])
    );
  });
});
