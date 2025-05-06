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
  /*
  [
    "enum declaration and usage",
    "struct Blue {} enum Color { red: value, green: None, Blue } mod create(v: value) { c = Color { v }; }",
  ],
  [
    "method declaration and call",
    "mod increment(x: int) -> int { x++; return x; } mod main() { a = increment(5); }",
  ],
  */
];
const semanticErrors = [
  /*
  [
    "non-distinct struct fields",
    "struct S {x: boolean x: int}",
    /Expected "}"$/,
  ],
  [
    "type mismatch in assignment",
    "mod starting() { a: boolean = 5; }",
    /Expected type Value/,
  ],
  [
    "redeclared variable in same scope",
    "mod starting() { x: int; x: boolean; }",
    /Expected statement/,
  ],
  [
    "invalid control flow",
    "mod starting() { if 5 then x: 1; }",
    /Expected "!=", "==", ">", or "<"/,
  ],
  [
    "calling non-function as function",
    "mod starting() { x: int; x(); }",
    /Expected statement/,
  ],
  // [
  //   "invalid function return type",
  //   "mod f() -> string { return 42; }",
  //   /Return type mismatch: expected string but got int/,
  // ],

  [
    "invalid loop variable",
    "mod starting() { for 5 in [1,2,3] do x++; }",
    /Expected "_" or a letter/,
  ],
  [
    "using uninitialized variable",
    "mod starting() { x: int; y = x + 1; }",
    /Variable x used before initialization/,
  ],
  [
    "operation on incompatible types",
    "mod starting() { x: boolean = true + 1; }",
    /Expected not a keyword/,
  ],
  [
    "struct field assignment with wrong type",
    "struct S {x: int, y: bool} mod starting() { s: S; s.x = true; }",
    /Type mismatch: expected int but got boolean/,
  ],
  [
    "accessing non-existent struct field",
    "struct S {x: int} mod main() { s = S { 0 }; y = s.y; }",
    /Struct has no field named y/,
  ],
  */
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
