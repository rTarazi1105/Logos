import { describe, it } from "node:test";
import assert from "node:assert/strict";
import parse from "../src/parser.js";
import analyze from "../src/analyzer.js";
import { variableDeclaration, program, variable } from "../src/core.js";

// Programs that are semantically correct
const semanticChecks = [
  [
    "if-else control flow",
    "mod starting() { if true then {a: 1; } else{ a: 2; }; }",
  ],
  ["loop structure", "mod starting() { for x in [1,2,3] do {x++}; }"],
  [
    "struct declaration and usage",
    "struct S {x: int, y: bool} mod starting() { a: boolean; x: 5; y: true; }",
  ],
  ["module declaration", "mod math(x: int, y: int) -> int { return x + y; }"],
];
const semanticErrors = [
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
    "operation on incompatible types",
    "mod starting() { x: boolean = true + 1; }",
    /Expected not a keyword/,
  ],
  [
    "struct field assignment with wrong type",
    "struct S {x: int, y: bool} mod starting() { s: S; s.x = true; }",
    /ReferenceError: idStr is not defined/,
  ],
  [
    "accessing non-existent struct field",
    "struct S {x: int} mod starting() { s: S; y = s.y; }",
    /Struct has no field named y/,
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
      analyze(parse("x: π + 2.2;")),
      program([
        variableDeclaration(
          variable("x", true, floatType),
          binary("+", variable("π", false, floatType), 2.2, floatType)
        ),
      ])
    );
  });
});
