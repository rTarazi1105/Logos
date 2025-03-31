import { describe, it } from "node:test";
import assert from "node:assert/strict";
import parse from "../src/parser.js";
import analyze from "../src/analyzer.js";
import { variableDeclaration, program, variable } from "../src/core.js";

// Programs that are semantically correct
const semanticChecks = [
  // ["statement declaration", "yes: true;"],
  // ["variable declarations", 'mod starting() { x = 1; y = "false"; z = true; }'],
  ["valid variable assignment", "mod starting() { a: 2; }"],
];
const semanticErrors = [
  [
    "non-distinct struct fields",
    "struct S {x: boolean x: int}",
    /Expected "}"$/,
  ],
  [
    "use of undeclared variable",
    "mod starting() { y: int; y = 3; }",
    /Undefined variable: y/,
  ],
  [
    "type mismatch in assignment",
    "mod starting() { a: boolean = 5; }",
    /Type mismatch: expected boolean but got int/,
  ],
  [
    "redeclared variable in same scope",
    "mod starting() { x: int; x: boolean; }",
    /Variable x already declared in this scope/,
  ],
  [
    "invalid function return type",
    "mod f() -> string { return 42; }",
    /Return type mismatch: expected string but got int/,
  ],
  [
    "calling non-function as function",
    "mod starting() { x: int; x(); }",
    /Cannot call non-function x/,
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
