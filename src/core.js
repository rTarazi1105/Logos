export function program(sections) {
	return { kind : "Program", sections }
}

// Primitives
export const boolType = { kind: "BoolType" }
export const intType = { kind: "IntType" }
export const nullType = { kind: "NullType" }
export const anyType = { kind: "AnyType" }

export function optionalType(baseType) {
  return { kind: "OptionalType", baseType }
}

export function assignment(variable, assignable) {
  return { kind: "Assignment", variable, assignable }
}

export function variable(name, mutable, type) {
  return { kind: "Variable", name, mutable, type }
}

export function typeDeclaration(type) {	// struct, enum or class
  return { kind: "TypeDeclaration", type }
}

export function structType(name, typeParameter, superClass, fields) {
  return { kind: "StructType", name, typeParameter, superClass, fields }
}

export function enumType(name, typeParameter, fields) {
  return { kind: "EnumType", name, typeParameter, fields }
}

export function moduleType(paramTypes, returnType) {
  return { kind: "ModType", paramTypes, returnType }
}

export function module(name, params, returnType, body) {	// returnType
  return { kind: "Module", name, params, returnType, body }
}

export function method(name, structOrClass, params, returnType, body) {	// returnType
  return { kind: "Method", name, structOrClass, params, returnType, body }
}

export function classType(name, typeParameter, superClass, fields, modules) {
  return { kind: "ClassType", name, typeParameter, superClass, fields, modules }
}

export function classImpl(type, superClass, fields, modules) {
  return { kind: "ClassImplType", type, superClass, fields, modules }
}

// DATA

export function value(name, relation) {
  return { kind: "Value", name, relation }
}

export function relation(name, args, statement) {
  return { kind: "Relation", name, args, statement }
}

export function operation(name, args, statement) {
  return { kind: "Operation", name, args, statement }
}

export function infix(name, operation) {	// Default: and, or, ==
  return { kind: "Infix", name, operation }
}

export function property(name, relationArgs, statement) {	// relationArgs are numbered
  return { kind: "Property", name, relationArgs, statement }
}

export function statement(name, statement) {
  return { kind: "Statement", name, statement }
}

export function assumption(name, boolean, statement) {
  return { kind: "Assumption", name, boolean, statement }
}

	// Statements
export function equalityData(value, value) {
  return { kind: "EqualityData", value, value }
}
export function negationData(value) {
  return { kind: "NegationData", value }
}
export function filledRelation(relation, values) {
  return { kind: "FilledRelationData", relation, values }
}
export function filledOperation(operation, statements) {
  return { kind: "FilledOperationData", operation, statements }
}
export function filledProperty(property, relations) {
  return { kind: "FilledPropertyData", property, relations }
}
	// Declarations
export function valueDeclaration(value) {
  return { kind: "ValueDeclaration", value }
}
export function relationDeclaration(relation) {
  return { kind: "RelationDeclaration", relation }
}
export function operationDeclaration(operation) {
  return { kind: "OperationDeclaration", operation }
}
export function infixDeclaration(infix) {
  return { kind: "InfixDeclaration", infix }
}
export function propertyDeclaration(value) {
  return { kind: "ValueDeclaration", value }
}
export function assumptionDeclaration(assumption) {
  return { kind: "AssumptionDeclaration", assumption }
}
export function statementDeclaration(statement) {
  return { kind: "StatementDeclaration", statement }
}

// Arguments

export function field(name, type) {
  return { kind: "Field", name, type }
}

export function relationArg(name, number) {
  return { kind: "RelationArg", name, number }
}

// Actions

// assignment() above

export function crementVar(variable, boolean) {
  return { kind: "Crement", variable, boolean }
}

export function constructor(struct, assignables) {
  return { kind: "Struct", struct, assignables, type: struct.type) }
}

export function returnLine(degree, expression) {
  return { kind: "Return", degree, expression }
}

export function yieldLine(degree, expression) {
  return { kind: "Yield", degree, expression) }
}

export const breakLine() = { kind: "Break" }

export const continueLine() = { kind: "Continue"}

// Variable constructors
export function methodCall(variable, method, assignable) {
  return { kind: "MethodCall", variable, method, assignable, type: method.type.returnType}
}

export function modCall(mod, assignable) {
  return { kind: "ModCall", mod, assignable}
}

export function inEqualityType(variable, variable, comparison) {
  return { kind: "VarInEquality", variable, variable, comparison }
}

export function notVariable(evalBool) {
  return { kind: "NotVariable", evalBool }
}

export function andVariable(evalBool, evalBool) {
  return { kind: "AndVariable", evalBool, evalBool }
}

export function orVariable(evalBool, evalBool) {
  return { kind: "OrVariable", evalBool, evalBool }
}
	// x.i
export function varField(variable, field) {	// includes list indices
  return { kind: "VarField", variable, field }
}
export function enumCase(enumName, caseOfEnum) {
  return { kind: "EnumCase", enumName, caseOfEnum }
}

	// Arrays
export function tuple(assignables) {
  return { kind: "Tuple", assignables }
}
export function array(assignable, length) {
  return { kind: "Array", assignable, length, type: arrayType(assignables[0].type) }
}
export function emptyArray(type) { 
  return { kind: "EmptyArray", type }
}

export function arrayType(baseType) {
  return { kind: "ArrayType", baseType }
}
export function tupleType(baseTypes) {
  return { kind: "ArrayType", baseTypes }
}

// Control Flow
export function ifFlow(evalBool, action, elseAction) {
  return { kind: "IfFlow", evalBool, action, elseAction }
}

export function whileFlow(evalBool, action) {
  return { kind: "WhileFlow", evalBool, action }
}

export function forFlow(argName, collection, action) {
  return { kind: "ForFlow", argName, collection, action }
}

export function matchFlow(variable, matchLines) {
  return { kind: "MatchFlow", variable, matchLines }
}

export function matchLineWithIf(evalBool, action) {
  return { kind: "MatchLineIf", evalBool, action }
}
export function matchLineWithType(type, action) {
  return { kind: "MatchLineType", type, action }
}












export function memberExpression(object, op, field) {
  return { kind: "MemberExpression", object, op, field, type: field.type }
}

export function functionCall(callee, args) {
  return { kind: "FunctionCall", callee, args, type: callee.type.returnType }
}

export function constructorCall(callee, args) {
  return { kind: "ConstructorCall", callee, args, type: callee }
}					// renamed...

const anyToNullType = moduleType([anyType], nullType)

export const standardLibrary = Object.freeze({
  int: intType,
  float: floatType,
  boolean: boolType,
  string: stringType,
  void: nullType,
  any: anyType,
  print: intrinsicFunction("print", anyToVoidType),
})

String.prototype.type = stringType
BigInt.prototype.type = intType
Boolean.prototype.type = boolType
