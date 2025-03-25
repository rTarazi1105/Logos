export function program(sections) {
	return { kind : "Program", sections }
}

// Primitives
export const boolType = { kind: "BoolType" }
export const intType = { kind: "IntType" }
export const nullType = { kind: "NullType" }
export const anyType = { kind: "AnyType" }

export function assignment(variable, assignable) {
  return { kind: "Assignment", variable, assignable }
}

export function variable(name, mutable, type) {
  return { kind: "Variable", name, mutable, type }
}

export function typeParam(name) {
  return { kind: "TypeParam", name }
}

export function struct(name, fields) {	// auto-impl superclasses
  return { kind: "Struct", name, fields }
}

export function enumeration(name, cases) {
  return { kind: "Enum", name, cases }
}

export function module(name, params, returnType, body) {
  return { kind: "Module", name, params, returnType, body }
}

export function method(name, structOrClass, params, returnType, body) {
  return { kind: "Method", name, structOrClass, params, returnType, body }
}

export function classType(name, typeParameter, fields, modules) {	// auto-impl "
  return { kind: "ClassType", name, typeParameter, fields, modules }
}

export function classImpl(type, superClass, fieldsMap, modules) {
  return { kind: "ClassImplType", type, superClass, fieldsMap, modules }
}

export function fieldMapping(field1, field2) {
  return { kind: "FieldMapping", field1, field2 }
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

export function assumption(name, bool, statement) {
  return { kind: "Assumption", name, bool, statement }
}

	// Statements
export function equalityData(value1, value2) {
  return { kind: "EqualityData", value1, value2 }
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

export function action(body, returnType) {
  return { kind: "Action", body, returnType }
}

// assignment() above

export function incrementVar(variable) {
  return { kind: "Increment", variable }
}
export function decrementVar(variable) {
  return { kind: "Decrement", variable }
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
export function constructor(struct, assignables) {
  return { kind: "Struct", struct, assignables, type: struct.type) }
}

export function methodCall(variable, method, assignable) {
  return { kind: "MethodCall", variable, method, assignable, type: method.returnType}
}

export function modCall(mod, assignable) {
  return { kind: "ModCall", mod, assignable, type: mod.returnType}
}

export function inEqualityType(variable1, variable2, comparison) {
  return { kind: "VarInEquality", variable1, variable2, comparison, type: boolType }
}

export function notVariable(bool) {
  return { kind: "NotVariable", bool, type: boolType }
}

export function andVariable(bool1, bool2) {
  return { kind: "AndVariable", bool1, bool2, type: boolType }
}

export function orVariable(bool1, bool2) {
  return { kind: "OrVariable", bool1, bool2, type: boolType }
}
	// x.i
export function varField(variable, field) {	// includes list indices
  return { kind: "VarField", variable, field, type: field.type }
}
export function enumCase(enumName, caseOfEnum) {
  return { kind: "EnumCase", enumName, caseOfEnum, type: field.type }
}

	// Collections
export function tuple(assignables) {	// list
  return { kind: "Tuple", assignables }
}
export function array(assignable, length) {
  return { kind: "Array", assignable, length, type: arrayType(assignable.type) }
}

export function arrayType(baseType) {
  return { kind: "ArrayType", baseType }
}
export function tupleType(baseTypes) {
  return { kind: "TupleType", baseTypes }
}

// Control Flow
export function ifFlow(bool, action, elseAction) {
  return { kind: "IfFlow", bool, action, elseAction, type: action.returnType }
}

export function whileFlow(bool, action) {
  return { kind: "WhileFlow", bool, action, type: action.returnType }
}

export function forFlow(argName, collection, action) {
  return { kind: "ForFlow", argName, collection, action, type: action.returnType }
}

export function matchFlow(variable, matchLines) {
  return { kind: "MatchFlow", variable, matchLines, type: matchLines[0].type }
}
export function matchLine(condition, action) { // condition can be type or "if"
  return { kind: "MatchLine", condition, action, type: action.returnType
}
export function typeCondition(type) {
  return { kind: "MatchTypeCondition", type }
}




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
