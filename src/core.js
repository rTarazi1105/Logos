export function program(sections) {
	return { kind : "Program", sections }
}

// Primitives
export const boolType = { kind: "BoolType" }
export const intType = { kind: "IntType" }
export const stringType = { kind: "StringType" }
export const voidType = { kind: "VoidType" }
export const anyType = { kind: "AnyType" }

export function assignment(variable, assignable) {
  return { kind: "Assignment", variable, assignable }
}

export function variable(name, mutable, type) {
  return { kind: "Variable", name, mutable, type }
}

export function typeParameter(name) {
  return { kind: "TypeParameter", name }
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

export function classs(name, typeParams, fields, modules) {	// auto-impl "
  return { kind: "Classs", name, typeParams, fields, modules }
}

export function classImpl(type, superClass, fieldsMap, modules) {
  return { kind: "ClassImpl", type, superClass, fieldsMap, modules }
}

export function fieldMapping(field1, field2) {
  return { kind: "FieldMapping", field1, field2 }
}

export function field(name, type) {	// or param
  return { kind: "Field", name, type }
}

export function parameter(name, mutable, type) {
  return { kind: "Parameter", name, mutable, type }
}


// DATA

export function value(name, relation) {
  return { kind: "Value", name, relation }
}

export function relation(name, args, statement) {
  return { kind: relationType(args.length), name, args, statement }
}

export function operation(name, args, statement) {
  return { kind: operationType(args.length), name, args, statement }
}

export function infix(name, operation) {	// Default: and, or, ==
  return { kind: "Infix", name, operation }
}

export function property(name, relationArgs, statement) {	// relationArgs are numbered
  return { kind: propertyType(args.length), name, relationArgs, statement }
}

export function statement(name, statement) {
  return { kind: "Statement", name, statement }
}

export function assumption(name, truth, statement) {
  return { kind: "Assumption", name, truth, statement }
}


export function relationType(number) {
  return { kind: "RelationType", number }
}
export function operationType(number) {
  return { kind: "OperationType", number }
}
export function propertyType(number, argNumbers) {
  return { kind: "PropertyType", number, argNumbers }
}

	// Statements
export function equalityStatement(value1, value2) {
  return { kind: "EqualityStatement", value1, value2 }
}
export function negationStatement(value) {
  return { kind: "Negation", value }
}
export function filledRelation(relation, values) {
  return { kind: "FilledRelation", relation, values }
}
export function filledOperation(operation, statements) {
  return { kind: "FilledOperation", operation, statements }
}
export function filledProperty(property, relations) {
  return { kind: "FilledProperty", property, relations }
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

// Actions

export function action(innerAction, type) {
  return { kind: actionType(type), innerAction }
}
export function actionType(returnType) {
  return { kind: "ActionType", returnType }
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

export function inEquality(variable1, variable2, comparison) {
  return { kind: "VarInEquality", variable1, variable2, comparison, type: boolType }
}

export function notVariable(boolean) {
  return { kind: "NotVariable", boolean, type: boolType }
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
export function arrayType(baseType, len) {
  return { kind: "ArrayType", baseType, len }
}
export function listType(baseType) {	// array but growable
  return { kind: "ListType", baseType }
}

export function array(assignable, len) {
  return { kind: arrayType(assignable.type, len), assignable }
}
export function list(assignables) {
  return { kind: listType(assignables[0].type), assignables }
}

// Control Flow
export function ifFlow(condition, action, alternate) {
  return { kind: "IfFlow", condition, action, alternate, type: action.returnType }
}

export function whileFlow(condition, action) {
  return { kind: "WhileFlow", condition, action, type: action.returnType }
}

export function forFlow(argName, collection, action) {
  return { kind: "ForFlow", argName, collection, action, type: action.returnType }
}

export function matchFlow(variable, matchLines) {
  return { kind: "MatchFlow", variable, matchLines, type: matchLines[0].type }
}
export function matchLine(condition, action) { // condition can be type or "if"
  return { kind: "MatchLine", condition, action, type: action.returnType }
}
export function matchConditionType(typeToMatch) {
  return { kind: "MatchConditionType", typeToMatch, type: boolType }
}




const anyToVoidType = moduleType([anyType], voidType)

export const standardLibrary = Object.freeze({
  int: intType,
  bool: boolType,
  string: stringType,
  void: voidType,
  any: anyType,
  print: intrinsicFunction("print", anyToVoidType),
})

String.prototype.type = stringType
BigInt.prototype.type = intType
Boolean.prototype.type = boolType
