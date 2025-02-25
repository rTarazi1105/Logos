export function program(sections) {
	return { kind : "Program", sections }
}

export function variableDeclaration(variable, initializer) {
  return { kind: "VariableDeclaration", variable, initializer }
}

export function variable(name, mutable, type) {
  return { kind: "Variable", name, mutable, type }
}

export function typeDeclaration(type) {
  return { kind: "TypeDeclaration", type }
}

export const boolType = { kind: "BoolType" }
export const intType = { kind: "IntType" }
export const nullType = { kind: "NullType" }		// renamed
export const anyType = { kind: "AnyType" }

export function structType(name, fields) {
  return { kind: "StructType", name, fields }
}

export function field(name, type) {
  return { kind: "Field", name, type }
}

export function moduleDeclaration(mod, params, body) {
  return { kind: "ModuleDeclaration", mod, params, body }	// renamed
}

export function mod(name, type) {				// renamed
  return { kind: "Module", name, type }
}

export function increment(variable) {
  return { kind: "Increment", variable }
}

export function decrement(variable) {
  return { kind: "Decrement", variable }
}

export function assignment(target, source) {
  return { kind: "Assignment", target, source }
}

export const breakStatement = { kind: "BreakStatement" }

export function upReturnStatement(degree, expression) {
  return { kind: "upReturnStatement" }
}

export function returnStatement(expression) {
  return { kind: "ReturnStatement", degree, expression }
}

export function nullReturnStatement() {
  return { kind: "nullReturnStatement" }
}

export function ifStatement(test, consequent, alternate) {
  return { kind: "IfStatement", test, consequent, alternate }
}

export function shortIfStatement(test, consequent) {
  return { kind: "ShortIfStatement", test, consequent }
}

export function whileStatement(test, body) {
  return { kind: "WhileStatement", test, body }
}

export function forStatement(iterator, collection, body) {
  return { kind: "ForStatement", iterator, collection, body }
}

export function conditional(test, consequent, alternate, type) {
  return { kind: "Conditional", test, consequent, alternate, type }
}

export function infix(op, left, right, type) {
  return { kind: "InfixOperation", op, left, right, type }
}
export function subscript(array, index) {
  return { kind: "SubscriptExpression", array, index, type: array.type.baseType }
}

export function arrayExpression(elements) {
  return { kind: "ArrayExpression", elements, type: arrayType(elements[0].type) }
}

export function emptyArray(type) {
  return { kind: "EmptyArray", type }
}

export function memberExpression(object, op, field) {
  return { kind: "MemberExpression", object, op, field, type: field.type }
}

export function functionCall(callee, args) {
  return { kind: "FunctionCall", callee, args, type: callee.type.returnType }
}

export function constructorCall(callee, args) {
  return { kind: "ConstructorCall", callee, args, type: callee }
}


export function moduleType(paramTypes, returnType) {
  return { kind: "ModuleType", paramTypes, returnType }
}									// renamed...

const stringToIntsType = moduleType([stringType], arrayType(intType))
const anyToNullType = moduleType([anyType], nullType)

export const standardLibrary = Object.freeze({
  int: intType,
  float: floatType,
  boolean: boolType,
  string: stringType,
  void: voidType,
  any: anyType,
})

String.prototype.type = stringType
BigInt.prototype.type = intType
Boolean.prototype.type = boolType
