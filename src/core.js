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

export function returnType(mutable, type) {
  return { kind: "ReturnType", mutable, type }
}

export function variableDeclaration(variable) {
  return { kind: "VariableDeclaration", variable };
}

export function variable(name, type, contents) { // always mutable
  return { kind: "Variable", name, type, contents }
}

export function typeParameter(name) {
  return { kind: "TypeParameter", name, classes: new Set() }
}

export function struct(name, typeParams, fields) {	// auto-impl superclasses
  return { kind: "Struct", name, typeParams, fields, methods: [] }
}

export function enumeration(name, typeParams, cases) { // cases are like fields
  return { kind: "Enum", name, typeParams, cases }
}

export function moduleType(paramsMut, paramTypes, returnType) {
  return { kind: "ModuleType", paramsMut, paramTypes, returnType }
}

export function module(name, typeParams, params, returnType, body) {
  return {
    kind: "Module",
    name,
    typeParams,
    params,
    body,
    type: moduleType(
      (Array.isArray(params) ? params : []).map((p) => p.mutable),
      (Array.isArray(params) ? params : []).map((p) => p.type),
      returnType
    ),
  };
}

export function filledStruct(struct, filledTypeParams) {
  return { kind: "FilledStruct", struct, filledTypeParams }
}
export function filledEnum(enumeration, filledTypeParams) {
  return { kind: "FilledEnum", enumeration, filledTypeParams }
}

export function filledClass(classs, filledTypeParams) {
  return { kind: "FilledClass", classs, filledTypeParams }
}
// Used for organizing methods
export function getTypeName(userDefinedType) {
  if (userDefinedType.kind === "FilledClass") {
    string = filledClass.classs.name + "<";
    for (p in filledClass.filledTypeParams) {
      string = string + stringify(p); // should be struct, enum or class
    }
  } else {
    return userDefinedType.name
  }
}

export function classs(name, typeParams, fields, modules) {	// auto-impl "
  return { kind: "Classs", name, typeParams, fields, modules }
}

export function classImpl(type, classs, fieldsMap, modules) {
  return { kind: "ClassImpl", type, classs, fieldsMap, modules }
}
	// Elements

export function fieldMapping(name, nameInType, type) { // first name is in class
  return { kind: "FieldMapping", name, nameInType, type }
}

export function field(name, type) {	// or param
  return { kind: "Field", name, type }
}

export function parameter(name, mutable, type) {
  return { kind: "Parameter", name, mutable, type }
}

	// Declarations
export function structDeclaration(struct) {
  return { kind: "StructDeclaration", struct }
}
export function enumDeclaration(enumeration) {
  return { kind: "EnumDeclaration", enumeration }
}
export function moduleDeclaration(module) {
  return { kind: "ModuleDeclaration", module }
}
export function methodDeclaration(struct, module) {
  return { kind: "MethodDeclaration", struct, module }
}
export function classDeclaration(classs) {
  return { kind: "ClassDeclaration", classs }
}

// DATA


export function value(name) {
  return { kind: "Value", name }
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

export function statement(name, innerStatement) {
  return { kind: "Statement", statement: true, name, inner: innerStatement }
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
  return { kind: "EqualityStatement", statement: true, value1, value2 }
}
export function negationStatement(inner) {
  return { kind: "Negation", statement: true, inner }
}
export function filledRelation(relation, values) {
  return { kind: "FilledRelation", statement: true, relation, values }
}
export function filledOperation(operation, statements) {
  return { kind: "FilledOperation", statement: true, operation, statements }
}
export function filledProperty(property, relations) {
  return { kind: "FilledProperty", statement: true, property, relations }
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
export function assumptionDeclaration(statement, truth) {
  return { kind: "AssumptionDeclaration", statement, truth }
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
  return { kind: "Yield", degree, expression }
}

export const breakLine = { kind: "Break" }

export const continueLine = { kind: "Continue"}

// Variable constructors
export function construct(filledStruct, assignables) {
  return { kind: "Struct", assignables, type: filledStruct }
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
export function enumCase(filledEnum, field) {
  return { kind: "EnumCase", filledEnum, field, type: field.type }
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


// Standard operations
export function andStatement(s1, s2) {
  return { kind: "AndStatement", statement: true, s1, s2 }
}
const andOp = operation("and1234567890", ["A","B"], andStatement("A","B"))

export function orStatement(s1, s2) {
  return { kind: "OrStatement", statement: true, s1, s2 }
}
const orOp = operation("or1234567890", ["A","B"], orStatement("A","B"))

export function equalStatement(s1, s2) {
  return { kind: "equalStatement", statement: true, s1, s2 }
}
const equalOp = operation("equal1234567890", ["A","B"], equalStatement("A","B"))



    const errorClass = classs(
      "Error",
      [],
      [
        field("crash", boolType), 
        field("print", boolType)
      ],
      [module(
        "print", 
        [parameter("message", false, stringType)],
        voidType,
        null
      )]
    );
    
    const collectionBaseType = typeParameter("T");
    const collectionClass = classs(
      "Collection",
      [collectionBaseType],
      [
        field("crash", boolType), 
        field("print", boolType)
      ],
      [module(
        "get", 
        [parameter("self", false, null)],
        collectionBaseType,
        null
      ), module(
        "next",
        [parameter("self", true, null)],
        voidType,
        null
      )]
    );
    // How deep does this go? Be careful maybe
    if (collectionClass.modules?.length >= 2) {
      if (collectionClass.modules[0].params?.length > 0) {
        collectionClass.modules[0].params[0].type = collectionClass;
      }
      if (collectionClass.modules[1].params?.length > 0) {
        collectionClass.modules[1].params[0].type = collectionClass;
      }
    }
    
    const orderingEnum = enumeration("Ordering", [], [
      field("LessThan", voidType), 
      field("EqualTo", voidType), 
      field("GreaterThan", voidType)
    ]);
    const comparableClass = classs(
      "Comparable",
      [],
      [],
      [
        module(
        "cmp",
        [parameter("self", false, null), parameter("other", false, null)],
        filledEnum(orderingEnum,[]),
        null
      )]
    );
    if (comparableClass.modules?.length > 0) {
      const cmpModule = comparableClass.modules[0];
      if (cmpModule.params?.length >= 2) {
        cmpModule.params[0].type = comparableClass;
        cmpModule.params[1].type = comparableClass;
      }
    }
    
    const equatableClass = classs(
      "Equatable",
      [],
      [],
      [module(
        "eq",
        [parameter("self", false, null), parameter("other", false, null)],
        boolType,
        null
      )]
    );
    if (equatableClass.modules?.length > 0) {
      const eqModule = equatableClass.modules[0];
      if (eqModule.params?.length >= 2) {
        eqModule.params[0].type = equatableClass;
        eqModule.params[1].type = equatableClass;
      }
    }


const anyToVoidType = moduleType([anyType], voidType)

export const standardLibrary = Object.freeze({
  int: intType,
  bool: boolType,
  string: stringType,
  void: voidType,
  any: anyType,
  //print: intrinsicFunction("print", anyToVoidType),
  "and": andOp,
  "or": orOp,
  "==": equalOp,
  "Error": errorClass,
  "Ordering": orderingEnum,
  "Comparable": comparableClass,
  "Equatable": equatableClass,
  "Collection": collectionClass,
})

String.prototype.type = stringType
BigInt.prototype.type = intType
Boolean.prototype.type = boolType
