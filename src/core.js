export function program(sections) {
  return { kind : "Program", sections }
}

// Primitives
export const boolType = { kind: "BoolType", isType: true }
export const intType = { kind: "IntType", isType: true }
export const stringType = { kind: "StringType", isType: true }
export const voidType = { kind: "VoidType", isType: true }
//export const anyType = { kind: "AnyType", isType: true }

export function fullType(mutable, basicType) {
  return { kind: "FullType", mutable, basicType }
}
export function mutRef(content) {
  return { kind: "MutRef", content, type: fullType(mutable, content.type) }
}

export function variable(name, fullType, contents) {
  return { kind: "Variable", name, fullType, contents }
}

export function variableDeclaration(variable) {
  return { kind: "VariableDeclaration", variable, type: "Action" };
}

export function assignment(variable, readable) {
  return { kind: "Assignment", variable, readable, type: "Action" }
}

export function struct(name, fields) {	// auto-impl superclasses
  return { kind: "Struct", name, fields, methods: [] }
}

export function enumeration(name, cases) { // cases have kind field
  return { kind: "Enum", name, cases }
}

export function moduleType(paramTypes, returnType) {
  return { kind: "ModuleType", paramTypes, returnType }
}

export function module(name, params, returnType, body) {
  if (!Array.isArray(params)) {
    throw new Error("Params must be array");
  }
  return {
    kind: "Module",
    name,
    params,
    body,
    type: moduleType(
      //(Array.isArray(params) ? params : []).map((p) => p.type),
      params.map(p => p.type),
      returnType
    ),
  };
}

export function classs(name, modules) {	// auto-impl "
  return { kind: "Class", name, modules }
}

export function classImpl(type, classs, modules) {
  return { kind: "ClassImpl", name: type.name + ".impl." + classs.name, subjectType: type, classs, modules }
}
	// Elements
export function field(name, fullType) {	// or param
  return { kind: "Field", name, type: fullType }
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
  return { type: "Value", name }
}

export function relation(name, args, statement) {
  return { kind: "Relation", type: relationType(args.length), name, args, statement }
}

export function operation(name, args, statement) {
  return { kind: "Operation", type: operationType(args.length), name, args, statement }
}

export function infix(name, operation) {	// Default: and, or, ==
  return { kind: "Infix", name, operation }
}

export function property(name, relationArgs, statement) {	// relationArgs are numbered
  return { kind: "Property", type: propertyType(relationArgs.map(r => r.type.number)), name, relationArgs, statement }
}

/*
export function statement(name, innerStatement) {
  return { kind: "StatementWrapper", type: "Statement", name, inner: innerStatement }
}
*/

export function relationType(number) {
  return { kind: "RelationType", number }
}
export function operationType(number) {
  return { kind: "OperationType", number }
}
export function propertyType(numbers) {
  return { kind: "PropertyType", numbers }
}

	// Statements
export function equalityStatement(value1, value2) {
  return { kind: "EqualityStatement", type: "Statement", value1, value2 }
}
export function negationStatement(inner) {
  return { kind: "Negation", type: "Statement", inner }
}
export function filledRelation(relation, values) {
  return { kind: "FilledRelation", type: "Statement", relation, values }
}
export function filledOperation(operation, statements) {
  return { kind: "FilledOperation", type: "Statement", operation, statements }
}
export function filledProperty(property, relations) {
  return { kind: "FilledProperty", type: "Statement", property, relations }
}
	// Declarations
export function valueDeclaration(value) {
  return { kind: "ValueDeclaration", value, type: "DataDecl" }
}
export function relationDeclaration(relation) {
  return { kind: "RelationDeclaration", relation, type: "DataDecl" }
}
export function operationDeclaration(operation) {
  return { kind: "OperationDeclaration", operation, type; "DataDecl" }
}
export function infixDeclaration(infix) {
  return { kind: "InfixDeclaration", infix, type: "DataDecl" }
}
export function propertyDeclaration(value) {
  return { kind: "ValueDeclaration", value, type: "DataDecl" }
}
export function assumptionDeclaration(statement, truth) {
  return { kind: "AssumptionDeclaration", statement, truth, type: "DataDecl" }
}
export function statementDeclaration(statement) {
  return { kind: "StatementDeclaration", statement, type: "DataDecl" }
}

// Actions

export function incrementVar(variable) {
  return { kind: "Increment", variable, type: "Action" }
}
export function decrementVar(variable) {
  return { kind: "Decrement", variable, type: "Action" }
}

export function returnLine(expression) {
  return { kind: "Return", expression, type: "Action" }
}

export function yieldLine(expression) {
  return { kind: "Yield", expression, type: "Action" }
}

export function breakLine(number) {
  return { kind: "Break", number, type: "Action" } 
}

export function continueLine(number) {
  return { kind: "Continue", number, type: "Action" } 
}

// Variable constructors
export function construct(struct, readables) {
  return { kind: "Struct", readables, type: struct }
}

export function methodCall(variable, method, args) {
  return { kind: "MethodCall", variable, method, args, type: method.type.returnType}
}

export function modCall(mod, args) {
  return { kind: "ModCall", mod, args, type: mod.type.returnType}
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

export function inEquality(variable1, variable2, comparison) {
  return { kind: "VarInEquality", variable1, variable2, comparison, type: boolType }
}
// Comparisons
export const lessThan = { name: "<", type: "Comparison" };
export const greaterThan = { name: "<", type: "Comparison" };
export const equalTo = { name: "==", type: "Comparison" };
export const unequalTo = { name: "!=", type: "Comparison" };

	// x.i
export function varField(variable, field) {	// includes list indices
  return { kind: "VarField", variable, field, type: field.type }
}
export function varIndex(variable, index, type) { // get type from arrayType.baseType
  return { kind: "VarIndex", variable, index, type }
}
export function enumCase(enumeration, field) {
  return { kind: "EnumCase", enumeration, field, type: field.type }
}

	// Collections
export function arrayType(baseType, len) {
  return { kind: "ArrayType", baseType, len }
}
// TODO: Add List(array)
export function listType(baseType) {	// array but growable
  return { kind: "ListType", baseType }
}

export function logosArray(contents) {
  return { kind: "Array", type: arrayType(contents[0].type, contents.length), contents }
}
export function emptyArray() {
  return { kind: "EmptyArray", type: arrayType(null, 0), contents: [] }
}
export function list(readables) {
  return { kind: "List", type: listType(readables[0].type), readables }
}
export function emptyList(type) {
  return { kind: "EmptyList", type: listType(type), contents: [] }
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
  return { kind: "MatchLine", condition, action }
}
export function matchConditionType(typeToMatch) {
  return { kind: "MatchConditionType", typeToMatch, type: boolType }
}


// Standard operations
const andInfix = infix("and",null);
const orInfix = infix("or", null);
const equalInfix = infix("==", null);


    const anyClass = classs(
      "Any",
      []
    );
    const errorClass = classs(
      "Error",
      [module(
        "print", 
        [variable("message", false, stringType)],
        voidType,
        null
      ),
      module(
        "crash",
        [],
        boolType,
        null
      )]
    );
    
    const collectionClass = classs(
      "Collection",
      [module(
        "get", 
        [variable("self", false, null)],
        anyClass,
        null
      ), module(
        "next",
        [variable("self", true, null)],
        voidType,
        null
      )]
    );
    if (collectionClass.modules?.length >= 2) {
      if (collectionClass.modules[0].params?.length > 0) {
        collectionClass.modules[0].params[0].type = collectionClass;
      }
      if (collectionClass.modules[1].params?.length > 0) {
        collectionClass.modules[1].params[0].type = collectionClass;
      }
    }
    
    const orderingEnum = enumeration("Ordering", [
      field("LessThan", voidType), 
      field("EqualTo", voidType), 
      field("GreaterThan", voidType)
    ]);
    const comparableClass = classs(
      "Comparable",
      [
        module(
        "cmp",
        [variable("self", false, null), variable("other", false, null)],
        orderingEnum,
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
      [module(
        "eq",
        [variable("self", false, null), variable("other", false, null)],
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
    
    
    
    
    const listStruct = struct(
      "List",
      [field("0", listType(anyClass))]
    );
    listStruct.methods.push(module(
      "new",
      null,
      null
    ));
    listStruct.methods[0].type.returnType = listStruct;

const strMod = module(
  "str", 
  [variable("text", false, anyClass)], 
  stringType, 
  null
);
const printMod = module(
  "print", 
  [variable("text", false, stringType)], 
  voidType,
  null
);

// Literals
export function nullObject() { 
  return { type: voidType }
}


//const anyToVoidType = moduleType([anyType], voidType);

export const standardLibrary = Object.freeze({
  int: intType,
  bool: boolType,
  string: stringType,
  void: voidType,
  //any: anyType,
  //print: intrinsicFunction("print", anyToVoidType),
  "and": andInfix,
  "or": orInfix,
  "==": equalInfix,
  "Any": anyClass,
  "Error": errorClass,
  "Ordering": orderingEnum,
  "Comparable": comparableClass,
  "Equatable": equatableClass,
  "Collection": collectionClass,
  "List": listStruct,
  "str": strMod,
  "print": printMod, 
})

String.prototype.type = stringType
Number.prototype.type = intType
Boolean.prototype.type = boolType
