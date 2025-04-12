export function program(sections) {
	return { kind : "Program", sections }
}

// These are used to check things that are used before declaration
// Namely, FilledStruct, FilledEnum, and FilledClass
export function requireType(name, type) { // FilledClass, FilledStruct, and FilledEnum can all be in ValidType
  return { kind: "RequireType", name, type }
}
export function requireMethod(object, methodName, paramTypes) { // Method could be for a class or struct
  return { kind: "RequireMethod", object, methodName, paramTypes } // Or method could be not declared yet
}
// Futures that are not here because they are just added to context: Method, module, struct, enum, class
// TODO Check futures whenever moving up to parent --> make futures local for methods and modules but not for types

// New solution for future
export function customType(name) {
  // TODO Whenever this is encountered, add it to the highest parent
  return { kind: "CustomType", typeParams: [null], methods: [], cases: null, fields: null }
  // Type.case --> add to cases + We know it's an enum
  // mod m(arg: Type) { arg.field } --> add to fields + We know it's a struct
  // For methods, it could be anything  
  
  // typeParams has to be a list of types that fulfill the classes hypothetically required by this customType
  // Using [null] marks this as an undefined custom type
}
/*
If we allow mods to be declared out of order, we lose type-checking (at least until the program is over)
Therefore NO, mods must be declared before use
However, types do not have to be declared before use
Therefore, you can call a method before it is declared, if the type has not been declared yet
*/


// Primitives
export const boolType = { kind: "BoolType", isType: true }
export const intType = { kind: "IntType", isType: true }
export const stringType = { kind: "StringType", isType: true }
export const voidType = { kind: "VoidType", isType: true }
export const anyType = { kind: "AnyType", isType: true }

export function assignment(variable, readable) {
  return { kind: "Assignment", variable, readable }
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
  return { kind: "TypeParameter", name, classes: new Map() } // Map<string, Class> - explained in analyzer
}

export function struct(name, typeParams, fields) {	// auto-impl superclasses
  return { kind: "Struct", name, typeParams, fields, methods: [] }
}

export function enumeration(name, typeParams, cases) { // cases have kind field
  return { kind: "Enum", name, typeParams, cases }
}

export function moduleType(paramsMut, paramTypes, returnType) {
  return { kind: "ModuleType", paramsMut, paramTypes, returnType }
}

export function module(name, typeParams, params, returnType, body) {
  if (!Array.isArray(params)) {
    throw new Error("Params must be array");
  }
  return {
    kind: "Module",
    name,
    typeParams,
    params,
    body,
    type: moduleType(
      /*
      (Array.isArray(params) ? params : []).map((p) => p.mutable),
      (Array.isArray(params) ? params : []).map((p) => p.type),
      */
      params.map(p => p.mutable),
      params.map(p => p.type),
      returnType
    ),
  };
}

export function filledType(type, typeArgs) {
  const filledType = { kind: "FilledType", inner: type, typeArgs, name: null };
  const name = getTypeName(filledType);
  filledType.name = name;
  return filledType;
}
// Used for organizing methods
// TODO: Move to string
export function getTypeName(userDefinedType) {
  const string = "";
  if (userDefinedType?.kind === "FilledType") {
    string = filledClass.inner.name + "<";
    for (const p in filledClass.typeArgs) {
      string = string + getTypeName(p) + ","; // should be struct, enum or class
    }
    string = string + ">";
  } else {
    return userDefinedType.name
  }
}

export function classs(name, typeParams, modules) {	// auto-impl "
  return { kind: "Class", name, typeParams, modules }
}

export function classImpl(type, filledClass, modules) {
  return { kind: "ClassImpl", name: type.name + ".impl." + getTypeName(filledClass), type, filledClass, modules }
}
	// Elements
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
  return { kind: "Statement", isStatement: true, name, inner: innerStatement }
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
  return { kind: "EqualityStatement", isStatement: true, value1, value2 }
}
export function negationStatement(inner) {
  return { kind: "Negation", isStatement: true, inner }
}
export function filledRelation(relation, values) {
  return { kind: "FilledRelation", isStatement: true, relation, values }
}
export function filledOperation(operation, statements) {
  return { kind: "FilledOperation", isStatement: true, operation, statements }
}
export function filledProperty(property, relations) {
  return { kind: "FilledProperty", isStatement: true, property, relations }
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

export function action(innerAction, returnType) {
  return { kind: "Action", innerAction, returnType }
}

// assignment() above

export function incrementVar(variable) {
  return { kind: "Increment", variable }
}
export function decrementVar(variable) {
  return { kind: "Decrement", variable }
}

export function returnLine(expression) {
  return { kind: "Return", expression }
}

export function yieldLine(expression) {
  return { kind: "Yield", expression }
}

export function breakLine(number) { return { kind: "Break", number } }

export function continueLine(number) { return { kind: "Continue", number } }

// Variable constructors
export function construct(filledStruct, readables) {
  return { kind: "Struct", readables, type: filledStruct }
}

export function methodCall(variable, method, args) {
  return { kind: "MethodCall", variable, method, args, type: method.type.returnType}
}

export function modCall(mod, args) {
  return { kind: "ModCall", mod, args, type: mod.type.returnType}
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
export function varIndex(variable, index, type) { // get type from arrayType.baseType
  return { kind: "VarIndex", variable, index, type }
}
export function enumCase(filledEnum, field) {
  return { kind: "EnumCase", filledEnum, field, type: field.type }
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
  return { type: arrayType(contents[0].type, contents.length), contents }
}
export function emptyArray() {
  return { type: arrayType(null, 0), contents: [] }
}
export function list(readables) {
  return { type: listType(readables[0].type), readables }
}
export function emptyList(type) {
  return { type: listType(type), contents: [] }
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
const andInfix = infix("and",null);
const orInfix = infix("or", null);
const equalInfix = infix("==", null);


    const errorClass = classs(
      "Error",
      [],
      [module(
        "print", 
        [],
        [parameter("message", false, stringType)],
        voidType,
        null
      ),
      module(
        "crash",
        [],
        [],
        boolType,
        null
      )]
    );
    
    const collectionBaseType = typeParameter("T");
    const collectionClass = classs(
      "Collection",
      [collectionBaseType],
      [module(
        "get", 
        [],
        [parameter("self", false, null)],
        collectionBaseType,
        null
      ), module(
        "next",
        [],
        [parameter("self", true, null)],
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
    
    const orderingEnum = enumeration("Ordering", [], [
      field("LessThan", voidType), 
      field("EqualTo", voidType), 
      field("GreaterThan", voidType)
    ]);
    const comparableClass = classs(
      "Comparable",
      [],
      [
        module(
        "cmp",
        [],
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
      [module(
        "eq",
        [],
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
    
    
    
    const listBaseType = typeParameter("T");
    const listStruct = struct(
      "List",
      [listBaseType],
      [field("0", listType(listBaseType))]
    );
    listStruct.methods.push(module(
      "new",
      [],
      [],
      null,
      null
    ));
    listStruct.methods[0].type.returnType = listStruct;

const strMod = module(
  "str", 
  [], 
  [parameter("text", false, anyType)], 
  stringType, 
  null
);
const printMod = module(
  "print", 
  [], 
  [parameter("text", false, stringType)], 
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
  any: anyType,
  //print: intrinsicFunction("print", anyToVoidType),
  "and": andInfix,
  "or": orInfix,
  "==": equalInfix,
  "Error": errorClass,
  "Ordering": orderingEnum,
  "Comparable": comparableClass,
  "Equatable": equatableClass,
  "Collection": collectionClass,
  "List": listStruct,
  "str": strMod
  "print": printMod, 
})

String.prototype.type = stringType
Number.prototype.type = intType
Boolean.prototype.type = boolType
