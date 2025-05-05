export function program(sections) {
  return { kind : "Program", sections }
}

// Used to define several things
// needed in module bodies rather than "null" 
// because for a class, module may actually be null
export const intrinsic = { kind: "Intrinsic", todo: true };

// Strings
export function implName(typeName, className) {
 return typeName + ".impl." + className;
}


export function typeName(type) {
    if (typeof type === "string") return type;
    
    if (type?.kind === "RelationType") {
      return "relation<" + type.number + ">";
    }
    if (type?.kind === "OperationType") {
      return "operation<" + type.number + ">";
    }
    if (type?.kind === "PropertyType") {
      let name = "property<";
      for (const n in type.numbers) {
        name = name + n.toString() + ","
      }
      name = name.slice(0, -1);
      name = name + ">";
      return name;
    }
    
    return type.name;
    
    /*
    if (
      type.kind === "Struct" 
      || type.kind === "Enum"
      || type.kind === "ClassObjectType"
      || type.kind === "ArrayType"
      || type.kind === "ListType"
    ) return type.name;
    
    return type.kind; // BoolType, Relation, etc
    //throw new Error("TBD");
    */
}

// Primitives
export const boolType = { kind: "BoolType", name: "bool", isType: true }
export const intType = { kind: "IntType", name: "int", isType: true }
export const stringType = { kind: "StringType", name: "string", isType: true }
export const voidType = { kind: "VoidType", name: "null", isType: true }

export const statementType = struct(
  "statement",
  [field("truth", boolType)]
);
export const valueType = struct(
  "value",
  [field("name", stringType)]
);
valueType.methods.push(module(
  "rel",
  null,
  [],
  listType(relationType(null)),
  intrinsic
));


export function mutRefType(basicType) {
  return { kind: "MutRefType", basicType, isType: true }
}
export function mutRef(variable) {
  return { kind: "MutRef", variable, type: mutRefType(variable.type) }
}

export function readVar(variable) {
  return { kind: "ReadVar", variable }
}

export function variable(name, content) {
  return { kind: "Variable", name, content, type: content.type }
}

export function variableDeclaration(variable) {
  return { kind: "VariableDeclaration", variable, isAction: true, type: voidType };
}

export function assignment(variable, readable) {
  return { kind: "Assignment", variable, readable, isAction: true, type: voidType }
}

export function struct(name, fields) {	// auto-impl superclasses
  return { kind: "Struct", name, fields, methods: [], isType: true }
}

export function enumeration(name, cases) { // cases have kind field
  return { kind: "Enum", name, cases, isType: true }
}

/*
export function moduleType(mutSelf, paramTypes, returnType) {
  return { kind: "ModuleType", mutSelf, paramTypes, returnType, isType: false } // false bc cannot be used
}
*/

export function module(name, mutSelf, params, returnType, body) {
  if (!Array.isArray(params)) {
    throw new Error("Params must be array");
  }
  return {
    kind: "Module",
    name,
    mutSelf, // false if self is immutable. If no self, null
    params,
    body,
    returnType
    /*
    type: moduleType(
      mutSelf,
      //(Array.isArray(params) ? params : []).map((p) => p.type),
      params.map(p => p.type),
      returnType
    ),
    */
  };
}

export function modBody(actions, returnType) {
  return { kind: "ModBody", actions, returnType }
}

export function classObjectType(classes) {
// If two classes have a method of the same name, what is the priority?
  //classes.sort(); 
  // Let the user define priority
  
  const name = classes[0].name;
  for (const classs in classes.slice(1)) {
    name = name + "+" + classs.name;
  }
  return { kind: "ClassObjectType", name, classes, isType: true }
}

export function classs(name, modules) {	// auto-impl "
  return { kind: "Class", name, modules }
}


export function classImpl(type, classs, modules) {
  const typeName = type.name;
  return { kind: "ClassImpl", name: implName(typeName, classs.name), subjectType: type, classs, modules }
}
	// Elements
export function field(name, type) {	// or param
  return { kind: "Field", name, type }
}

export function parameter(name, type) {
  return { kind: "Parameter", name, type }
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
  return { type: valueType, name }
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
  return { kind: "StatementWrapper", type: statementType, name, inner: innerStatement }
}
*/

export function relationType(number) {
  return { kind: "RelationType", number, isType: true }
}
export function operationType(number) {
  return { kind: "OperationType", number, isType: true }
}
export function propertyType(numbers) {
  return { kind: "PropertyType", numbers, isType: true }
}

	// Statements
export function customStatement(string) {
  return { kind: "CustomStatement", type: statementType, string }
}
export function equalityStatement(value1, value2) {
  return { kind: "EqualityStatement", type: statementType, value1, value2 }
}
export function negationStatement(inner) {
  return { kind: "Negation", type: statementType, inner }
}
export function filledRelation(relation, values) {
  return { kind: "FilledRelation", type: statementType, relation, values }
}
export function filledOperation(operation, statements) {
  return { kind: "FilledOperation", type: statementType, operation, statements }
}
export function filledProperty(property, relations) {
  return { kind: "FilledProperty", type: statementType, property, relations }
}

export function statementTruth(statement) {
  return { kind: "StatementTruth", statement, type: boolType }
}

export function namedStatement(name, inner) {
  return { kind: "NamedStatement", name, inner, type: statementType }
}
	// Declarations
export function valueDeclaration(value) {
  return { kind: "ValueDeclaration", value, type: "DataDecl" }
}
export function relationDeclaration(relation) {
  return { kind: "RelationDeclaration", relation, type: "DataDecl" }
}
export function operationDeclaration(operation) {
  return { kind: "OperationDeclaration", operation, type: "DataDecl" }
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
  return { kind: "StatementDeclaration", namedStatement, type: "DataDecl" }
}

// Actions

export function increment(variable) {
  return { kind: "Increment", variable, isAction: true, type: voidType }
}
export function decrement(variable) {
  return { kind: "Decrement", variable, isAction: true, type: voidType }
}

export function returnLine(expression, modBody) {
  return { kind: "Return", expression, modBody, isAction: true, type: voidType }
}

export function yieldLine(expression, modBody) {
  return { kind: "Yield", expression, modBody, isAction: true, type: voidType }
}

export function breakLine(modBody) {
  return { kind: "Break", modBody, isAction: true, type: voidType } 
}

export function continueLine(modBody) {
  return { kind: "Continue", modBody, isAction: true, type: voidType } 
}

// Variable constructors
export function construct(struct, contents) {
  return { kind: "Construct", contents, type: struct }
}

export function methodCall(variable, method, args) {
  return { kind: "MethodCall", variable, method, args, type: method.returnType}
}

export function associatedMethodCall(struct, method, args) {
  return { kind: "AssociatedMethodCall", struct, method, args, type: method.returnType}
}

export function modCall(mod, args) {
  return { kind: "ModCall", mod, args, type: mod.returnType}
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
export function comparison(name) {
  let classReq = "Equatable";
  if (name === "<" || name === ">") {
    classReq = "Comparable";
  }
  return { name, classReq, kind: "Comparison" }
}

	// x.i
export function varField(variable, field) {	// includes list indices
  return { kind: "VarField", variable, field, type: field.type }
}
export function varIndex(variable, index, type) { // get type from arrayType.basicType
  return { kind: "VarIndex", variable, index, type }
}
export function enumCase(enumeration, content, enumCase) {
  return { kind: "EnumCase", enumeration, content, enumCase, type: content.type }
}

	// Collections
export function arrayType(basicType, len) {
  const typeName = basicType.name;
  return { kind: "ArrayType", name: `[${typeName}, ${len}]`, basicType, len, isType: true }
}

export function listType(basicType) {	// array but growable
  const typeName = basicType.name;
  return { kind: "ListType", name: `[${typeName}]`, basicType, isType: true }
}

export function logosArray(contents) {
  return { kind: "Array", type: arrayType(contents[0].type, contents.length), contents }
}
export function emptyArray() {
  return { kind: "EmptyArray", type: arrayType(null, 0), contents: [] }
}
export function list(contents) {
  return { kind: "List", type: listType(readables[0].type), contents }
}
export function emptyList(type) {
  return { kind: "EmptyList", type: listType(type), contents: [] }
}

// Control Flow
export function ifFlow(condition, action, alternate) {
  return { kind: "IfFlow", condition, action, alternate, type: action.type, isAction: true }
}

export function whileFlow(condition, action) {
  return { kind: "WhileFlow", condition, action, type: action.type, isAction: true }
}

export function forFlow(argName, collection, action) {
  return { kind: "ForFlow", argName, collection, action, type: action.type, isAction: true }
}

export function matchFlow(variable, matchLines) {
  return { kind: "MatchFlow", variable, matchLines, type: matchLines[0].type, isAction: true }
}
export function matchLine(conditions, action) { // conditions must be [boolType], incl type check (see below)
  return { kind: "MatchLine", conditions, action, type: action.type }
}
export function matchConditionType(typeToMatch) {
  return { kind: "MatchConditionType", typeToMatch, type: boolType }
}

// Standard operations
const andInfix = infix("and",intrinsic);
const orInfix = infix("or", intrinsic);
const iffInfix = infix("<=>", intrinsic);
const ifThenInfix = infix("=>", intrinsic);
const thenIfInfix = infix("<=", intrinsic);


const anyClass = classs(
  "Any",
  []
);
const anyType = classObjectType([anyClass]);

// Error causes the program to immediately stop when encountered
    const errorClass = classs(
      "Error",
      []
    );
    
    const collectionClass = classs(
      "Collection",
      [module(
        "get", 
        false,
        [parameter("i", false, intType)],
        anyType,
        null
      )]
    );
    
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
        false,
        [parameter("other", false, null)],
        orderingEnum,
        null
      )]
    );
    if (comparableClass.modules?.length > 0) {
      const cmpModule = comparableClass.modules[0];
      if (cmpModule.params?.length >= 2) {
        cmpModule.params[0].type = comparableClass;
      }
    }
    
    const equatableClass = classs(
      "Equatable",
      [module(
        "eq",
        false,
        [parameter("other", false, null)],
        boolType,
        null
      )]
    );
    if (equatableClass.modules?.length > 0) {
      const eqModule = equatableClass.modules[0];
      if (eqModule.params?.length >= 2) {
        eqModule.params[0].type = equatableClass;
      }
    }
    
    
    
    /*
    const listStruct = struct(
      "List",
      [field("0", listType(anyType))]
    );
    listStruct.methods.push(module(
      "new",
      null,
      listStruct
    ));
    */

const strMod = module(
  "str", 
  null,
  [parameter("text", false, anyType)], 
  stringType, 
  intrinsic
);
const printMod = module(
  "print", 
  null,
  [parameter("text", false, stringType)], 
  voidType,
  intrinsic
);

const readModParam = parameter("obj", false, anyType);
const readModBody = modBody([
    returnLine(readVar(readModParam), null)
], anyType);
readModBody.actions[0].modBody = readModBody;
const readMod = module(
  "read",
  null,
  [readModParam],
  anyType,
  readModBody
);

const dropMod = module(
  "drop",
  null,
  [parameter("obj", false, anyType)],
  voidType,
  intrinsic
);

const typeMod = module(
  "type",
  null,
  [parameter("obj", false, anyType)],
  voidType,
  intrinsic
);

const concatMod = module(
  "concat",
  null,
  [
    parameter("str1", false, stringType),
    parameter("str2", false, stringType)
  ],
  stringType,
  intrinsic
);
  

// Literals
export function nullObject() { 
  return { kind: "NullObject", type: voidType }
}


//const anyToVoidType = moduleType([anyType], voidType);

export const standardLibrary = Object.freeze({
  int: intType,
  bool: boolType,
  string: stringType,
  "None": voidType,
  "value": valueType,
  "statement": statementType,
  //any: anyType,
  //print: intrinsicFunction("print", anyToVoidType),
  "and": andInfix,
  "or": orInfix,
  "<=>": iffInfix,
  "=>": ifThenInfix,
  "<=": thenIfInfix,
  "Any": anyClass,
  "Error": errorClass,
  "Ordering": orderingEnum,
  "Comparable": comparableClass,
  "Equatable": equatableClass,
  "Collection": collectionClass,
  //"List": listStruct,
  "str": strMod,
  "print": printMod, 
  "read": readMod, // gets the value from a mutref, basically deref
  "drop": dropMod,
  "type": typeMod,
  "concat": concatMod,
  
})

String.prototype.type = stringType
Number.prototype.type = intType
Boolean.prototype.type = boolType
