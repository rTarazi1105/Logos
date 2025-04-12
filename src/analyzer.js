// Note on comments: "future" refers to a type that has been called but not defined (declared) yet
// Note 2: Types and methods can be declared out of order, but class impl must be declared before use?
// TODO: Remove field mapping

// TODO Add inbuilt list etc

import * as core from "../src/core.js";
class Context {
  constructor({
    parent = null,
    locals = new Map(),
    classes = new Map(),
    inLoop = false,
    inData = false,
    module = null,
    musts = new Array(),
  }) {
    // Data can have locals
    // Data can be inside module but not vice versa
    Object.assign(this, { parent, locals, classes, inLoop, module, musts });
  }
  // Default will be Key: String, value: T
  // For truths, key: statement, value: bool
  add(name, entity) {
    this.locals.set(name, entity);
  }
  lookup(key) {
    return this.locals.get(key) || this.parent?.lookup(key);
  }

// TODO: Add this everywhere
  addStatement(statement, truth) {
    /*
    if (/bool/.test(statement)) {
      return; // Don't add "true"
    }
    */ // Can add "true" as "false", it should show up as a contradiction
    
    if (statement.isStatement !== true) {
      throw new Error(`Not a statement: ${statement}`);
    }

    // Unwrap
    while (statement.kind === "Statement") {
      statement = statement.inner;
    }
    // Unwrap negation
    while (statement.kind === "Negation") {
      statement = statement.inner;
      truth = !truth;
    }

    const existing = this.locals.get(statement);
    if (existing != null) {
      if (existing != truth) {
        throw new Error(`Direct contradiction in data: ${statement.name}`);
      } else {
        console.log(`Statement ${statement.name} redundant`);
        return;
      }
    }

    this.locals.set(statement, truth);
  }

  // Collection, Equatable, Comparable, Error
  // classes: Map<type, Map<string,class>>
  
  // Why are classes indexed by string? Bc:
  // We don't have Struct<T> impl Class<T> thankfully
  // If Struct impl Class<A>, Struct cannot impl Class<B>
  
  classify(filledClass, type) {
    if (!this.classes.containsKey(type)) {
      this.classes.set(type, new Map());
    }
    
    const typeEntry = this.classes.get(type);
    typeEntry.set(type.name, type);
  }
  
  lookupClass(className, type) {
    let superClasses = this.classes.get(type);
    if (type.kind === "TypeParameter") {
      superClasses = type.classes;
    }
    
    if (superClasses.containsKey(className)) {
      return true;
    }
    
    for (let filledClass of superClasses.values()) {
      if (lookupClass(className, filledClass)) {
        return true;
      }
    }
    
    // Don't look in parent - it should be the same
    // this.parent.lookupClass(className, type)
    
    return false;
  }
  
  getClass(className, type) {
    let superClasses = this.classes.get(type);
    if (type.kind === "TypeParameter") {
      superClasses = type.classes;
    }
    
    for (let [name, filledClass] of superClasses) {
      if (name === className) {
        return filledClass;
      }
      const getSuperClass = getClass(className, filledClass);
      if (getSuperClass != null) {
        return getSuperClass;
      }
    }
  }

  static root() {
    let equatableClass = core.standardLibrary["Equatable"];
    let comparableClass = core.standardLibrary["Comparable"];

    return new Context({
      locals: new Map(Object.entries(core.standardLibrary)),
      classes: new Map([
        [core.boolType, new Map([["Equatable",equatableClass]])],
        [(core.intType, new Map([["Comparable",comparableClass]]))],
        [(comparableClass, new Map([["Equatable",equatableClass]]))],
      ]),
    });
  }
  newChildContext(props) {
    return new Context({ ...this, ...props, parent: this, locals: new Map() });
  }
}

export default function analyze(match) {
  let context = Context.root();

  function must(condition, message, errorLocation) {
    if (!condition) {
      const prefix = errorLocation.at.source.getLineAndColumnMessage();
      throw new Error(`${prefix}${message}`);
    }
  }

  function mustNotAlreadyBeDeclared(name, at) {
    must(!context.lookup(name), `Identifier ${name} already declared`, at);
  }

  // Only at the end
  function mustHaveBeenFound(entity, name, at) {
    must(entity, `Identifier ${name} not declared`, at);
  }
  
  function mustBeTypeT(e, type, msg, at) {
    must(e.type === type, msg, at);
  }

  function mustBeInteger(e, at) {
    mustBeTypeT(e, core.intType, "Expected an integer", at);
  }

  function mustBeBoolean(e, at) {
    mustBeTypeT(e, core.booleanType, "Expected a boolean", at);
  }

  function mustBeVoid(e, at) {
    mustBeTypeT(e, core.voidType, "Expected void", at);
  }

  function mustBeStatement(e, at) {
    must(
      e?.isStatement === true || /bool/.test(e),
      "Expected statement",
      at
    );
  }

  function mustBeKindK(e, t, at) {
    must(e?.kind === t, `Expected type ${t}`, at);
  }

  function mustHaveClass(e, className, at) {
    must(
      context.lookupClass(className, e),
      `Expected type to have class <${className}>`,
      at
    );
  }

  function mustNotHaveClass(e, className, at) {
    must(
      !context.lookupClass(className, e),
      `Class <${className}> already declared or not expected`,
      at
    );
  }

// Unused but should be FilledStruct
  function mustBeAStruct(e, at) {
    must(e.type?.kind === "Struct", "Expected a struct", at);
  }
// ^
  function mustBeAStructOrClass(e, at) {
    must(
      e.type?.kind === "Struct" || e.type?.kind === "Class",
      "Expected a struct or class",
      at
    );
  }

  function mustBothHaveTheSameType(e1, e2, at) {
    // must(e2.type === core.anyType || equivalent(e1.type, e2.type), "Operands do not have the same type", at)
    must(
      equivalent(e1.type, e2.type),
      "Operands do not have the same type",
      at
    );
  }

  function mustAllHaveSameType(expressions, at) {
    // Used to check the elements of an array expression, and the two
    // arms of a conditional expression, among other scenarios.
    must(
      expressions
        .slice(1)
        .every((e) => equivalent(e.type, expressions[0].type)),
      "Not all elements have the same type",
      at
    );
  }

  function includesAsField(structType, type) {
    // Whether the struct type has a field of type "type", directly
    return structType.fields.some((field) => field.type === type);
  }

  function mustNotBeSelfContaining(structType, at) {
    const containsSelf =
      includesAsField(structType, structType) ||
      includesAsField(structType, "Self");
    must(!containsSelf, "Struct type must not be self-containing", at);
  }

  function mustBeMutable(variable, at) {
    must(variable.mutable === true, at);
  }

  // ValidType in Ohm
  function mustBeObject(e, at) {
    const isPrimitiveType = /int|bool|void|any/.test(e);
    const isDataType = /Value|Infix|Statement|Assumption/.test(e);
    const isNumberedDataType = /RelationType|OperationType|PropertyType/.test(
      e?.kind
    );
    const isCompositeType = /Struct|Enum|ArrayType|TupleType|TypeParam/.test(
      e?.kind
    );
    const isControlFlow = /IfFlow|ForFlow|WhileFlow|MatchFlow/.test(e?.kind);
    must(
      isPrimitiveType || isDataType || isNumberedDataType || isCompositeType || isControlFlow,
      "Type expected",
      at
    );
  }
  
  function isVariable(e) {
    e?.kind != null && (e.kind === "Variable" || e.kind === "Parameter")
  }

  function mustBeAssignedVar(e, at) {
    must(
      isVariable(e),
      "Variable not assigned",
      at
    );
  }

  function equivalent(t1, t2) {
    return (
      t1 === t2 ||
      (t1?.kind === t2?.kind &&
        ((t1?.kind === "ArrayType" &&
          t1?.len === t2?.len &&
          equivalent(t1.baseType, t2.baseType)) ||
          (t1?.kind === "ListType" && equivalent(t1.baseType, t2.baseType)) ||
          (t1?.kind === "ActionType" &&
            equivalent(t1.returnType, t2.returnType)) ||
          (t1?.number === t2?.number &&
            (t1?.kind === "RelationType" ||
              t1?.kind === "OperationType" ||
              t1?.kind === "RelationArgType" ||
              (t1?.kind === "PropertyType" &&
                t1?.argNumbers.every((n, i) => t2?.argNumbers[i] === n))))))
    );
  }

  function mustHaveNumber(e, n, at) {
    must(e?.number === n, "Incorrect number", at);
  }
  
  function mustHaveEqualLength(e1, e2, at) {
    must(e1?.length === e2?.length, "Expected equal length", at)
  }

  function mustHaveLength(e, n, at) {
    must(e?.length === n, "Incorrect length", at);
  }

  function isMutable(e) {
    return (
      (isVariable(e) && e?.mutable) ||
      (e?.kind === "VarField" && isMutable(e?.variable)) ||
      (e?.kind === "MethodCall" && isMutable(e?.object))
    );
  }

  function mustBeMutable(e, at) {
    must(isMutable(e), "Cannot assign to immutable variable", at);
  }

  function typeDescription(type) {
    if (typeof type === "string") return type;
    if (type.kind === "Struct" || type.kind === "Enum") return type.name;
    if (type.kind === "ArrayType") return `[${typeDescription(type.baseType)}]`;
    if (type.kind === "ListType")
      return `List[${typeDescription(type.baseType)}]`;
  }

  function mustBeAssignable(e, { toType: type }, at) {
    must(false, "Tbd", at);
    const source = typeDescription(e.type);
    const target = typeDescription(type);
    const message = `Cannot assign a ${source} to a ${target}`;
    must(assignable(e.type, type), message, at);
  }

  function mustBeDistinct(list, message, at) {
    const listSet = new Set(list.map((x) => x.name));
    must(listSet.size === list.length, message, at);
  }

  function mustHaveDistinctFields(type, at) {
    mustBeDistinct(type.fields, "Fields must be distinct", at);
  }

  function mustHaveDistinctModules(classs, at) {
    mustBeDistinct(classs.modules, "Modules must be distinct", at);
  }

  function checkFieldMap(typeFields, classFields) {
    return classFields.every(
      (field, _i) =>
        typeFields.find((f) => f.name === field.name)?.type === field.type
    );
  }
  
  // TODO: Update this for FilledStruct or FilledClass to impl FilledClass
  function mustMapFields(struct, classs, at) {
    must(checkFieldMap(struct.fields, classs.fields), "Fields do not map", at);
  }

  function mustHaveField(type, field, at) {
    must(
      type.fields.some(
        (typeField) =>
          typeField.name === field.name && typeField.type === field.type,
        "No such field",
        at
      )
    );
  }

  function mustHaveModule(type, module, at) {
    must(
      type.modules.some(
        (typeMod) =>
          typeMod.name === module.name && equivalent(typeMod.type, module.type),
        "No such method",
        at
      )
    );
  }

  function mustImplAllModules(classImpl, classs, at) {
    for (const [i, mod] of classs.modules.filter((m) => m.body != null)) {
      mustHaveModule(classImpl, mod, at);
    }
  }

  function includesTypeArg(filledClass, typeName) {
    return filledClass.typeArgs.some(
      (validType) =>
        validType?.name === typeName ||
        (validType?.kind === "FilledClass" &&
          includesTypeArg(validType, typeName))
    );
  }

  function mustBeImplementable(validType, filledClass, at) {
    if (validType == null) { // Future
      return;
    }
    
    let userDefinedMsg =
      "Only user-defined types (struct, enum, class) can implement";
    must(typeof validType.name !== "undefined", userDefinedMsg, at);
    must(
      validType.name === "Class" ||
        validType.name === "Struct" ||
        validType.name === "Enum",
      userDefinedMsg,
      at
    );
    
    // For struct S and class C, cannot: S impl C<S>
    must(
      !includesTypeArg(filledClass, validType.name),
      "Cannot include self in type parameters",
      at
    );
  }

  function mustBeInLoop(at) {
    must(context.inLoop, "Break can only appear in a loop", at);
  }

  function mustBeInAFunction(at) {
    must(context.module, "Return can only appear in a function", at);
  }
  
  function mustBeAType(t, at) {
    must(t.isType != null && t.isType === true, "Must be a type", at);
  }

  // Up to here
  // reconsider enum cases

// UNUSED
  function mustBeCallable(e, at) {
    const callable =
      e?.kind === "FilledStruct" || e?.kind === "FilledClass" || e.type?.kind === "FunctionType";
    must(callable, "Call of non-function or non-constructor", at);
  }

  function mustNotReturnAnything(f, at) {
    const returnsNothing = f.type.returnType === core.voidType;
    must(returnsNothing, "Something should be returned", at);
  }

  function mustReturnSomething(f, at) {
    const returnsSomething = f.type.returnType !== core.voidType;
    must(returnsSomething, "Cannot return a value from this function", at);
  }

  function mustBeReturnable(e, { from: f }, at) {
    mustBeAssignable(e, { toType: f.type.returnType }, at);
  }

  function mustHaveCorrectArgumentCount(argCount, paramCount, at) {
    const message = `${paramCount} argument(s) required but ${argCount} passed`;
    must(argCount === paramCount, message, at);
  }

  // BUILDER
  // Before .map(), use .children if + or *
  // If list, use .asIteration.children

  const builder = match.matcher.grammar.createSemantics().addOperation("rep", {
    Program(sections) {
      return core.program(sections.children.map((s) => s.rep()));
    },
    
    // Readables
    Construct(filledStruct, _brack1, readables, _brack2) {
      filledStructRep = filledStruct.rep();
      readablesRep = readables.asIteration().children.map(a => a.rep());
      
      if (filledStructRep.inner.fields == null) {
        // Future
      } else {
        mustHaveEqualLength(readablesRep, filledStructRep.inner.fields, { at: filledStruct });
      }
      
      return core.construct(filledStructRep, readablesRep)
    },
        // TODO Literal?
    
    FullArray(_left, readables, _right) {
      const contents = readables.asIteration().children.map(r => r.rep());
      if (contents.length === 0) {
        return core.emptyArray();
      } else {
        return core.logosArray(contents);
      }
    },
    
    CopiedArray(_left, readable, _colon, length, _right) {
      let len = length.rep();
      if isVariable(len) {
        mustBeInteger(len);
        len = len.contents;
      }
      const contents = Array(len).fill(readable.rep());
      return core.logosArray(contents);
    },
    

    // Data
    ValueDecl(_value, id, _colon, relationId) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });
      const value = core.value(idStr);
      context.add(idStr, value);

      if (relationId != null) {
        const relation = relationId.rep();
        mustHaveNumber(relation.kind, 1, { at: id });
        context.addStatement(core.filledRelation(relation, [value]), true);
      }

      return core.valueDeclaration(value);
    },

    RelationDecl(id, _colon1, args, _colon2, statement) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      const idStr = id.sourceString;

      const relation = core.relation(idStr, args, null);
      // Add immediately so that we can have recursion
      context.add(idStr, relation);

      // Parameters are part of the child context
      context = context.newChildContext({
        inLoop: false,
        inData: true,
        module: relation,
      });
      for (const arg in args.asIteration().children) {
        const argStr = arg.sourceString;
        mustNotAlreadyBeDeclared(argStr, { at: arg } );
        const argValue = core.value(argStr);
        context.add(argStr, argValue);
        relation.args.push(argValue);
      }

      // Analyze body while still in child context
      relation.statement = statement.rep();

      // Go back up to the outer context before returning
      context = context.parent;
      return core.relationDeclaration(relation);
    },

    OperationDecl(_op, id, _colon1, args, _colon2, statement) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      id = id.sourceString;

      const operation = core.operation(id, args, null);
      // Add immediately so that we can have recursion
      context.add(id, operation);

      // Parameters are part of the child context
      context = context.newChildContext({
        inLoop: false,
        inData: true,
        module: operation,
      });
      for (const arg in args.asIteration().children) {
        const argStr = arg.sourceString;
        mustNotAlreadyBeDeclared(argStr, { at: arg } );
        const argStatement = core.statement(argStr, null);
        context.add(argStr, argStatement);
        relation.args.push(argStatement);
      }

      // Analyze body while still in child context
      operation.statement = statement.rep();

      // Go back up to the outer context before returning
      context = context.parent;
      return core.operationDeclaration(operation);
    },

    PropertyDecl(_prop, id, _colon1, args, _colon2, statement) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      id = id.sourceString;

      const property = core.property(id, args, null);
      // Add immediately so that we can have recursion
      context.add(id, property);

      // Parameters are part of the child context
      let context = context.newChildContext({
        inLoop: false,
        inData: true,
        module: property,
      });
      property.args = args.asIteration().children.map(rel => rel.rep());

      // Analyze body while still in child context
      property.statement = statement.rep();

      // Go back up to the outer context before returning
      context = context.parent;
      return core.propertyDeclaration(property);
    },

    ArgRelation(id, numbering) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      id = id.sourceString;

      number = numbering.rep();

      const relation = core.relation(id, new Array(number), null);
      context.add(id, relation);
      return relation;
    },

    Numbering(_leftArrow, number, _rightArrow) {
      mustBeInteger(number.sourceString, { at: number });

      return Number(number.sourceString);
    },

    StatementDecl(id, _colon, body) {
      const idStr = id.sourceString;
      const bodyRep = body.rep();
      mustBeStatement(bodyRep, { at: id });
      mustNotAlreadyBeDeclared(idStr, { at: id });

      const statement = core.statement(idStr, bodyRep);
      context.add(idStr, statement);
      return core.statementDeclaration(statement);
    },

    InfixDecl(_infix, id, operation) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      id = id.sourceString;

      const infix = core.infix(id, operation);
      context.add(id, infix);
      return core.infixDeclaration(infix);
    },

    Assume(_assume, id, truth, _colon, body) {
      mustBeBoolean(truth, { at: body });
      const statement = body.rep();
      context.addStatement(statement, truth);

      if (id != null) {
        mustNotAlreadyBeDeclared(id.sourceString, { at: id });
        const id = id.sourceString;

        const named = core.statement(id, statement);
        context.add(id, named);
        return core.assumptionDeclaration(named, truth);
      }
    },
    
    DataVar(id) {
      const variable = id.rep();
      if isVariable(variable, { at: id }) {
        return variable;
      }
      
      const dataVar = context.lookup(id.sourceString);
      if (dataVar == null) {
        must(false, "DataVar not found", { at: id });
      }
      
      return rep;
    },

    Equality(v1, _equals, v2) {
      const v1Rep = v1.rep();
      const v2Rep = v2.rep();
      mustBeKindK(v1Rep, "Value", { at: v1 });
      mustBeKindK(v2Rep, "Value", { at: v2 });
      return core.equalityStatement(v1Rep, v2Rep);
    },
    FilledInfix(s1, inf, s2) {
      const statement1 = s1.rep();
      const statement2 = s2.rep();
      const infix = inf.rep();
      return core.filledOperation(infix?.operation, [statement1, statement2]);
    },
    defaultInfix(id) {
      const idStr = id.sourceString;
      if (idStr === "and") {
        return core.standardLibrary["and"];
      } else if (idStr === "or") {
        return core.standardLibrary["or"];
      } else if (idStr === "==") {
        return core.standardLibrary["=="];
      } else {
        must(false, "No default infix - SHOULD have been caught by OHM", { at: id });
      }
    },
    UserDefinedInfix(id) {
      const idStr = id.sourceString;
      const infix = context.lookup(idStr);
      if (infix == null) {
        must(false, "Not an infix", { at: id });
      }
      return infix;
    },
    
    FilledOperation(id, _leftBracket, statements, _rightBracket) {
      const operation = id.rep();
      const statementsRep = statements.rep();
      return core.filledOperation(operation, statementsRep);
    },
    FilledRelationOrProp(relOrProp, _leftBracket, args, _rightBracket) {
      const subject = relOrProp.rep();
      
      if (subject?.kind?.kind === "RelationType") {
        const argsRep = [];
        for (const arg in args.asIteration().children) {
          const value = arg.rep();
          mustBeKindK(value, "Value", {at: arg});
          argsRep.push(value);
        }
        mustHaveLength(argsRep, subject.kind.number, {at: args});
        return core.filledRelation(subject, argsRep);
        
      } else if (subject?.kind?.kind === "PropertyType") {
        const argsRep = [];
        for (const arg in args.asIteration().children) {
          const relation = arg.rep();
          mustBeKindK(relation, "Relation", {at: arg});
          argsRep.push(relation);
        }
        mustHaveLength(argsRep, subject.kind.number, {at: args});
        return core.filledProperty(subject, argsRep);
      
      } else {
        must(false, "Not a relation or property", { at: id });
      }
    },
    
    Infix(i) {
      return i.rep();
    },
    AssignedVariable(name) {
      const variable = context.lookup(name.sourceString);
      mustBeAssignedVar(variable, { at: name });
      return variable;
    },
    
    
    CustomType(id) {
      const idStr = id.sourceString;
      let type = context.lookup(idStr);
      
      if type == null {
        type = core.customType(idStr);
      }
      return type;
    },
    FilledCustomType(customType, typeArgs) {
      const customTypeRep = customType.rep();
      context = context.newChildContext({inLoop: false, inData: false, module: customTypeRep.typeParams});
      let typeArgsRep = typeArgs.rep();
      context = context.parent;
      if typeArgsRep == null {
        typeArgsRep = [];
      }
      
      if (customTypeRep.kind !== "FilledType") {
        mustHaveEqualLength(customTypeRep.typeParams, typeArgsRep, { at: typeArgs });
        for (const [index, typeParam] in customTypeRep.typeParams.entries()) {
          for (const className in typeParam.classes.keys()) {
            mustHaveClass(typeArgsRep[index], className, { at: typeArgs });
          }
        }
      }
      
      return core.filledType(customTypeRep, typeArgsRep);
    },

    VarField(id, _dot, fieldId) {
      let idStr = id.sourceString;
      let fieldStr = fieldId.sourceString; // .trim()?
      
      if (idStr[0] === idStr[0].toUpperCase()) {
        // Must be an enum
        const enumeration = context.lookup(idStr);
        const newCase = core.field(fieldStr, null);
        
        if (enumeration == null) { // Future
          const newType = core.customType(idStr);
          newType.fields.push(newField);
          
          context.add(idStr, newType);
          
          return newType;
        }
        
        if (enumeration?.kind == null) {
          must(false, "ERROR: No kind attribute", { at: id });
        }
        
        if (enumeration.kind === "CustomType") {
          enumeration.cases.push(newCase);
          return core.enumCase(enumeration, newCase);
        }
        
        if (enumeration.kind === "Enum") {
          const casee = enumeration.cases.find((c) => c.name === fieldStr);
          if (casee == null) {
            must(false, "No such case", { at: id } );
          }
          return core.enumCase(enumeration, casee);
        }
        
        
      } else {
        // Must be a variable
        // No you can't do [a,b,c].0
        const variable = context.lookup(idStr);
        mustBeAssignedVar(variable, { at: id });
        
        if (variable.type?.kind === "FilledType") {
          let field = variable.type.inner?.fields.find((f) => f.name === fieldStr);
          if (variable.type?.inner === "Struct") {
            if (field == null) {
              must(false, "No such field", { at: id});
            }
            
          } else if variable.type?.inner === "CustomType" {
            // Future
            if (field == null) {
              field = core.field(fieldStr, null);	// TODO HOOKMAX WE LOST TYPE CHECKING
            }
            
          } else {
            must(false, "Class or enum has no fields", { at: id });
          }
          return core.varField(variable, field);
        } else {
          // Must be list index
          //mustHaveClass(variable, "Collection", { at: id });
          const collectionClass = context.getClass("Collection", variable.type);
          if (collectionClass == null) {
            must(false, "Expected type to be collection", { at: id });
          }
          mustHaveLength(collectionClass.typeArgs, 1, { at: id });
          const baseType = collectionClass.typeArgs[0];
          
          const number = Number(fieldStr);
          must(Number.isInteger(number), "List index is not numeric", { at: id });
          
          return core.varIndex(variable, number, baseType);
        }
      }
    },

    // Section: Struct, enum, types
    TypeArgs(_leftAngle, types, _rightAngle) {
      const typeParams = context.module;
      const typesList = types.asIteration().children;
      
      if (typeParams.length > 0 && typesParams[0] == null) {
        // Future: Struct, enum, or class has not been defined yet
        // we assume it will be later
        const typesRep = typesList.map(t => t.rep());
        
        if (typeParams.length > 1) {
          mustHaveEqualLength(typesRep, typeParams[1], {at: types});
        }
        
        typeParams.push(typesRep);
        return typesRep;
      }
      
      must(
        Array.isArray(typeParams) && typeParams.every(item => 
          typeof item === "object" &&
          item !== null &&
          item.kind === "TypeParameter"
        ),
        "Expected context to be TypeParameters",
        { at: types },
      );
      
      let typesList = types.asIteration().children;
      
      let typesRep = new Array();
      for (const [index, type] in typesList.entries()) {
        let typeRep = type.rep();
        
        // Check at end?
        for (const classRequired of typeParams[index].classes) {
           mustHaveClass(typeRep, classRequired.name, { at: types });
        }
        
        typesRep.push(typeRep);
      }
      mustHaveEqualLength(typesRep, typeParams, { at: types });
      return typesRep;
    },
    SuperClass(_colon, classes) {
      const superclasses = [];
      for classs in classes.asIteration().children {
        const classRep = classs.rep();
        mustBeKindK(classRep, "FilledType", { at: classs});
        mustBeKindK(classRep.inner, "Class", { at: classs});
        superclasses.push(classRep);
      }
      return superclasses;
    },
    IdAndSuperClass(aTypeParam, superclasses) {
      const typeParam = aTypeParam.rep();
      for (const classs in superclasses.rep()) {
        typeParam.classes.add(classs);
      }
      return typeParam;
    },
    TypeParams(_leftAngle, typeParamList, _rightAngle) {
      return typeParamList.asIteration().children.map((t) => t.rep());
    },

    Param(id, _colon, type) {
      let idStr = "0";
      if (id != null) {
        idStr = id.sourceString;
      }
      return core.field(idStr, type.rep());
    },

    // Types can be declared out of order, so check what's already in the database
    StructDecl(
      _struct,
      id,
      typeParameters,
      _leftBracket,
      parameters,
      _rightBracket
    ) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });

      // can be null
      const typeParamsRep = typeParameters.rep();

      // Allow recursion (but not for the type params)
      const struct = core.struct(idStr, typeParamsRep, []);
      context.add(idStr, struct);

      // Analyze parameters with typeParams
      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: struct,
      });
      context.add("Self", struct);
      for (const typeParam in typeParamsRep) {
        // Already checked mustNotAlreadyBeDeclared in IdAndSuperClass
        context.add(typeParam.name, typeParam);
      }

      const params = parameters.asIteration().children.map((p) => p.rep());
      let i = 0;
      for (const p in params) {
        if (p.name === "0") {
          p.name = i.toString();
          i++;
        }
      }
      struct.fields = params;
      mustHaveDistinctFields(struct, { at: id });
      mustNotBeSelfContaining(struct, { at: id });

      context = context.parent;
      return core.structDeclaration(struct);
    },

    EnumDecl(
      _enum,
      id,
      typeParameters,
      _leftBracket,
      parameters,
      _rightBracket
    ) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });

      // can be null
      const typeParamsRep = typeParameters.rep();

      const enumeration = core.enumeration(idStr, typeParamsRep, []);
      context.add(idStr, enumeration);

      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: enumeration,
      });
      context.add("Self", enumeration);
      for (const typeParam in typeParamsRep) {
        context.add(typeParam.name, typeParam);
      }

      const params = parameters.asIteration().children.map((p) => p.rep());
      let i = 0;
      for (const p in params) {
        if (p.name === "0") {
          p.name = i.toString();
          i++;
        }
      }
      enumeration.cases = params;
      mustHaveDistinctFields(enumeration, { at: id });
      mustNotBeSelfContaining(enumeration, { at: id });

      context = context.parent;
      return core.enumDeclaration(enumeration);
    },

    // Section: Methods, classes

    SelfParam(mut, _self) {
      let mutable = true;
      if (mut == null) {
        mutable = false;
      }
      return core.parameter("self", mutable, null); // change type to Self later
    },
    ModParam(id, _colon, mut, type) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });
      let mutable = true;
      if (mut == null) {
        mutable = false;
      }
      return core.parameter(idStr, mutable, type.rep());
    },
    ModHead(typeParam, _paren1, params, _paren2, returnType) {
      let typeParamRep = [];
      if (typeParam != null) {
        typeParamRep = typeParam.rep();
      }
      const paramsRep = params.asIteration().children.map((p) => p.rep());

      let returnTypeRep = core.voidType;
      if (returnType != null) {
        returnTypeRep = returnType.rep();
      }

      const mod = core.module(
        null,
        typeParamRep,
        paramsRep,
        returnTypeRep,
        null
      );
      return mod;
    },

    _iter(...children) {  // Explicit `_iter` handler
      return children.map(child => child.rep());
    }, 
    _terminal() {  // Explicitly handle terminal nodes
      return this.sourceString;  // Return the raw string value
    },
    ModBody(_brack1, actionLines, _brack2) {
      return actionLines.children.map((a) => a.rep());
    },
    ModDecl(_mod, id, head, body) {
      let idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });
      let mod = head.rep();
      mod.name = idStr;
      context.add(idStr, mod); // Allow recursion

      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: mod,
      });
      for (let param in mod.params) {
        must(param.name != "self", "No self parameter", { at: id });
        context.add(param.name, param);
      }
      for (let typeParam in mod.typeParams) {
        context.add(typeParam.name, typeParam);
      }
      let bodyRep = body.rep();
      mod.body = bodyRep;

      context = context.parent;
      return core.moduleDeclaration(mod);
    },
    
    ATypeParam(id) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, {at: id});
      return core.typeParameter(idStr);
    },
    TypeArgsOrParams(list) {
      return list.asIteration().children.map(t => t.rep());
    },
    MethodType(customType, typeArgsOrParams) {
      const struct = customType.rep();
      mustBeKindK(struct, "Struct", { at: customType });
      if typeArgsOrParams == null {
        // Must be struct with 0 type-args
        mustHaveLength(struct.typeParams, 0, { at: typeArgsOrParams });
        
        return core.filledType(struct, []);
      } else {
        const typesRep = typeArgsOrParams.rep();
        if typesRep[0].kind === "TypeParameter" {
          mustHaveEqualLength(struct.typeParams, typesRep, { at: typeArgsOrParams });
          // every typeParam in typesRep should have no classes, but this should be caught by parser
          
          return struct;
        } else {
          //must(typesRep.kind === "", { at: customType }); // Redundant - should be caught by parser
          // Type is ValidType
          mustHaveEqualLength(struct.typeParams, typesRep, {at: typeArgsOrParams});
          
          return core.filledType(struct, typesRep);
        }
      }
    },

    MethodDecl(_mod, methodType, typeParams, _dot, id, head, body) {
	// Here:
	// you can have mod Struct<T> for type parameter T
	// and can have mod Struct<bool>
	// Yet you cannot have mod Struct<T: Class>
      const struct = methodType.rep();
      const idStr = id.sourceString;
      const contextualName = struct.name + "." + idStr;
      mustNotAlreadyBeDeclared(contextualName, { at: id });
      context.add(contextualName, method); // Allow recursion
      
      
      if struct.kind === "Struct" {
        // TODO: how should unfilled struct be treated?
        for (const typeParam in struct.typeParams) {
          mustNotAlreadyBeDeclared(param.name, { at: head });
          context.add(typeParam.name, typeParam);
        }
        
        return;
      }
      // else: FilledType
      mustBeKindK(struct, "FilledType", { at: methodType });
      
      const mod = head.rep();
      mod.name = idStr;
      struct.methods.push(mod);

      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: mod,
      });
      context.add("Self", struct); // Self type
      for (const param in mod.params) {
        if (param.name === "self") {
          param.type = struct;
        }
        mustNotAlreadyBeDeclared(param.name, { at: head });
        context.add(param.name, param);
      }
      for (const typeParam in mod.typeParams) {
        mustNotAlreadyBeDeclared(param.name, { at: head });
        context.add(typeParam.name, typeParam);
      }
      const bodyRep = body.rep();
      mod.body = bodyRep;

      context = context.parent;
      return core.methodDeclaration(struct, mod);
    },

    // Classes
    ClassMod(_mod, id, head, body) {
      // TODO: Context.module can be either classdecl or classimpl; handle all cases
      // Note: This is one case where Self can be an unfilled type: just Struct
      
      const idStr = id.sourceString;
      const classs = context.module;
      
      const className = getTypeName(classs);
      mustNotAlreadyBeDeclared(idStr, { at: id });

      const mod = head.rep();
      mod.name = idStr;

      classs.modules.push(mod); // Recursion

      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: mod,
      });
      context.add("Self", struct); // Self type
      for (const param in mod.params) {
        if (param.name === "self") {
          param.type = struct;
        }
        context.add(param.name, param);
      }
      for (const typeParam in mod.typeParams) {
        context.add(typeParam.name, typeParam);
      }

      let bodyRep = null;
      if (body.sourceString !== ";") {
        bodyRep = body.rep();
      }
      mod.body = bodyRep;

      context = context.parent;
      return core.moduleDeclaration(mod);
    },
    
    ClassBody(_left, classMods, _right) {
      const mods = new Array();
      for (const module in classMods.children) {
        const mod = module.Rep();
        const contextualName = context.module.name + "." + mod.name; // context.mod can be class or classImpl
        mustNotAlreadyBeDeclared(contextualName, { at: classMods });
        context.add(contextualName, mod);
      }
      mustBeDistinct(mods, "Modules must be distinct", { at: classMods });
      return mods
    },

    ClassDecl(_class, id, typeParameters, superClass, classBody) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });

      // can be null
      const typeParamsRep = typeParameters.rep();

      const classs = core.classs(idStr, typeParamsRep, [], []);
      context.add(idStr, classs);

      // can be null
      const superClassRep = superClass.rep();
      for (superClass in superClassRep) {
        context.classify(superClass, classs);
      }

      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: classs,
      });
      context.add("Self", classs);
      for (const typeParam in typeParamsRep) {
        context.add(typeParam.name, typeParam);
      }
      
      // TODO: Check future?
      classs.modules = classBody.rep();
      //mustHaveDistinctModules(classs, { at: id });

      context = context.parent;
      return core.classDeclaration(classs);
    },

    ClassImpl(type, _impl, filledClass, body) {
      const typeRep = type.rep(); // TODO future: May be null
      const classRep = filledClass.rep(); // TODO future: May be null
      mustHaveKindK(classRep.inner, "Class", { at: filledClass });
      
      const impl = core.classImpl(typeRep, classRep, []);
      
      const contextualName = typeRep.name + ".impl." + core.getTypeName(classRep);
      mustNotAlreadyBeDeclared(contextualName, { at: type });
      context.add(contextualName, impl);

      context.classify(typeRep, classRep);

      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: impl,
      });
      context.add("Self", classRep);
      
      impl.modules = body.rep();
      
      mustHaveDistinctModules(impl, { at: type });
      mustImplAllModules(impl, classRep, { at: type });

      context = context.parent;
      return impl;
    },

    // Actions
    // TODO Check and redo

    ActionLine(action, _semicolon) {
      return action.rep();
    },

    Assignment(id, _eq, expr) {
      idStr = id.sourceString;
      exprRep = expr.rep();
      mustBeObject(exprRep, { at: id});
      const variable = context.lookup(idStr);
      if (variable == null) {
        newVar = core.variable(idStr, exprRep.type, exprRep);
        context.add(idStr,newVar);
        return core.variableDeclaration(newVar);
      } else {
        if (variable.type !== exprRep.type) {
          newVar = core.variable(idStr, exprRep.type, exprRep);
          context.add(idStr, newVar);
          return core.variableDeclaration(newVar);
        }
        
        return core.assignment(variable, exprRep);
      }
    },
    
    Call(_left, args, _right) {
      return args.asIteration().children.map(a => a.rep());
    }

    MethodCall(varOrType, _dot, methodId, args) {
      const subjectRep = varOrType.rep();
      if isVariable(subjectRep) {
        return
      
      } else if subjectRep?.kind === "FilledType" {
        const methodName = subjectRep.inner.name + methodId.sourceString; // hook
        
        
      } else {
        must(false, "Cannot call this object", { at: varOrType });
      }
      const method = 
      
      
      const idStr = id.sourceString;
      const variable = context.lookup(
      const method = context.lookup(id.sourceString);
      //mustBeCallable(method, { at: id });
      // TODO Either the method exists, or a Future
      mustHaveCorrectArgumentCount(args.children.length, method.params.length, {
        at: id,
      });
      
      // TODO: Fix MustBeAssignable
      args.children.forEach((arg, i) => {
        mustBeAssignable(
          arg.rep(),
          { toType: method.params[i].type },
          { at: arg }
        );
      });
      return core.methodCall(
        
        method,
        args.children.map((arg) => arg.rep())
      );
    },

    ModCall(id, args) {
      const idStr = id.sourceString;
      const module = context.lookup(idStr);
      must(module?.kind != null && module?.kind === "Module", "Expected a module", { at: id });
      for arg in args.children {
        const argRep = arg.rep();
        mustHaveKindK(argRep, 
      }
      const argsRep = args.rep();
      
      
      const methodCall = MethodCall();
      return core.modCall(module, methodCall);
    },

    Return(returnOrYield, expr) {
      mustBeInAFunction({ at: _return });
      const functionContext = context.module;
      mustBeReturnable(expr.rep(), { from: functionContext }, { at: expr });
      return core.returnLine(expr.rep());
    },

    Increment(id, _plus) {
      const variable = context.lookup(id.sourceString);
      mustBeInteger(variable, { at: id });
      mustBeMutable(variable, { at: id });
      return core.incrementVar(variable);
    },

    Decrement(id, _minus) {
      const variable = context.lookup(id.sourceString);
      mustBeInteger(variable, { at: id });
      mustBeMutable(variable, { at: id });
      return core.decrementVar(variable);
    },
    
    none(_) {
      return core.nullObject();
    },
    true(_) {
      return true;
    },
    false(_) {
      return false;
    },
    number(_digits) {
      return Number(this.sourceString);
    },
  });

  return builder(match).rep();
}
