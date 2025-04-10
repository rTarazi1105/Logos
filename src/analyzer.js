// Note on comments: "future" refers to a type that has been called but not defined (declared) yet

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

  addStatement(statement, truth) {
    if (statement?.statement != true) {
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
        throw new Error(`Contradiction in data: ${statement.name}`);
      } else {
        console.log(`Statement ${statement.name} redundant`);
        return;
      }
    }

    this.locals.set(statement, truth);
  }

  // Collection, Equatable, Comparable, Error
  // classes: Map<type, Map<string,class>>
  
  // Why are classes indexed by string?
  // We don't have Struct<T> impl Class<T> thankfully
  // If Struct impl Class<A>, Struct cannot impl Class<B>
  
  classify(filledClass, type) {
    if (!this.classes.containsKey(type)) {
      this.classes.set(type, new Map());
    }
    
    const typeEntry = this.classes.get(type);
    if 
    this.classes.get(type).add(filledClass);
  }
  
  lookupClass(className, type) {
    let superClasses = this.classes.get(type);
    if (type.kind === "TypeParameter") {
      superClasses = type.classes;
    }
    
    for (let filledClass of superClasses) {
      if (filledClass.classs.name === className) {
        return true;
      }
      if lookupClass(className, filledClass) {
        return true;
      }
    }
    
    // Don't look in parent - it should be the same
    // this.parent.lookupClass(className, type)
    
    return false;
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

  function mustBeInteger(e, at) {
    must(e.type === core.intType, "Expected an integer", at);
  }

  function mustBeBoolean(e, at) {
    must(e.type === core.booleanType, "Expected a boolean", at);
  }

  function mustBeVoid(e, at) {
    must(e.type === core.voidType, "Expected void", at);
  }

  function mustBeStatement(e, at) {
    must(
      e?.statement === true || e.kind === core.booleanType,
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
      e.type?.kind === "Struct" || e.type?.kind === "Classs",
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

  function mustBeAssignedVar(e, at) {
    must(
      e?.kind === "Variable" || e?.kind === "Parameter",
      "Variable not assigned",
      at
    );
  }

  function mustBeAssignable(e, at) {
    // TODO
    must(false, "Tbd", at);
  }

  function equivalent(t1, t2) {
    return (
      t1 === t2 ||
      (t1?.kind == t2?.kind &&
        ((t1?.kind === "ArrayType" &&
          t1?.len === t2?.len &&
          equivalent(t1.baseType, t2.baseType)) ||
          (t1?.kind === "ListType" && equivalent(t1.baseType, t2.baseType)) ||
          (t1?.kind === "ActionType" &&
            equivalent(t1.returnType, t2.returnType)) ||
          (t1?.number == t2?.number &&
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

  // TODO: Error: ModCall return type can be mutable, but shouldn't be
  function isMutable(e) {
    return (
      (e?.kind === "Variable" && e?.mutable) ||
      (e?.kind === "VarField" && isMutable(e?.variable)) ||
      (e?.kind === "MethodCall" && isMutable(e?.object))
    );
  }

  function mustBeMutable(e, at) {
    must(isMutable(e), "Cannot assign to immutable variable", at);
  }

  function typeDescription(type) {
    if (typeof type === "string") return type;
    if (type.kind == "Struct" || type.kind == "Enum") return type.name;
    if (type.kind == "ArrayType") return `[${typeDescription(type.baseType)}]`;
    if (type.kind == "ListType")
      return `List[${typeDescription(type.baseType)}]`;
  }

  function mustBeAssignable(e, { toType: type }, at) {
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
    for (let [i, mod] of classs.modules.filter((m) => m.body != null)) {
      mustHaveModule(classImpl, mod, at);
    }
  }

  function includesAsFilledTypeParam(filledClass, typeName) {
    return filledClass.filledTypeParams.some(
      (validType) =>
        validType?.name === typeName ||
        (validType?.kind === "FilledClass" &&
          includesAsFilledTypeParam(validType, typeName))
    );
  }

  function mustBeImplementable(validType, filledClass, at) {
    let userDefinedMsg =
      "Only user-defined types (struct, enum, class) can implement";
    must(typeof validType.name !== "undefined", userDefinedMsg, at);
    must(
      validType.name !== "Class" ||
        validType.name !== "Struct" ||
        validType.name !== "Enum",
      userDefinedMsg,
      at
    );

    must(
      !includesAsFilledTypeParam(filledClass, validType.name),
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
    
    // Assignables
    Construct(filledStruct, _brack1, assignables, _brack2) {
      filledStructRep = filledStruct.rep();
      assignablesRep = assignables.asIteration().children.map(a => a.rep());
      
      if filledStructRep.struct.fields == null {
        // Future
      } else {
        mustHaveLength(assignablesRep, filledStructRep.struct.fields.length, { at: filledStruct });
        
      }
      
      return core.construct(filledStructRep, assignablesRep)
    },
        // Literal?
    
    
    

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
      relation.args = args.rep();

      // Analyze body while still in child context
      relation.statement = statement.rep();

      // Go back up to the outer context before returning
      context = context.parent;
      return core.relationDeclaration(relation);
    },

    ArgValue(id) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      const idStr = id.sourceString;

      const value = core.value(idStr);
      context.add(idStr, value);
      return value;
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
      operation.args = args.rep();

      // Analyze body while still in child context
      operation.statement = statement.rep();

      // Go back up to the outer context before returning
      context = context.parent;
      return core.operationDeclaration(operation);
    },

    ArgStatement(id) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      id = id.sourceString;

      const statement = core.statement(id, null);
      context.add(id, statement);
      return statement;
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
      property.args = args.rep();

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
      body = body.rep();
      mustBeStatement(body, { at: id });
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      id = id.sourceString;

      const named = core.statement(id, body);
      context.add(id, named);
      return core.statementDeclaration(named);
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

    Equality(v1, _equals, v2) {
      return core.equalityStatement(v1.rep(), v2.rep());
    },
    FilledInfix(s1, inf, s2) {
      const statement1 = s1.rep();
      const statement2 = s2.rep();
      const infix = inf.rep();
      return core.filledOperation(infix?.operation, [statement1, statement2]);
    },
    FilledOperation(id, _leftBracket, statements, _rightBracket) {
      const operation = id.rep();
      const statementsRep = statements.rep();
      return core.filledOperation(operation, statementsRep);
    },
    FilledRelation(id, _leftBracket, values, _rightBracket) {
      const relation = id.rep();
      const valuesRep = values.rep();
      return core.filledRelation(relation, valuesRep);
    },
    FilledProperty(id, _leftBracket, relations, _rightBracket) {
      const property = id.rep();
      const relationsRep = relations.rep();
      return core.filledProperty(property, relationsRep);
    },

    DeclaredStatement(s) {
      const statement = context.lookup(s.sourceString);
      mustBeStatement(statement, { at: s });
      return statement;
    },
    Relation(r) {
      const relation = context.lookup(r.sourceString);
      mustBeKindK(relation?.kind, "RelationType", { at: r });
      return relation;
    },
    Operation(o) {
      const operation = context.lookup(o.sourceString);
      mustBeKindK(operation?.kind, "OperationType", { at: o });
      return operation;
    },
    Property(p) {
      const property = context.lookup(p.sourceString);
      mustBeKindK(property?.kind, "PropertyType", { at: p });
      return property;
    },
    Value(v) {
      const value = context.lookup(v.sourceString);
      mustBeKindK(value, "Value", { at: v });
      return value;
    },
    Infix(i) {
      const infix = context.lookup(i.sourceString);
      mustBeKindK(infix, "Infix", { at: i });
      return infix;
    },
    AssignedVariable(name) {
      const variable = context.lookup(name.sourceString);
      mustBeAssignedVar(variable, { at: name });
      return variable;
    },

    DeclaredClass(id) {
      // Lookup local is ok, they can only be declared globally
      // Though if not found, assume it's declared later
      const classs = context.lookup(id.sourceString);
      if (classs == null) {
        const id = id.sourceString;
        // Last two null show that it's assumed before declaration
        // But empty list should remain
        const classs = core.classs(id, null, null, null); // Future
        context.add(id, classs);
      }
      return classs;
    },
    Struct(id) {
      const idStr = id.sourceString;
      let struct = context.lookup(idStr);
      if (struct == null) {
        struct = core.struct(idStr, null, null); // Future
        context.add(idStr, struct);
      }
      return struct;
    },
    Enum(id) {
      const idStr = id.sourceString;
      let enumeration = context.lookup(idStr);
      if (enumeration == null) {
        enumeration = core.enumeration(idStr, null, null); // Future
        context.add(idStr, struct);
      }
      return enumeration;
    },
    FilledStruct(id, filledTypeParams) {
      const struct = id.rep();
      context = context.newChildContext({inLoop: false, inData: false, module: struct.typeParams});
      const filledTypeParamsRep = filledTypeParams.rep();
      context = context.parent;
      return core.filledStruct(struct, filledTypeParamsRep);
    },
    FilledEnum(id, filledTypeParams) {
      const enumeration = id.rep();
      context = context.newChildContext({inLoop: false, inData: false, module: enumeration.typeParams});
      const filledTypeParamsRep = filledTypeParams.rep();
      context = context.parent;
      return core.filledEnum(enumeration, filledTypeParamsRep);
    },

    VarField(id, _dot, fieldId) {
      let idStr = id.sourceString;
      let fieldStr = fieldId.sourceString; // .trim()?
      
      if (word[0] === word[0].toUpperCase()) {
        // Must be an enum
        const enumeration = context.lookup(idStr);
        
        if (enumeration == null) { // Future
          const newCase = core.field(fieldStr, null);
          const newEnum = core.enumeration(id, null, [newCase]);
          context.add(idStr, newEnum);
          
          return core.enumCase(newEnum, newCase);
          
          
        } else {
          mustBeKindK(enumeration, "Enum", { at: id });
          
          let casee = enumeration.cases.find((c) => c.name === fieldStr);
          
          if casee == null {
            if (enumeration.typeParams == null) {
              // Future so cases are mutable
              casee = core.field(fieldStr, null);
              enumeration.cases.push(casee);
            } else {
              must(false, "No such field", { at: id });
            }
          }
          return core.enumCase(enumeration, casee);
        }
        
        
      } else {
        // Must be a variable
        // No you can't do [a,b,c].0
        const variable = context.lookup(idStr);
        mustBeKindK(variable, "Variable", { at: id });
        
        if variable.type?.kind === "FilledStruct" || variable.type?.kind === "FilledClass" {
          
          
        } else {
          // Must be list index
          mustHaveClass(variable, "Collection", { at: id });
          // TODO: ArrayType and ListType auto-impl Collection
          
          let number = Number(fieldStr);
          must(Number.isInteger(number), "List index is not numeric", { at: id });
          
          return core.varIndex(
        }
      }
      
      const variable = context.lookup(id.sourceString);
      fieldName = fieldId.sourceString;
      if (variable == null) {
        // Assume tbd enum - future
        id = id.sourceString;
        const newField = core.field(fieldName, null);
        const enumeration = core.enumeration(id, [newField]); // Future
        context.add(id, enumeration);
        
        return core.enumCase(enumeration, field);
        
      } else {
        let field = variable.fields.find((f) => f.name === fieldName);
        if (field == null) {
          if variable.kind === "Enum" { // Future
            newField = core.field(fieldName, null);
            variable.fields.push(
          }
        }
      }
      
      
      
       else {
        //mustHaveField(variable, field, { at: id } )
        const field = variable.fields.find((f) => f.name === field); // Can be an enum
        if (field == null) {
          if (variable.kind === "Enum") {
            // Future
            newField = core.field(field, null);
            enumeration.fields.push(core.field(field, null));
            field = 
          }
          
        }

        if (variable.kind === "Enum") {
          return core.enumCase(variable, field);
        } else {
          return core.varField(variable, field);
        }
      }
    },

    // Section: Struct, enum, types
    FilledTypeParams(_leftAngle, types, _rightAngle) {
      let typeParams = context.module;
      
      if typeParams == null {
        // Struct, enum, or class has not been defined yet
        // we assume it will be later
        throw new Error("TODO");
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
      mustHaveEqualLength(typeParams.length === typesList.length, { at: types });
      
      let typesRep = new Array();
      for (index, type) in typesList.entries() {
        let typeRep = type.rep();
        
        // Check at end?
        for (let classRequired of typeParams[index].classes) {
           mustHaveClass(typeRep, classRequired.name, { at: types });
        }
        
        typesRep.push(typeRep);
      }
      return typesRep;
    },
    FilledClass(declaredClass, filledTypeParams) {
      const classs = declaredClass.rep();
      
      // Use context for type parameters
      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: classs.typeParams, // Future: may be null
      });
      const params = filledTypeParams.rep();
      context = context.parent;
      
      return core.classFilledWithParam(classs, params);
    },
    SuperClass(_colon, classes) {
      return classes.asIteration().children.map((c) => c.rep());
    },
    IdAndSuperClass(id, superclasses) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      const idStr = id.sourceString;
      let typeParam = core.typeParameter(idStr);
      for (let classs in superclasses.rep()) {
        typeParam.classes.add(classs);
      }
      return typeParam;
    },
    TypeParam(_leftAngle, typeParamList, _rightAngle) {
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
      superClass,
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

      // Auto-impl superclasses
      // can be null
      const superClassRep = superClass.rep();
      for (let filledClass in superClassRep) {
        if (filledClass.classs.modules != []) {
          must(false, "Class has modules, use impl instead", { at: id });
        }
        mustMapFields(struct, filledClass.classs);
        context.classify(filledClass, struct);
      }

      // Analyze parameters with typeParams
      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: struct,
      });
      context.add("Self", struct);
      for (let typeParam in typeParamsRep) {
        // Already checked mustNotAlreadyBeDeclared in IdAndSuperClass
        context.add(typeParam.name, typeParam);
      }

      const params = parameters.asIteration().children.map((p) => p.rep());
      let i = 0;
      for (let p in params) {
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
      for (let typeParam in typeParamsRep) {
        context.add(typeParam.name, typeParam);
      }

      const params = parameters.asIteration().children.map((p) => p.rep());
      let i = 0;
      for (let p in params) {
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

    MethodDecl(_mod, structId, _dot, id, head, body) {
      let struct = context.lookup(structId.sourceString);
      mustBeKindK(struct, "Struct", { at: structId });

      let idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });
      let mod = head.rep();
      mod.name = idStr;
      struct.methods.push(mod);
      context.add(struct.name + "." + idStr, method); // Allow recursion

      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: mod,
      });
      context.add("Self", struct); // Self type
      for (let param in mod.params) {
        if (param.name === "self") {
          param.type = struct;
        }
        context.add(param.name, param);
      }
      for (let typeParam in mod.typeParams) {
        context.add(typeParam.name, typeParam);
      }
      let bodyRep = body.rep();
      mod.body = bodyRep;

      context = context.parent;
      return core.methodDeclaration(struct, mod);
    },

    // Classes
    ClassBody(_left, lines, _right) {
      return lines.children.map((l) => l.rep());
    },
    // ClassLine
    ClassField(param, _comma) {
      return param.rep();
    },
    ClassMod(_mod, id, head, body) {
      let idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });

      let mod = head.rep();
      mod.name = idStr;

      let classs = context.module;
      classs.modules.push(mod); // Recursion

      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: mod,
      });
      context.add("Self", struct); // Self type
      for (let param in mod.params) {
        if (param.name === "self") {
          param.type = struct;
        }
        context.add(param.name, param);
      }
      for (let typeParam in mod.typeParams) {
        context.add(typeParam.name, typeParam);
      }

      let bodyRep = null;
      if (body.sourceString != ";") {
        bodyRep = body.rep();
      }
      mod.body = bodyRep;

      context = context.parent;
      return core.moduleDeclaration(mod);
    },

    ClassDecl(_class, id, typeParameters, superClass, classBody) {
      let idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });

      // can be null
      let typeParamsRep = typeParameters.rep();

      const classs = core.classs(idStr, typeParamsRep, [], []);
      context.add(idStr, classs);

      // can be null
      let superClassRep = superClass.rep();
      for (superClass in superClassRep) {
        mustMapFields(classs, superClass);
        context.classify(superClass, classs);
      }

      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: classs,
      });
      context.add("Self", classs);
      for (let typeParam in typeParamsRep) {
        context.add(typeParam.name, typeParam);
      }

      for (let line in classBody) {
        let lineRep = line.rep();
        if (lineRep.kind === "Field") {
          classs.fields.push(lineRep);
        } else if (lineRep.kind === "ModuleDeclaration") {
          classs.modules.push(lineRep.module);
        } else {
          must(false, "Expected field or module in class", { at: id });
        }
      }

      mustHaveDistinctFields(classs, { at: id });
      mustHaveDistinctModules(classs, { at: id });
      mustNotBeSelfContaining(classs, { at: id });

      context = context.parent;
      return core.classDeclaration(classs);
    },

    ClassImplBody(_left, lines, _right) {
      return lines.children.map((l) => l.rep());
    },
    // ClassImplLine
    ParamMap(_colon, id) {
      let idStr = id.sourceString;
      let impl = context.module;
      mustHaveField(impl.type, idStr);
      must(!(impl.type.kind === "Enum"), "Enum does not have fields", {
        at: id,
      });
      return impl.type.fields.find((f) => f.name === idStr);
    },
    ClassImplField(id, respectiveParam, _comma) {
      let idStr = id.sourceString;
      let impl = context.module;

      mustHaveField(impl.classs, idStr);
      let classField = impl.classs.fields.find((f) => f.name === idStr);

      let typeField = null;
      if (respectiveParam != null) {
        typeField = respectiveParam.rep();
      }
      mustBothHaveTheSameType(classField, typeField);

      return core.fieldMapping(classField.name, typeField.name, typeField.type);
    },

    ClassImpl(type, _impl, filledClass, body) {
      let fieldsMap = [];
      let modules = [];
      let typeRep = type.rep();
      let classRep = filledClass.rep();
      mustBeImplementable(typeRep, classRep, { at: type });
      let impl = core.classImpl(typeRep, classRep, fieldsMap, modules);
      context.add(typeRep.name + ".impl." + classRep.name, impl);

      context.classify(typeRep, classRep);

      context = context.newChildContext({
        inLoop: false,
        inData: false,
        module: impl,
      });

      for (let line in body.rep()) {
        if (line.kind === "FieldMapping") {
          impl.fieldsMap.push(line);
        } else if (line.kind === "ModuleDeclaration") {
          mustHaveModule(impl, line.mod);
          impl.modules.push(line.mod);
        } else {
          must(false, "Expected field or module in impl", { at: type });
        }
      }

      // Check fields
      mustBeDistinct(impl.fieldsMap, "Cannot duplicate field", { at: type });
      mustHaveDistinctModules(impl, { at: type });

      must(
        checkFieldMap(impl.fieldsMap, classRep.fields),
        "Cannot map fields",
        { at: type }
      );
      mustImplAllModules(impl, classRep, { at: type });

      context = context.parent;
      return impl;
    },

    // Actions

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
        
        return core.assignment(variable, exprRep)
      }
    },

    MethodCall(id, _dot, id2, _rp) {
      const method = context.lookup(id.sourceString);
      mustBeCallable(method, { at: id });
      mustHaveCorrectArgumentCount(args.children.length, method.params.length, {
        at: id,
      });
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

    ModCall(id, expr) {
      const module = context.lookup(id.sourceString);
      must(module?.kind === "Module", "Expected a module", { at: id });
      const methodCall = MethodCall();
      return core.modCall(module, methodCall);
    },

    Return(_return, _yield, expr) {
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
  });

  return builder(match).rep();
}
