import * as core from "../src/core.js";
class Context {
  constructor({
    parent = null,
    locals = new Map(),
    classes = new Map(),
    inLoop = false,
    inData = false,
    module = null,
  }) {
    // Data can have locals
    // Data can be inside module but not vice versa
    Object.assign(this, { parent, locals, classes, inLoop, module });
  }
  // Default will be Key: String, value: T
  // For truths, key: statement, value: bool
  add(name, entity) {
    this.locals.set(name, entity);
  }
  lookup(name) {
    return this.locals.get(name) || this.parent?.lookup(name);
  }

  addStatement(statement, truth) {
    if (!statement?.statement != true) {
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

    let existing = this.locals.get(statement);
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
  // classes: Map<type, Set<class>>
  classify(classs, type) {
    if (!this.classes.has(type)) {
      this.classes.set(type, new Set())
    }
    this.classes.get(type).add(classs)
  }
  lookupClass(classs, type) {
    if this.classes.get(type).has(classs) {
      return true
    }
    for superClass of this.classes.get(type) {
      if lookupClass(superClass, classs) {
        return true
      }
    }
    // Don't look in parent - it should be the same
    // this.parent.lookupClass(classs, type)
    
    return false
  }
  
  
  
  static root() {
    equatableClass = core.standardLibrary["Equatable"]
    comparableClass = core.standardLibrary["Comparable"]
    
    return new Context({
      locals: new Map(Object.entries(core.standardLibrary)),
      classes: new Map([
        [core.boolType, Set([equatableClass])]
        [core.intType, Set([comparableClass])]
        [comparableClass, Set([equatableClass])]
      ])
    })
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

  function mustBeTypeT(e, t, at) {
    must(e?.kind === t, `Expected type ${t}`, at);
  }

  function mustHaveClass(e, at, className) {
    must(
      context.lookupClass(className, e),
      `Expected type to have class <${className}>`,
      at
    );
  }

  function mustNotHaveClass(e, at, className) {
    must(
      !context.lookupClass(className, e),
      `Class <${className}> already declared or not expected`,
      at
    );
  }

  function mustBeAStruct(e, at) {
    must(e.type?.kind === "Struct", "Expected a struct", at);
  }

  function mustBeAStructOrClass(e, at) {
    must(
      e.type?.kind === "Struct" || e.type?.kind === "Classs",
      "Expected a struct or class",
      at
    );
  }

  function mustBothHaveTheSameType(e1, e2, at) {
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
    const containsSelf = includesAsField(structType, structType);
    must(!containsSelf, "Struct type must not be self-containing", at);
  }

  function mustBeMutable(variable, at) {
    must(variable.mutable === true, at);
  }

  // ValidType in Ohm
  function mustBeAType(e, at) {
    const isPrimitiveType = /int|bool|void|any/.test(e);
    const isDataType = /Value|Infix|Statement|Assumption/.test(e);
    const isNumberedDataType = /RelationType|OperationType|PropertyType/.test(
      e?.kind
    );
    const isCompositeType = /Struct|Enum|ArrayType|TupleType|TypeParam/.test(
      e?.kind
    );
    must(
      isPrimitiveType || isDataType || isNumberedDataType || isCompositeType,
      "Type expected",
      at
    );
  }

  function equivalent(t1, t2) {
    return (
      t1 === t2 || (
      
        t1?.kind == t2?.kind && (
          (
            t1?.kind === "ArrayType" &&
            t1?.len === t2?.len &&
            equivalent(t1.baseType, t2.baseType)
          ) ||
          (
            t1?.kind === "ListType" &&
            equivalent(t1.baseType, t2.baseType)
          ) ||
          (
            t1?.kind === "ActionType" &&
            equivalent(t1.returnType, t2.returnType)
          ) ||
          (
            t1?.number == t2?.number &&
            (
              t1?.kind === "RelationType" ||
              t1?.kind === "OperationType" ||
              t1?.kind === "RelationArgType" ||
              (t1?.kind === "PropertyType" &&
                t1?.argNumbers.every((n, i) => t2?.argNumbers[i] === n))))))
    );
  }

  function mustHaveNumber(e, n, at) {
    must(e?.number === n, "Incorrect number", at);
  }

  function isMutable(e) {
    return (
      (e?.kind === "Variable" && e?.mutable) ||
      (e?.kind === "VarField" && isMutable(e?.variable)) ||
      (e?.kind === "MemberExpression" && isMutable(e?.object))
    );
  }

  function assignable(fromType, toType) {
    return toType == core.anyType || equivalent(fromType, toType);
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

  function mustHaveDistinctFields(type, at) {
    const fieldNames = new Set(type.fields.map(f => f.name))
    must(fieldNames.size === type.fields.length, "Fields must be distinct", at)
  }

  function mustHaveDistinctModules(classs, at) {
    const moduleNames = new Set(classs.modules.map(f => f.name))
    must(moduleNames.size === classs.modules.length, "Fields must be distinct", at)
  }
  
  function checkFieldMap(fields1, fields2) {
    return fields2.every(
      (field, _i) =>
        fields1.find(field)?.type === field.type
    )
  }
  
  function mustMapFields(struct, classs, at) {
    must(checkFieldMap(struct.fields, classs.fields), "Fields do not map", at)
  }

  function mustHaveMember(structOrEnumType, field, at) {
    must(structOrEnumType.fields.some(typeField => typeField.name === field.name && typeField.type === field.type , "No such field", at)
  }
  
  function mustHaveModules(classImpl, classs, at) {
    for (i, mod) in classs.modules.entries() {
      otherMod = classImpl.modules[i]
      must(otherMod.name === mod.name, "Mod mismatch in name", at)
      must(equivalent(mod.Type, otherMod.type), "Mod mismatch in type", at)
    }
  }

  function mustBeInLoop(at) {
    must(context.inLoop, "Break can only appear in a loop", at);
  }

  function mustBeInAFunction(at) {
    must(context.module, "Return can only appear in a function", at);
  }

  // Up to here
  // reconsider enum cases

  function mustBeCallable(e, at) {
    const callable =
      e?.kind === "StructType" || e.type?.kind === "FunctionType";
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
  // TODO: For .map, use .children if + or *. If list, use .asIteration.children
  
  const builder = match.matcher.grammar.createSemantics().addOperation("rep", {
    Program(sections) {
      return core.program(sections.children.map((s) => s.rep()));
    },

    // Actions
    Assignment(id, _eq, expr) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      const variable = context.lookup(id.sourceString);
      mustBeAssignable(expr.rep(), { toType: variable.type }, { at: id });
      return core.assignment(variable, expr.rep());
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

    ModCall(id, _dot, method, _lp, args, _rp) {
      const module = context.lookup(id.sourceString);
      must(module?.kind === "Module", "Expected a module", { at: id });
      const methodCall = MethodCall() 
      return core.modCall(module, methodCall);
    },

    Return(_return, expr) {
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

    // Data
    ValueDecl(_value, id, _colon, relationId) {
        idStr = id.sourceString
        mustNotAlreadyBeDeclared(idStr, { at: id })
        value = core.value(idStr)
        context.add(idStr, value)
        
        if relationId != null {
          relation = relationId.rep()
          mustHaveNumber(relation.kind, 1, { at: id })
          context.addStatement(core.filledRelation(relation, [value]), true)
        }
            
        return core.valueDeclaration(value)
    },

    RelationDecl(id, _colon1, args, _colon2, statement) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      idStr = id.sourceString
      
      const relation = core.relation(idStr, args, null)
      // Add immediately so that we can have recursion
      context.add(idStr, relation)
      
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
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      idStr = id.sourceString
    
      value = core.value(idStr)
      context.add(idStr, value)
      return value
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
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      id = id.sourceString
    
      statement = core.statement(id, null)
      context.add(id, statement)
      return statement
    },

    PropertyDecl(_prop, id, _colon1, args, _colon2, statement) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      id = id.sourceString;

      const property = core.property(id, args, null);
      // Add immediately so that we can have recursion
      context.add(id, property);

      // Parameters are part of the child context
      context = context.newChildContext({
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
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      id = id.sourceString
      
      number = numbering.rep()
    
      relation = core.relation(id, new Array(number), null)
      context.add(id, relation)
      return relation
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
        id = id.sourceString;

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
      mustBeTypeT(relation?.kind, "RelationType", { at: r });
      return relation;
    },
    Operation(o) {
      const operation = context.lookup(o.sourceString);
      mustBeTypeT(operation?.kind, "OperationType", { at: o });
      return operation;
    },
    Property(p) {
      const property = context.lookup(p.sourceString);
      mustBeTypeT(property?.kind, "PropertyType", { at: p });
      return property;
    },
    Value(v) {
      const value = context.lookup(v.sourceString);
      mustBeTypeT(value, "Value", { at: v });
      return value;
    },
    Infix(i) {
      const infix = context.lookup(i.sourceString);
      mustBeTypeT(infix, "Infix", { at: i });
      return infix;
    },

    DeclaredClass(id) {
      // Lookup local is ok, they can only be declared globally
      // Though if not found, assume it's declared later
      classs = context.lookup(id.sourceString);
      if (classs == null) {
        id = id.sourceString;
        // Last two null show that it's assumed before declaration
        // But empty list should remain
        const classs = core.classs(id, [], null, null);
        context.add(id, classs);
      }
      return classs;
    },
    Struct(id) {
      const struct = context.lookup(id.sourceString);
      if (struct == null) {
        id = id.sourceString;
        const struct = core.struct(id, null);
        context.add(id, struct);
      }
      return struct;
    },
    VarField(id, _dot, field) {
      const variable = context.lookup(id.sourceString);
      field = field.sourceString;
      if (variable == null) {
        // Assume tbd enum
        id = id.sourceString;
        const enumeration = core.enumeration(id, [core.field(field, null)]);
        context.add(id, enumeration);
      } else {
        //mustHaveMember(variable, field, { at: id } )
        field = variable.fields.find((f) => f.name === field); // Can be an enum
        if (field == null) {
          must(false, "No such field", { at: id });
        }

        if (variable.kind === "Enum") {
          return core.enumCase(variable, field);
        } else {
          return core.varField(variable, field);
        }
      }
    },
    FilledTypeParams(_leftAngle, types, _rightAngle) {
      // TODO: CHECK
      // Check ValidType != self at moddecl
      return types.asIteration().children.map((t) => t.rep())
    },
    FilledClass(declaredClass, filledTypeParams) {
      const classs = declaredClass.rep();
      const params = filledTypeParams.rep();
      must(
        classs.typeParams.length === params.length,
        "Too many or few type parameters",
        { at: declaredClass }
      );
      return core.classFilledWithParam(classs, params);
    },
    SuperClass(_colon, classes) {
      return classes.asIteration().children.map((c) => c.rep())
    },
    IdAndSuperClass(id, superclasses) {
      mustNotBeAlreadyDeclared(id.sourceString, { at: id })
      id = id.sourceString
      let typeParam = core.typeParameter(id)
      for (classs in superclasses.rep()) {
        typeParam.classes.add(classs)
      }
      return typeParam;
    },
    TypeParam(_leftAngle, typeParamList, _rightAngle) {
      return typeParamList.asIteration().children.map((t) => t.rep())
    },

    // Types can be declared out of order, so check what's already in the database
    StructDecl(_struct, id, typeParameters, superClass, _leftBracket, parameters, _rightBracket) {
      idStr = id.sourceString
      mustNotBeAlreadyDeclared(idStr, { at: id })
      
      // can be null
      typeParamsRep = typeParameters.rep()
      
      // Allow recursion (but not for the type params)
      const struct = core.struct(idStr, typeParamsRep, [])
      context.add(idStr, struct)
      
      // Auto-impl superclasses
      // can be null
      superClassRep = superClass.rep()
      for classs in superClassRep {
        if classs.modules != [] {
          must(false, "Class has modules, use impl instead", { at: id })
        }
        mustMapFields(struct, classs)
        context.classify(classs, struct)
      }
      
      // Analyze parameters with typeParams
      context = context.newChildContext({ inLoop: false, inData: false, module: struct })
      for typeParam in typeParamsRep {
        // Already checked mustNotBeAlreadyDeclared in IdAndSuperClass
        context.add(typeParam.name, typeParam)
      }
      
      params = parameters.asIteration().children.map((p) => p.rep())
      struct.fields = params
      mustHaveDistinctFields(struct, { at: id })
      mustNotBeSelfContaining(struct, { at: id })
      
      context = context.parent
      return core.structDeclaration(struct)
    }
    
    EnumDecl(_enum, id, typeParameters, _leftBracket, parameters, _rightBracket) {
      idStr = id.sourceString
      mustNotBeAlreadyDeclared(idStr, { at: id })
      
      // can be null
      typeParamsRep = typeParameters.rep()
      
      const enumeration = core.enumeration(idStr, typeParamsRep, [])
      context.add(idStr, enumeration)
      
      context = context.newChildContext({ inLoop: false, inData: false, module: enumeration })
      for typeParam in typeParamsRep {
        context.add(typeParam.name, typeParam)
      }
      
      params = parameters.asIteration().children.map((p) => p.rep())
      enumeration.cases = params
      mustHaveDistinctFields(enumeration, { at: id })
      mustNotBeSelfContaining(enumeration, { at: id })
      
      context = context.parent
      return core.enumDeclaration(enumeration)
    }
    
    ClassDecl(_class, id, typeParameters, superClass, classBody) {
      idStr = id.sourceString
      mustNotBeAlreadyDeclared(idStr, { at: id })
      
      // can be null
      typeParamsRep = typeParameters.rep()
      
      const classs = core.classs(idStr, typeParamsRep, [], [])
      context.add(idStr, classs)
      
      // can be null
      superClassRep = superClass.rep()
      for superClass in superClassRep {
        mustMapFields(classs, superClass)
        context.classify(superClass, classs)
      }
      
      context = context.newChildContext({ inLoop: false, inData: false, module: struct })
      for typeParam in typeParamsRep {
        context.add(typeParam.name, typeParam)
      }
      
      for line in classBody.rep() {
        if line.kind === "Field" {
          classs.fields.push(line)
        } else if line.kind === "Module" {
          classs.modules.push(line)
        } else {
          must(false, "Expected field or module in class", { at: id })
        }
      }
      
      mustHaveDistinctFields(classs, { at: id })
      mustHaveDistinctModules(classs, { at: id })
      mustNotBeSelfContaining(classs, { at: id })
      
      context = context.parent
      return core.classDeclaration(classs)
    }
    
    // TODO: things starting with "Class"
    
    ClassImpl(type, _impl, classs, body) {
      fields = []
      modules = []
      for line in body.rep().{
        if line.kind === "Field" {
          classs.fields.push(line)
        } else if line.kind === "Module" {
          classs.modules.push(line)
        } else {
          must(false, "Expected field or module in impl", { at: id })
        }
      }
      //impl = classImpl(type, classs, 
      //mustHaveModules(
      
      context.classify(type, classs)
    }
    
    ClassBody(_left, lines, _right) {
      return lines.children.map(l => l.rep())
    }
    
    ClassImplBody(_left, lines, _right) {
      return lines.children.map(l => l.rep())
    }
  });


  return builder(match).rep()
}
