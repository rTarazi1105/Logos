class Context {
  constructor({ parent = null, locals = new Map(), classes = new Map(), inLoop = false, inData = false, module = null }) {
  // Data can have locals
  // Data can be inside module but not vice versa
    Object.assign(this, { parent, locals, classes, inLoop, module })
  }
  // Default will be Key: String, value: T
  // For truths, key: statement, value: bool
  // For classes, 
  add(name, entity) {
    this.locals.set(name, entity)
  }
  lookup(name) {
    return this.locals.get(name) || this.parent?.lookup(name)
  }
  
  addStatement(statement, truth) {
    if (statement?.statement != true) {
      throw new Error(`Not a statement: ${statement}`)
    }
  
    statement = statement
    truth = truth
    // Unwrap
    while (statement.kind === "Statement") {
      statement = statement.inner
    }
    // Unwrap negation
    while (statement.kind === "Negation") {
      statement = statement.inner
      truth = !truth
    }
    
    existing = this.locals.get(statement)
    if (existing != null) {
      if (existing != truth) {
        throw new Error(`Contradiction in data: ${statement.name}`)
      } else {
        console.log(`Statement ${statement.name} redundant`)
        return
      }
    }
    
    this.locals.set(statement, truth)
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
    return new Context({ ...this, ...props, parent: this, locals: new Map() })
  }
}

export default function analyze(match) {
  let context = Context.root()
  
  function must(condition, message, errorLocation) {
    if (!condition) {
      const prefix = errorLocation.at.source.getLineAndColumnMessage()
      throw new Error(`${prefix}${message}`)
    }
  }
  
  function mustNotAlreadyBeDeclared(name, at) {
    must(!context.lookup(name), `Identifier ${name} already declared`, at)
  }

  // Only at the end
  function mustHaveBeenFound(entity, name, at) {
    must(entity, `Identifier ${name} not declared`, at)
  }

  function mustBeInteger(e, at) {
    must(e.type === core.intType, "Expected an integer", at)
  }

  function mustBeBoolean(e, at) {
    must(e.type === core.booleanType, "Expected a boolean", at)
  }

  function mustBeVoid(e, at) {
    must(e.type === core.voidType, "Expected void", at)
  }
  
  function mustBeStatement(e, at) {
    must(e?.statement === true || e.kind === core.booleanType, "Expected statement", at)
  }
  
  function mustBeTypeT(e, t, at) {
    must(e?.kind === t, `Expected type ${t}`, at)
  }
  
  function mustHaveClass(e, at, className) {
    must(context.lookupClass(className, e), `Expected type to have class <${className}>`, at)
  }
  
  function mustNotHaveClass(e, at, className) {
    must(!context.lookupClass(className, e), `Class <${className}> already declared or not expected`, at)
  }

  function mustBeAStruct(e, at) {
    must(e.type?.kind === "Struct", "Expected a struct", at)
  }

  function mustBeAStructOrClass(e, at) {
    must(e.type?.kind === "Struct" || e.type?.kind === "Classs", "Expected a struct or class", at)
  }

  function mustBothHaveTheSameType(e1, e2, at) {
    must(equivalent(e1.type, e2.type), "Operands do not have the same type", at)
  }

  function mustAllHaveSameType(expressions, at) {
    // Used to check the elements of an array expression, and the two
    // arms of a conditional expression, among other scenarios.
    must(
      expressions.slice(1).every(e => equivalent(e.type, expressions[0].type)),
      "Not all elements have the same type",
      at
    )
  }

  function includesAsField(structType, type) {
    // Whether the struct type has a field of type "type", directly
    return structType.fields.some(
      field =>
        field.type === type
    )
  }

  function mustNotBeSelfContaining(structType, at) {
    const containsSelf = includesAsField(structType, structType)
    must(!containsSelf, "Struct type must not be self-containing", at)
  }
  
  function mustBeMutable(variable, at) {
    must(variable.mutable === true, at)
  }

  // ValidType in Ohm
  function mustBeAType(e, at) {
    const isPrimitiveType = /int|bool|void|any/.test(e)
    const isDataType = /Value|Infix|Statement|Assumption/.test(e)
    const isNumberedDataType = /RelationType|OperationType|PropertyType/.test(e?.kind)
    const isCompositeType = /Struct|Enum|ArrayType|TupleType|TypeParam/.test(e?.kind)
    must(isPrimitiveType || isDataType || isNumberedDataType || isCompositeType, "Type expected", at)
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
              (
                t1?.kind === "PropertyType" &&
                t1?.argNumbers.every((n,i) => t2?.argNumbers[i] === n)
              )
            )
          )
        )
      )
    )
  }
  
  function mustHaveNumber(e, n, at) {
    must(e?.number === n, "Incorrect number", at)
  }

  function isMutable(e) {
    return (
      (e?.kind === "Variable" && e?.mutable) ||
      (e?.kind === "VarField" && isMutable(e?.variable)) ||
      (e?.kind === "MemberExpression" && isMutable(e?.object))
    )
  }

  function mustBeMutable(e, at) {
    must(isMutable(e), "Cannot assign to immutable variable", at)
  }

  function assignable(fromType, toType) {
    return (
      toType == core.anyType ||
      equivalent(fromType, toType)
    )
  }

  function typeDescription(type) {
    if (typeof type === "string") return type
    if (type.kind == "Struct" || type.kind == "Enum") return type.name
    if (type.kind == "ArrayType") return `[${typeDescription(type.baseType)}]`
    if (type.kind == "ListType") return `List[${typeDescription(type.baseType)}]`
  }

  function mustBeAssignable(e, { toType: type }, at) {
    const source = typeDescription(e.type)
    const target = typeDescription(type)
    const message = `Cannot assign a ${source} to a ${target}`
    must(assignable(e.type, type), message, at)
  }

  function mustHaveDistinctFields(type, at) {
    const fieldNames = new Set(type.fields.map(f => f.name))
    must(fieldNames.size === type.fields.length, "Fields must be distinct", at)
  }

  function mustHaveDistinctModules(classs, at) {
    const moduleNames = new Set(classs.modules.map(f => f.name))
    must(moduleNames.size === classs.modules.length, "Fields must be distinct", at)
  }
  
  function checkFieldMap(struct, classs) {
    return classs.fields.every(
      (field, _i) =>
        struct.fields.find(field)?.type === field.type
    )
  }
  
  function mustMapFields(struct, classs, at) {
    must(checkFieldMap(struct, classs), "Fields do not map", at)
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
    must(context.inLoop, "Break can only appear in a loop", at)
  }

  function mustBeInAFunction(at) {
    must(context.module, "Return can only appear in a function", at)
  }
  
  // Up to here
  // reconsider enum cases

  function mustBeCallable(e, at) {
    const callable = e?.kind === "StructType" || e.type?.kind === "FunctionType"
    must(callable, "Call of non-function or non-constructor", at)
  }

  function mustNotReturnAnything(f, at) {
    const returnsNothing = f.type.returnType === core.voidType
    must(returnsNothing, "Something should be returned", at)
  }

  function mustReturnSomething(f, at) {
    const returnsSomething = f.type.returnType !== core.voidType
    must(returnsSomething, "Cannot return a value from this function", at)
  }

  function mustBeReturnable(e, { from: f }, at) {
    mustBeAssignable(e, { toType: f.type.returnType }, at)
  }

  function mustHaveCorrectArgumentCount(argCount, paramCount, at) {
    const message = `${paramCount} argument(s) required but ${argCount} passed`
    must(argCount === paramCount, message, at)
  }
  
  
  // BUILDER
  // TODO: For .map, use .children if + or *. If list, use .asIteration.children
  
  const builder = match.matcher.grammar.createSemantics().addOperation("rep", {
    Program(sections) {
        return core.program(sections.children.map(s => s.rep()));
    },
    
    // Data
    ValueDecl(_value, id, relationId) {
        mustNotAlreadyBeDeclared(id.sourceString, { at: id })
        mustHaveBeenFound(relationId.sourceString, { at: relationId })
        id = id.sourceString
        relationId = relationId.sourceString
        
        relation = context.lookup(relationId);
        mustHaveNumber(relation.kind, 1, { at: id })
            
        context.addStatement(core.filledRelation(relation, [id]), true)
        value = core.value(id)
        context.add(id, value)
        return core.valueDeclaration(value)
    },
    
    RelationDecl(id, _colon1, args, _colon2, statement) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      id = id.sourceString
      
      const relation = core.relation(id, args, null)
      // Add immediately so that we can have recursion
      context.add(id, relation)
      
      // Parameters are part of the child context
      context = context.newChildContext({ inLoop: false, inData: true, module: relation })
      relation.args = args.rep()
      
      // Analyze body while still in child context
      relation.statement = statement.rep()

      // Go back up to the outer context before returning
      context = context.parent
      return core.relationDeclaration(relation)
    },
    
    ArgValue(id) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      id = id.sourceString
    
      value = core.value(id)
      context.add(value)
      return value
    },
    
    OperationDecl(_op, id, _colon1, args, _colon2, statement) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      id = id.sourceString
      
      const operation = core.operation(id, args, null)
      // Add immediately so that we can have recursion
      context.add(id, operation)
      
      // Parameters are part of the child context
      context = context.newChildContext({ inLoop: false, inData: true, module: operation })
      operation.args = args.rep()
      
      // Analyze body while still in child context
      operation.statement = statement.rep()

      // Go back up to the outer context before returning
      context = context.parent
      return core.operationDeclaration(operation)
    },
    
    ArgStatement(id) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      id = id.sourceString
    
      statement = core.statement(id, null)
      context.add(statement)
      return statement
    },
    
    PropertyDecl(_prop, id, _colon1, args, _colon2, statement) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      id = id.sourceString
      
      const property = core.property(id, args, null)
      // Add immediately so that we can have recursion
      context.add(id, property)
      
      // Parameters are part of the child context
      context = context.newChildContext({ inLoop: false, inData: true, module: property })
      property.args = args.rep()
      
      // Analyze body while still in child context
      property.statement = statement.rep()

      // Go back up to the outer context before returning
      context = context.parent
      return core.propertyDeclaration(property)
    },
    
    ArgRelation(id, numbering) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      id = id.sourceString
      
      number = numbering.rep()
    
      relation = core.relation(id, new Array(number), null)
      context.add(relation)
      return relation
    },
    
    Numbering(_leftArrow, number, _rightArrow) {
      mustBeInteger(number.sourceString, { at: number })
      
      return Number(number.sourceString)
    },
    
    StatementDecl(id, _colon, body) {
      body = body.rep()
      mustBeStatement(body, { at: id } )
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      id = id.sourceString
      
      named = core.statement(id, body)
      context.add(id, named)
      return core.statementDeclaration(named)
    },
    
    InfixDecl(_infix, id, operation) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      id = id.sourceString
      
      infix = core.infix(id, operation)
      context.add(id, infix)
      return core.infixDeclaration(infix)
    },
    
    Assume(_assume, id, truth, _colon, body) {
      mustBeBoolean(truth, { at: body })
      statement = body.rep()
      context.addStatement(statement, truth)
      
      if (id != null) {
        mustNotAlreadyBeDeclared(id.sourceString, { at: id })
        id = id.sourceString
        
        named = core.statement(id, statement)
        context.add(id, named)
        return core.assumptionDeclaration(named, truth)
      }
    },
    
    Equality(v1, _equals, v2) {
      return core.equalityStatement(v1.rep(), v2.rep())
    },
    FilledInfix(s1, inf, s2) {
      statement1 = s1.rep()
      statement2 = s2.rep()
      infix = inf.rep()
      return core.filledOperation(infix?.operation, [statement1, statement2])
    },
    FilledOperation(id, _leftBracket, statements, _rightBracket) {
      operation = id.rep()
      statementsRep = statements.rep()
      return core.filledOperation(operation, statementsRep)
    },
    FilledRelation(id, _leftBracket, values, _rightBracket) {
      relation = id.rep()
      valuesRep = values.rep()
      return core.filledRelation(relation, valuesRep)
    },
    FilledProperty(id, _leftBracket, relations, _rightBracket) {
      property = id.rep()
      relationsRep = relations.rep()
      return core.filledProperty(property, relationsRep)
    },
    
    DeclaredStatement(s) {
      statement = context.lookup(s.sourceString)
      mustBeStatement(statement, { at: s } )
      return statement
    },
    Relation(r) {
      relation = context.lookup(r.sourceString)
      mustBeTypeT(relation?.kind, "RelationType", { at: r } )
      return relation
    },
    Operation(o) {
      operation = context.lookup(o.sourceString)
      mustBeTypeT(operation?.kind, "OperationType", { at: o } )
      return operation
    },
    Property(p) {
      property = context.lookup(p.sourceString)
      mustBeTypeT(property?.kind, "PropertyType", { at: p } )
      return property
    },
    Value(v) {
      value = context.lookup(v.sourceString)
      mustBeTypeT(value, "Value", { at: v } )
      return value
    },
    Infix(i) {
      infix = context.lookup(i.sourceString)
      mustBeTypeT(infix, "Infix", { at: i } )
      return infix
    },
    
    DeclaredClass(id) {
      // Lookup local is ok, they can only be declared globally
      // Though if not found, assume it's declared later
      classs = context.lookup(id.sourceString)
      if (classs == null) {
        id = id.sourceString
        // Last two null show that it's assumed before declaration
        // But empty list should remain
        classs = core.classs(id, [], null, null)
        context.add(id, classs)
      }
      return classs
    },
    Struct(id) {
      struct = context.lookup(id.sourceString)
      if (struct == null) {
        id = id.sourceString
        struct = core.struct(id, null)
        context.add(id, struct)
      }
      return struct
    },
    VarField(id, _dot, field) {
      variable = context.lookup(id.sourceString)
      field = field.sourceString
      if (variable == null) {
        // Assume tbd enum
        id = id.sourceString
        enumeration = core.enumeration(id, [core.field(field,null)])
        context.add(id, enumeration)
      } else {
        //mustHaveMember(variable, field, { at: id } )
        field = variable.fields.find((f) => f.name === field) // Can be an enum
        if (field == null) {
          must(false, "No such field", { at: id })
        }
        
        if (variable.kind === "Enum") {
          return core.enumCase(variable, field)
        } else {
          return core.varField(variable, field)
        }
      }
    },
    FilledTypeParams(_leftAngle, types, _rightAngle) {
      // TODO: CHECK
      // Check ValidType != self at moddecl
      return types.asIteration().children.map((t) => t.rep())
    },
    FilledClass(declaredClass, filledTypeParams) {
      classs = declaredClass.rep()
      params = filledTypeParams.rep()
      must(classs.typeParams.length === params.length, "Too many or few type parameters", { at: declaredClass } )
      return core.classFilledWithParam(classs, params)
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
      return typeParam
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
      
      for line in classBody.children.map(l => l.rep()) {
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
      for line in body.children.map(l => l.rep()).{
        if line.kind === "Field" {
          classs.fields.push(line)
        } else if line.kind === "Module" {
          classs.modules.push(line)
        } else {
          must(false, "Expected field or module in impl", { at: id })
        }
      }
    }
  });





  // TOAL CODE 
  const builder2 = match.matcher.grammar.createSemantics().addOperation("rep", {
    Program(statements) {
      return core.program(statements.children.map(s => s.rep()))
    },

    VarDecl(modifier, id, _eq, exp, _semicolon) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      const initializer = exp.rep()
      const mutable = modifier.sourceString === "mut"
      const variable = core.variable(id.sourceString, mutable, initializer.type)
      context.add(id.sourceString, variable)
      return core.variableDeclaration(variable, initializer)
    },
    
    

    TypeDecl(_struct, id, _left, fields, _right) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      // To allow recursion, enter into context without any fields yet
      const type = core.structType(id.sourceString, [])
      context.add(id.sourceString, type)
      mustHaveDistinctFields(type, { at: id })
      mustNotBeSelfContaining(type, { at: id })
      return core.typeDeclaration(type)
    },

    Field(id, _colon, type) {
      return core.field(id.sourceString, type.rep())
    },

    ModDecl(_fun, id, parameters, _colons, type, block) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id })
      // Add immediately so that we can have recursion
      const fun = core.fun(id.sourceString)
      context.add(id.sourceString, fun)

      // Parameters are part of the child context
      context = context.newChildContext({ inLoop: false, function: fun })
      fun.params = parameters.rep()

      // Now that the parameters are known, we compute the function's type.
      // This is fine; we did not need the type to analyze the parameters,
      // but we do need to set it before analyzing the body.
      const paramTypes = fun.params.map(param => param.type)
      const returnType = type.children?.[0]?.rep() ?? core.voidType
      fun.type = core.functionType(paramTypes, returnType)

      // Analyze body while still in child context
      fun.body = block.rep()

      // Go back up to the outer context before returning
      context = context.parent
      return core.functionDeclaration(fun)
    },

    Params(_open, paramList, _close) {
      // Returns a list of variable nodes
      return paramList.asIteration().children.map(p => p.rep())
    },

    Param(id, _colon, type) {
      const param = core.variable(id.sourceString, false, type.rep())
      mustNotAlreadyBeDeclared(param.name, { at: id })
      context.add(param.name, param)
      return param
    },

    Type_optional(baseType, _questionMark) {
      return core.optionalType(baseType.rep())
    },

    Type_array(_left, baseType, _right) {
      return core.arrayType(baseType.rep())
    },

    Type_function(_left, types, _right, _arrow, type) {
      const paramTypes = types.asIteration().children.map(t => t.rep())
      const returnType = type.rep()
      return core.functionType(paramTypes, returnType)
    },

    Type_id(id) {
      const entity = context.lookup(id.sourceString)
      mustHaveBeenFound(entity, id.sourceString, { at: id })
      mustBeAType(entity, { at: id })
      return entity
    },

    Statement_bump(exp, operator, _semicolon) {
      const variable = exp.rep()
      mustHaveIntegerType(variable, { at: exp })
      return operator.sourceString === "++"
        ? core.increment(variable)
        : core.decrement(variable)
    },

    Statement_assign(variable, _eq, expression, _semicolon) {
      const source = expression.rep()
      const target = variable.rep()
      mustBeMutable(target, { at: variable })
      mustBeAssignable(source, { toType: target.type }, { at: variable })
      return core.assignment(target, source)
    },

    Statement_call(call, _semicolon) {
      return call.rep()
    },

    Statement_break(breakKeyword, _semicolon) {
      mustBeInLoop({ at: breakKeyword })
      return core.breakStatement
    },

    Statement_return(returnKeyword, exp, _semicolon) {
      mustBeInAFunction({ at: returnKeyword })
      mustReturnSomething(context.function, { at: returnKeyword })
      const returnExpression = exp.rep()
      mustBeReturnable(returnExpression, { from: context.function }, { at: exp })
      return core.returnStatement(returnExpression)
    },

    Statement_shortreturn(returnKeyword, _semicolon) {
      mustBeInAFunction({ at: returnKeyword })
      mustNotReturnAnything(context.function, { at: returnKeyword })
      return core.shortReturnStatement
    },

    IfStmt_long(_if, exp, block1, _else, block2) {
      const test = exp.rep()
      mustHaveBooleanType(test, { at: exp })
      context = context.newChildContext()
      const consequent = block1.rep()
      context = context.parent
      context = context.newChildContext()
      const alternate = block2.rep()
      context = context.parent
      return core.ifStatement(test, consequent, alternate)
    },

    IfStmt_elsif(_if, exp, block, _else, trailingIfStatement) {
      const test = exp.rep()
      mustHaveBooleanType(test, { at: exp })
      context = context.newChildContext()
      const consequent = block.rep()
      context = context.parent
      const alternate = trailingIfStatement.rep()
      return core.ifStatement(test, consequent, alternate)
    },

    IfStmt_short(_if, exp, block) {
      const test = exp.rep()
      mustHaveBooleanType(test, { at: exp })
      context = context.newChildContext()
      const consequent = block.rep()
      context = context.parent
      return core.shortIfStatement(test, consequent)
    },

    LoopStmt_while(_while, exp, block) {
      const test = exp.rep()
      mustHaveBooleanType(test, { at: exp })
      context = context.newChildContext({ inLoop: true })
      const body = block.rep()
      context = context.parent
      return core.whileStatement(test, body)
    },

    LoopStmt_repeat(_repeat, exp, block) {
      const count = exp.rep()
      mustHaveIntegerType(count, { at: exp })
      context = context.newChildContext({ inLoop: true })
      const body = block.rep()
      context = context.parent
      return core.repeatStatement(count, body)
    },

    LoopStmt_range(_for, id, _in, exp1, op, exp2, block) {
      const [low, high] = [exp1.rep(), exp2.rep()]
      mustHaveIntegerType(low, { at: exp1 })
      mustHaveIntegerType(high, { at: exp2 })
      const iterator = core.variable(id.sourceString, false, core.intType)
      context = context.newChildContext({ inLoop: true })
      context.add(id.sourceString, iterator)
      const body = block.rep()
      context = context.parent
      return core.forRangeStatement(iterator, low, op.sourceString, high, body)
    },

    LoopStmt_collection(_for, id, _in, exp, block) {
      const collection = exp.rep()
      mustHaveAnArrayType(collection, { at: exp })
      const iterator = core.variable(id.sourceString, false, collection.type.baseType)
      context = context.newChildContext({ inLoop: true })
      context.add(iterator.name, iterator)
      const body = block.rep()
      context = context.parent
      return core.forStatement(iterator, collection, body)
    },

    Block(_open, statements, _close) {
      // No need for a block node, just return the list of statements
      return statements.children.map(s => s.rep())
    },

    Exp_conditional(exp, _questionMark, exp1, colon, exp2) {
      const test = exp.rep()
      mustHaveBooleanType(test, { at: exp })
      const [consequent, alternate] = [exp1.rep(), exp2.rep()]
      mustBothHaveTheSameType(consequent, alternate, { at: colon })
      return core.conditional(test, consequent, alternate, consequent.type)
    },

    Exp1_unwrapelse(exp1, elseOp, exp2) {
      const [optional, op, alternate] = [exp1.rep(), elseOp.sourceString, exp2.rep()]
      mustHaveAnOptionalType(optional, { at: exp1 })
      mustBeAssignable(alternate, { toType: optional.type.baseType }, { at: exp2 })
      return core.binary(op, optional, alternate, optional.type)
    },

    Exp2_or(exp, _ops, exps) {
      let left = exp.rep()
      mustHaveBooleanType(left, { at: exp })
      for (let e of exps.children) {
        let right = e.rep()
        mustHaveBooleanType(right, { at: e })
        left = core.binary("||", left, right, core.booleanType)
      }
      return left
    },

    Exp2_and(exp, _ops, exps) {
      let left = exp.rep()
      mustHaveBooleanType(left, { at: exp })
      for (let e of exps.children) {
        let right = e.rep()
        mustHaveBooleanType(right, { at: e })
        left = core.binary("&&", left, right, core.booleanType)
      }
      return left
    },

    Exp3_bitor(exp, _ops, exps) {
      let left = exp.rep()
      mustHaveIntegerType(left, { at: exp })
      for (let e of exps.children) {
        let right = e.rep()
        mustHaveIntegerType(right, { at: e })
        left = core.binary("|", left, right, core.intType)
      }
      return left
    },

    Exp3_bitxor(exp, xorOps, exps) {
      let left = exp.rep()
      mustHaveIntegerType(left, { at: exp })
      for (let e of exps.children) {
        let right = e.rep()
        mustHaveIntegerType(right, { at: e })
        left = core.binary("^", left, right, core.intType)
      }
      return left
    },

    Exp3_bitand(exp, andOps, exps) {
      let left = exp.rep()
      mustHaveIntegerType(left, { at: exp })
      for (let e of exps.children) {
        let right = e.rep()
        mustHaveIntegerType(right, { at: e })
        left = core.binary("&", left, right, core.intType)
      }
      return left
    },

    Exp4_compare(exp1, relop, exp2) {
      const [left, op, right] = [exp1.rep(), relop.sourceString, exp2.rep()]
      // == and != can have any operand types as long as they are the same
      // But inequality operators can only be applied to numbers and strings
      if (["<", "<=", ">", ">="].includes(op)) {
        mustHaveNumericOrStringType(left, { at: exp1 })
      }
      mustBothHaveTheSameType(left, right, { at: relop })
      return core.binary(op, left, right, core.booleanType)
    },

    Exp5_shift(exp1, shiftOp, exp2) {
      const [left, op, right] = [exp1.rep(), shiftOp.sourceString, exp2.rep()]
      mustHaveIntegerType(left, { at: exp1 })
      mustHaveIntegerType(right, { at: exp2 })
      return core.binary(op, left, right, core.intType)
    },

    Exp6_add(exp1, addOp, exp2) {
      const [left, op, right] = [exp1.rep(), addOp.sourceString, exp2.rep()]
      if (op === "+") {
        mustHaveNumericOrStringType(left, { at: exp1 })
      } else {
        mustHaveNumericType(left, { at: exp1 })
      }
      mustBothHaveTheSameType(left, right, { at: addOp })
      return core.binary(op, left, right, left.type)
    },

    Exp7_multiply(exp1, mulOp, exp2) {
      const [left, op, right] = [exp1.rep(), mulOp.sourceString, exp2.rep()]
      mustHaveNumericType(left, { at: exp1 })
      mustBothHaveTheSameType(left, right, { at: mulOp })
      return core.binary(op, left, right, left.type)
    },

    Exp8_power(exp1, powerOp, exp2) {
      const [left, op, right] = [exp1.rep(), powerOp.sourceString, exp2.rep()]
      mustHaveNumericType(left, { at: exp1 })
      mustBothHaveTheSameType(left, right, { at: powerOp })
      return core.binary(op, left, right, left.type)
    },

    Exp8_unary(unaryOp, exp) {
      const [op, operand] = [unaryOp.sourceString, exp.rep()]
      let type
      if (op === "#") {
        mustHaveAnArrayType(operand, { at: exp })
        type = core.intType
      } else if (op === "-") {
        mustHaveNumericType(operand, { at: exp })
        type = operand.type
      } else if (op === "!") {
        mustHaveBooleanType(operand, { at: exp })
        type = core.booleanType
      } else if (op === "some") {
        type = core.optionalType(operand.type)
      } else if (op === "random") {
        mustHaveAnArrayType(operand, { at: exp })
        type = operand.type.baseType
      }
      return core.unary(op, operand, type)
    },

    Exp9_emptyarray(ty, _open, _close) {
      const type = ty.rep()
      mustBeAnArrayType(type, { at: ty })
      return core.emptyArray(type)
    },

    Exp9_arrayexp(_open, args, _close) {
      const elements = args.asIteration().children.map(e => e.rep())
      mustAllHaveSameType(elements, { at: args })
      return core.arrayExpression(elements)
    },

    Exp9_emptyopt(_no, type) {
      return core.emptyOptional(type.rep())
    },

    Exp9_parens(_open, expression, _close) {
      return expression.rep()
    },

    Exp9_subscript(exp1, _open, exp2, _close) {
      const [array, subscript] = [exp1.rep(), exp2.rep()]
      mustHaveAnArrayType(array, { at: exp1 })
      mustHaveIntegerType(subscript, { at: exp2 })
      return core.subscript(array, subscript)
    },

    Exp9_member(exp, dot, id) {
      const object = exp.rep()
      let structType
      if (dot.sourceString === "?.") {
        mustHaveAnOptionalStructType(object, { at: exp })
        structType = object.type.baseType
      } else {
        mustHaveAStructType(object, { at: exp })
        structType = object.type
      }
      mustHaveMember(structType, id.sourceString, { at: id })
      const field = structType.fields.find(f => f.name === id.sourceString)
      return core.memberExpression(object, dot.sourceString, field)
    },

    Exp9_call(exp, open, expList, _close) {
      const callee = exp.rep()
      mustBeCallable(callee, { at: exp })
      const exps = expList.asIteration().children
      const targetTypes =
        callee?.kind === "StructType"
          ? callee.fields.map(f => f.type)
          : callee.type.paramTypes
      mustHaveCorrectArgumentCount(exps.length, targetTypes.length, { at: open })
      const args = exps.map((exp, i) => {
        const arg = exp.rep()
        mustBeAssignable(arg, { toType: targetTypes[i] }, { at: exp })
        return arg
      })
      return callee?.kind === "StructType"
        ? core.constructorCall(callee, args)
        : core.functionCall(callee, args)
    },

    Exp9_id(id) {
      // When an id appears in an expression, it had better have been declared
      const entity = context.lookup(id.sourceString)
      mustHaveBeenFound(entity, id.sourceString, { at: id })
      return entity
    },

    true(_) {
      return true
    },

    false(_) {
      return false
    },

    intlit(_digits) {
      // Carlos ints will be represented as plain JS bigints
      return BigInt(this.sourceString)
    },

    floatlit(_whole, _point, _fraction, _e, _sign, _exponent) {
      // Carlos floats will be represented as plain JS numbers
      return Number(this.sourceString)
    },

    stringlit(_openQuote, _chars, _closeQuote) {
      // Carlos strings will be represented as plain JS strings, including
      // the quotation marks
      return this.sourceString
    },
  })

  return builder(match).rep()
}
