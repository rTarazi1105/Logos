class Context {
  constructor({ parent = null, locals = new Map(), relations = new Map(), classes = new Map(), inLoop = false, module: f = null }) {
    Object.assign(this, { parent, locals, relations, classes, inLoop, module: f })
  }
  add(name, entity) {
    this.locals.set(name, entity)
  }
  lookup(name) {
    return this.locals.get(name) || this.parent?.lookup(name)
  }
  
  // Relations: Map<name, Set<List[values for which it is true]>
  lookupRelation(name) {
    return this.statements.get(name)
  }
  addRelation(name, values) {
    if (!this.statements.has(name)) {
      this.statements.set(name, new Set())
    } else {
      relation_number = this.statements.get(name).values().next().value.length;
      values_length = values.length;
      if relation_number != values.length {
        throw new Error(`Relation ${name} only accepts ${relation_number} values but was given ${values_length}`)
      }
    }
    this.statements.get(name).add(values)
  }
  
  
  // Collection, Equatable, Comparable, Error
  classify(classs, name) {
    if (!this.classes.has(classs)) {
      this.classes.set(classs, new Set())
    }
    this.classes.get(classs).add(name)
  }
  lookupClass(classs, name) {
    return this.classes.get(classs).has(name) || this.parent.lookupClass(name)
  }
  
  
  
  static root() {
    return new Context({
      locals: new Map(Object.entries(core.standardLibrary)),
      relations: new Map([
        ["Equal", Set()]
      ]),
      classes: new Map([
        ["Collection", Set()], // Add all arrays and tuples
        ["Equatable", Set([
          core.boolType,
          core.intType,
        ])],
        ["Comparable", Set([	// Add anything equatable
          core.intType,
        ])],
        ["Error", Set()],
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
      t2 === core.anyType ||
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
  
  function checkFields(struct, classs) {
    return classs.fields.every(
      (field, _i) =>
        struct.fields.find(field)?.type === field.type
    )
  }

  function mustHaveMember(structType, field, at) {
    must(structType.fields.map(f => f.name).includes(field), "No such field", at)
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
  
  
  const builder = match.matcher.grammar.createSemantics().addOperation("rep", {
    Program(sections) {
        return core.program(sections.children.map(s => s.rep()));
    },

    Section_dataDeclLine(dataDecl, _semicolon) {
        return dataDecl.rep();
    },

    Section_structDecl(structDecl) {
        return structDecl.rep();
    },

    Section_enumDecl(enumDecl) {
        return enumDecl.rep();
    },

    Section_classDecl(classDecl) {
        return classDecl.rep();
    },

    Section_methodDecl(methodDecl) {
        return methodDecl.rep();
    },

    Section_modDecl(modDecl) {
        return modDecl.rep();
    },

    Section_classImpl(classImpl) {
        return classImpl.rep();
    },

    Statement_bool(value) {
        return core.booleanLiteral(value.sourceString === "true");
    },

    Statement_equality(left, _op, right) {
        return core.equality(left.rep(), right.rep());
    },

    Statement_filledInfix(left, op, right) {
        return core.infixExpression(left.rep(), op.sourceString, right.rep());
    },

    Statement_filledRelation(left, op, right) {
        return core.relation(left.rep(), op.sourceString, right.rep());
    }
    
    // Data
    DataDecl_valueDecl(_value, id, relation) {
        mustNotAlreadyBeDeclared(id.sourceString, { at: id })
        
        existingRelation = context.lookup(relation);
        if (existingRelation == null) {
            // Assume it will be defined later
            newRelation = core.relation(relation, ["_"], null)
            context.addRelation(relation, [id])
            context.add(relation, newRelation)
            
            context.add(id, core.value(id, newRelation))
        } else {
            mustHaveNumber(existingRelation.kind, 1, { at: id })
            
            context.add(id, core.value(id, existingRelation))
        }
    }
    
    DataDecl_relationDecl(id, _colon1, args, _colon2, statement) {
        mustNotAlreadyBeDeclared(id.sourceString, { at: id })
        
        context.add(id, core.relation(id, args, statement))
    }
    
    DataDecl_operationDecl(_operation, id, _colon1, args, _colon2, statement) {
        mustNotAlreadyBeDeclared(id.sourceString, { at: id })
        
        context.add(id, core.operation(id, args, statement))
    }
    
    ArgRelation(id, number) {
        mustNotAlreadyBeDeclared(id.sourceString, { at: id })
        
        context.add(id, core.relationArg(id, number))
    }
    
    DataDecl_propertyDecl(_property, id, _colon1, args, _colon2, statement) {
        mustNotAlreadyBeDeclared(id.sourceString, { at: id })
        
        context = context.newChildContext({ inLoop: false, function: fun })
        fun.params = parameters.rep()
        
        context.add(id, core.operation(id, args, statement))
    }
  });





// TOAL CODE 
  const builder = match.matcher.grammar.createSemantics().addOperation("rep", {
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
