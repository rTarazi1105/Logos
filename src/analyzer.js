class Context {
  constructor({ parent = null, locals = new Map(), classes = new Map(), inLoop = false, function: f = null }) {
    Object.assign(this, { parent, locals, inLoop, function: f })
  }
  add(name, entity) {
    this.locals.set(name, entity)
  }
  lookup(name) {
    return this.locals.get(name) || this.parent?.lookup(name)
  }
  // Collection, Equatable, Comparable, Error
  classify(classs, name) {
    if (!this.classes.has(classs)) {
      this.classes.set(classs, new Set()
    }
    this.classes.get(classs).add(name)
  }
  lookup_class(classs, name) {
    return this.classes.get(classs).has(name) || this.parent.lookup_class(name)
  }
  
  
  
  static root() {
    return new Context({
      locals: new Map(Object.entries(core.standardLibrary)),
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

  function mustBeNull(e, at) {
    must(e.type === core.nullType, "Expected null", at)
  }

  function mustBeCollection(e, at) {
    must(context.lookup_class("Collection",e), "Expected Collection", at)
  }

  function mustBeEquatable(e, at) {
    must(context.lookup_class("Equatable",e), "Expected Equatable", at)
  }

  function mustBeComparable(e, at) {
    must(context.lookup_class("Comparable",e), "Expected Comparable", at)
  }

  function mustBeError(e, at) {
    must(context.lookup_class("Error",e), "Expected Error", at)
  }
  
  function mustNotBeError(e, at) {
    must(!context.lookup_class("Error",e), "Expected not Error", at)
  }

  function mustBeAStruct(e, at) {
    must(e.type?.kind === "StructType", "Expected a struct", at)
  }

  function mustBeAStructOrClass(e, at) {
    must(e.type?.kind === "StructType" || e.type?.kind === "ClassType", "Expected a struct or class", at)
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
  
  // 2025-3-25: UP TO HERE

  function mustBeAType(e, at) {
    const isBasicType = /int|float|string|bool|void|any/.test(e)
    const isCompositeType = /StructType|FunctionType|ArrayType|OptionalType/.test(e?.kind)
    must(isBasicType || isCompositeType, "Type expected", at)
  }

  function mustBeAnArrayType(t, at) {
    must(t?.kind === "ArrayType", "Must be an array type", at)
  }

  function includesAsField(structType, type) {
    // Whether the struct type has a field of type type, directly or indirectly
    return structType.fields.some(
      field =>
        field.type === type ||
        (field.type?.kind === "StructType" && includesAsField(field.type, type))
    )
  }

  function mustNotBeSelfContaining(structType, at) {
    const containsSelf = includesAsField(structType, structType)
    must(!containsSelf, "Struct type must not be self-containing", at)
  }

  function equivalent(t1, t2) {
    return (
      t1 === t2 ||
      (t1?.kind === "OptionalType" &&
        t2?.kind === "OptionalType" &&
        equivalent(t1.baseType, t2.baseType)) ||
      (t1?.kind === "ArrayType" &&
        t2?.kind === "ArrayType" &&
        equivalent(t1.baseType, t2.baseType)) ||
      (t1?.kind === "FunctionType" &&
        t2?.kind === "FunctionType" &&
        equivalent(t1.returnType, t2.returnType) &&
        t1.paramTypes.length === t2.paramTypes.length &&
        t1.paramTypes.every((t, i) => equivalent(t, t2.paramTypes[i])))
    )
  }

  function assignable(fromType, toType) {
    return (
      toType == core.anyType ||
      equivalent(fromType, toType) ||
      (fromType?.kind === "FunctionType" &&
        toType?.kind === "FunctionType" &&
        // covariant in return types
        assignable(fromType.returnType, toType.returnType) &&
        fromType.paramTypes.length === toType.paramTypes.length &&
        // contravariant in parameter types
        toType.paramTypes.every((t, i) => assignable(t, fromType.paramTypes[i])))
    )
  }

  function typeDescription(type) {
    if (typeof type === "string") return type
    if (type.kind == "StructType") return type.name
    if (type.kind == "FunctionType") {
      const paramTypes = type.paramTypes.map(typeDescription).join(", ")
      const returnType = typeDescription(type.returnType)
      return `(${paramTypes})->${returnType}`
    }
    if (type.kind == "ArrayType") return `[${typeDescription(type.baseType)}]`
    if (type.kind == "OptionalType") return `${typeDescription(type.baseType)}?`
  }

  function mustBeAssignable(e, { toType: type }, at) {
    const source = typeDescription(e.type)
    const target = typeDescription(type)
    const message = `Cannot assign a ${source} to a ${target}`
    must(assignable(e.type, type), message, at)
  }

  function isMutable(e) {
    return (
      (e?.kind === "Variable" && e?.mutable) ||
      (e?.kind === "SubscriptExpression" && isMutable(e?.array)) ||
      (e?.kind === "MemberExpression" && isMutable(e?.object))
    )
  }

  function mustBeMutable(e, at) {
    must(isMutable(e), "Cannot assign to immutable variable", at)
  }

  function mustHaveDistinctFields(type, at) {
    const fieldNames = new Set(type.fields.map(f => f.name))
    must(fieldNames.size === type.fields.length, "Fields must be distinct", at)
  }

  function mustHaveMember(structType, field, at) {
    must(structType.fields.map(f => f.name).includes(field), "No such field", at)
  }

  function mustBeInLoop(at) {
    must(context.inLoop, "Break can only appear in a loop", at)
  }

  function mustBeInAFunction(at) {
    must(context.function, "Return can only appear in a function", at)
  }

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
}
