// TODO Change most let to const
// Note: In errors, "?!" should've been caught by parser

import * as core from "../src/core.js";
class Context {
  constructor({
    parent = null,
    locals = new Map(),
    inLoop = false,
    module = null
  }) {
    // Data can have locals
    // Data can be inside module but not vice versa
    Object.assign(this, { parent, locals, inLoop, module });
  }
  // Default will be Key: String, value: T
  // For impl, key: (type).impl.(class)
      // TODO: Ensure all .classify also adds impl
  // For methods, key: (Struct/Class).(mod)
  // For impl methods, key: (type).impl.(class).(mod)
      // Not all methods will be added as such
      // It's just a byproduct of ClassBody
  
  // For truths, key: statement, value: bool
  // For classes: type, map<className, class>
  add(name, entity) {
    this.locals.set(name, entity);
  }
  remove(key) { // Used in drop()
    let location = this;
    while (location.locals.get(key) == null) {
      location = location.parent;
    }
    location.locals.delete(key);
  }
  lookup(key) {
    return this.locals.get(key) || this.parent?.lookup(key);
  }
  
  getRoot() {
    if (this.parent != null) {
      return this.parent.getParent();
    }
  }

  addStatement(statement, inputTruth) {
    if (!isStatement(statement)) {
    // Note: can add booleans, even "true" can be added as false
    // should show up as a contradiction later
      throw new Error(`Not a statement: ${statement}`);
    }
    
    const truth = inputTruth;
    // Unwrap negation
    while (statement.kind === "Negation") {
      statement = statement.inner;
      truth = !truth;
    }

    const existing = this.locals.get(statement);
    if (existing != null) {
      if (existing !== truth) {
        throw new Error(`Direct contradiction in data: ${statement.name}`);
      } else {
        console.log(`Statement ${statement.name} redundant`);
        return;
      }
    }

    this.add(statement, truth);
  }
  
  classify(type, classs) {
    if (this.locals.get(type) == null) {
      this.locals.set(type, new Map());
    }
    
    this.locals.get(type).set(classs.name, classs);
  }
  
  lookupClass(type, className) {
    if (className === "Any") {
      return true;
    }
    if (className === "Add" && type?.kind === "ListType") {
      return true;
    }
    
    const superClasses = this.classes.get(type);
    
    if (superClasses.containsKey(className)) {
      return true;
    }
    
    for (const classs of superClasses.values()) {
      if (lookupClass(classs, className)) {
        return true;
      }
    }
    
    return this.parent?.lookupClass(type, className);
  }

  static root() {
    const context = new Context({
      locals: new Map(Object.entries(core.standardLibrary))
    });
    const equatableClass = core.standardLibrary["Equatable"];
    const comparableClass = core.standardLibrary["Comparable"];
    const addClass = core.standardLibrary["Add"];
    context.classify(core.voidType, equatableClass);
    context.classify(core.boolType, equatableClass);
    context.classify(core.stringType, equatableClass);
    context.classify(core.intType, comparableClass);
    context.classify(comparableClass, equatableClass);
    
    context.classify(core.stringType, addClass);
    context.classify(core.intType, addClass);
    
    return context;
  }
  newChildContext(props) {
    return new Context({ ...this, ...props, parent: this, locals: new Map() });
  }
}

export default function analyze(match) {
  let context = Context.root();

  function must(condition, message, errorLocation) {
    if (condition !== true) {
      const prefix = errorLocation.at.source.getLineAndColumnMessage();
      throw new Error(`${prefix}${message}`);
    }
  }
  
  function mustNotBeNull(e, msg, at) {
    must(e != null, msg, at)
  }

  function mustNotAlreadyBeDeclared(name, at) {
    must(!context.lookup(name), `Identifier ${name} already declared`, at);
  }

  function mustAlreadyBeDeclared(name, at) {
    must(context.lookup(name), `Identifier ${name} not declared`, at);
  }
  
  function mustBeTypeT(e, type, msg, at) {
    must(e?.type === type, msg ?? `Expected type ${type}`, at);
  }
  
  function isArrayOrList(type) {
    return type?.kind === "ArrayType" || type?.kind === "ListType"
  }
  
  function mustHaveIndex(obj, i, at) {
    let len = null;
    if (isString(obj)) {
      len = obj.str.length
    }
    if (obj?.type?.kind === "ArrayType") {
      len = obj.type.len;
    }
    if (len != null) {
      must(i > 0 && i < len, "Index out of bounds", at);
      return;
    }
    
    must(obj?.type?.kind === "ListType", "Not indexable", at);
  }

  function mustBeKindK(e, k, msg, at) {
    let message = msg;
    if (msg == null) {
      message = `Expected kind ${k}`;
    }
    must(e?.kind === k, message, at);
  }
  
  function isString(s) {
    return s?.type === core.stringType;
  }
  
  function isBoolean(b) {
    //return typeof(b) === "boolean";
    //return b.type === core.boolType;
    return typeof(b) === core.boolType || typeof(b) === "boolean";
  }

  function mustBeBoolean(e, at) {
    must(isBoolean(e), "Expected a boolean", at);
  }

  function mustBeVoid(e, at) {
    mustBeTypeT(e, core.voidType, "Expected void", at);
  }
  
  function isInteger(e) {
    return e?.type === core.intType;
  }

  function mustBeInteger(e, at) {
    must(isInteger(e), "Expected an integer", at);
  }
  
  function numerify(nStr, at) {
    const number = Number(nStr);
    must(!Number.isNaN(number), "Not an integer", at);
    must(number >= 0, "Expected nonnegative?!", at);
    return number;
  }
  
  function numerifyNonzero(nStr, at) {
    const number = numerify(nStr, at);
    must(number !== 0, "Expected nonzero number", at);
    return number;
  }

  function isStatement(e) {
    //e?.type === core.statementType || /bool/.test(e)
    return isBoolean(e) || e?.type === core.statementType;
  }

  function mustBeStatement(e, at) {
    const t = typeof(e);
    const p = e?.type;
    must(
      isStatement(e),
      "Expected statement, found " + t + "/" + p,
      at
    );
  }

  function mustHaveClass(type, className, at) {
    must(
      context.lookupClass(type, className),
      `Expected type to have class <${className}>`,
      at
    );
  }

  function mustNotHaveClass(e, className, at) {
    must(
      !context.lookupClass(e?.type, className),
      `Cannot reuse class <${className}>`,
      at
    );
  }

  function mustBothHaveSameType(e1, e2, at) {
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
        .every((e) => compatible(e.type, expressions[0].type)),
      "All elements must have the same type as the first",
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
  
  function includesAsCase(enumType, type) {
    return enumType.cases.some((field) => field.type === type);
  }

  function enumMustNotBeSelfContaining(enumType, at) {
    const containsSelf =
      includesAsCase(enumType, enumType) ||
      includesAsCase(enumType, "Self");
    must(!containsSelf, "Enum type must not be self-containing", at);
  }

  function isData(x) {
    return (x?.type === core.valueType ||
    x?.type === core.statementType ||
    x?.kind === "Relation" ||
    x?.kind === "Infix" ||
    x?.kind === "Operation" ||
    x?.kind === "Property");
  }
  
  function isAction(x) {
    return x?.isAction || x?.type === "DataDecl";
  }
  
  function mustBeAction(a, at) {
    must(isAction(a), "Expected action", at);
  }
  
  function isDataDecl(x) {
    return (x?.type === "DataDecl");
  }
  
  function mustHaveType(x, msg, at) {
    return mustNotBeNull(x?.type, msg, at);
  }
  
  function mustBeObject(x, msg, at) {
    if (isVariable(x) || isBoolean(x) || isInteger(x)) {
      return true;
    }
    mustHaveType(x, "Not an object (no type)" + msg, at);
    must(//hasType(x) &&
      !(isAction(x?.type)) &&
      !(isDataDecl(x?.type)) &&
      x?.kind !== "MatchConditionType" &&
      x?.type !== "Comparison" &&
      x?.type !== "MutRef",// &&
      //x?.kind !== "Field" &&
      //x?.kind !== "Parameter", &&
      //x?.kind !== "Variable",
      "Not an object" + msg,
      at
    );
  }
  
  function isVariable(e) {
    return e?.kind === "Variable" || e?.kind === "VarField" || e?.kind === "Parameter";
  }

  function mustBeAssignedVar(e, at) {
    must(
      isVariable(e),
      "Variable not assigned",
      at
    );
  }

  function equivalent(t1, t2) {
  
    if (t1?.kind === "Class" || t2?.kind === "Class") {
      return false; // "Use classObjectType not class"
    }
    
    return (
      t1 === t2 
      || (
        t1?.kind != null 
        && t2?.kind != null 
        && t1.kind === t2.kind 
        && (
          (
            (
              (
                t1.kind === "ArrayType" 
                && t1?.len === t2?.len 
              ) || t1.kind === "ListType"
              || t1.kind === "MutRefType"
              
            )
            && equivalent(t1.basicType, t2.basicType)
          ) || (
            (
              t1.kind === "RelationType" ||
              t1.kind === "OperationType"
            ) 
            && t1.number === t2.number
          ) || (
            t1.kind === "PropertyType" 
            && t1.numbers.every((n, i) => t2.numbers[i] === n)
          ) || (
            (
              t1.kind === "Struct"
              || t1.kind === "Enum"
            ) && t1?.name != null
            && t1.name === t2?.name
          )
        )
      )
    );
  }
  
  function compatible(smallerType, biggerType) {
    if (equivalent(smallerType, biggerType)) {
      return true;
    }
    
    if (biggerType?.kind === "ListType" && smallerType?.kind === "ArrayType") {
      return true;
    }
    
    if (biggerType?.kind !== "ClassObjectType") {
      return false;
    }
    const included = true;
    for (const classs in biggerType.classes) {
      if (context.lookupClass(smallerType, classs) !== true) {
        included = false;
      }
    }
    return included;
  }

  function mustBeAssignable(content, targetType, at) {
    const source = content.type;
    
    mustBeObject(content, null, at);
    mustNotBeNull(targetType, "Variable has no type?!", at);
    const sourceStr = core.typeName(source);
    const targetStr = core.typeName(targetType);
    const message = `Cannot assign a ${sourceStr} to a ${targetStr}`;
    must(compatible(targetType, source), message, at);
  }

  function mustHaveNumber(e, n, at) {
    must(e?.number === n, "Incorrect number", at);
  }
  
  function mustHaveEqualLength(e1, e2, at) {
    must(e1?.length != null && e1?.length === e2?.length, "Expected equal length", at)
  }

  function mustHaveLength(e, n, at) {
    must(e?.length === n, "Incorrect length", at);
  }

  function mustBeDistinct(list, message, at) {
    const listSet = new Set(list.map((x) => x.name));
    must(listSet.size === list.length, message, at);
  }
  
  function casesMustHaveDistinctTypes(enumeration, at) {
    const msg = "Cases must be of distinct types";
    
    const setOfCases = new Set(enumeration.cases.map(c => c.type));
    must(setOfCases.size === enumeration.cases.length, msg, at);
    
    const entries = enumeration.cases.entries();
    for (const [i, laterType] in entries) {
      for (const [j, earlierType] in entries) {
        if (j === i) {
          break;
        }
        
        must(!compatible(laterType, earlierType), msg, at);
      }
    }
  }

  function mustHaveDistinctModules(classs, at) {
    mustBeDistinct(classs.modules, "Modules must be distinct", at);
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
  
  function modAssignable(smallMod, bigMod) {
    
    if (smallMod.mutSelf !== bigMod.mutSelf) {
      return false;
    }
    
    if (smallMod.params?.length !== bigMod.params?.length) {
      return false;
    }
    for (const i in smallMod.params.length) {
      if (compatible(smallMod.params[i], bigMod.params[i]) !== true) {
        return false;
      }
    }
    
    return compatible(smallMod.returnType, bigMod.returnType);
  }
  
  function hasModule(type, module) {
    return type.modules.some(
        (typeMod) =>
          typeMod.name === module.name && modAssignable(typeMod, module)
    );
  }

  function mustHaveModule(type, module, at) {
    must(
      hasModule(type, module),
      "No such method",
      at
    );
  }

  function mustImplAllModules(classImpl, classs, at) {
    const necessaryMods = classs.modules.filter((m) => m.body != null);
    // mustHaveEqualLength(classImpl.modules, necessaryMods); // Not required - method override allowed
    for (const mod of necessaryMods) {
      mustHaveModule(classImpl, mod, at);
    }
  }

  function mustBeInLoop(breaking, at) {
    let kind = "Continue";
    if (breaking) {
      kind = "Break";
    }
    must(context.inLoop, `${kind} can only appear in a loop`, at);
  }

  function mustBeInAModule(at) {
    must(context.module?.kind === "ModBody", "Return can only appear in a module", at);
  }
  
  function mustBeAType(t, at) {
    must(t?.isType, `Expected ${t} to be a type`, at);
  }
  
  function mustBeCustomType(t, allowStd, at) {
    mustBeAType(t, at);
    if (!allowStd) {
      must(t !== core.valueType && t!== core.statementType, "Cannot be statement or value structs", at);
    }
    must(t?.kind === "Struct" || t?.kind === "Enum" || t?.kind === "ClassObjectType", `Expected ${t} to be a custom type`, at);
  }
  
  function getMutRefContent(x) {
    if (x?.kind === "Variable") {
      return x.content;
    } else if (x?.kind === "VarField") {
      return x.variable.contents[x.index];
    }
    return null;
  }
  function mustBeMutRefable(x, at) {
    must(getMutRefContent(x) != null, "Must be a variable or field", at)
  } 
  
  
  function findMethod(subjType, methodStr, at) {
        let method = subjType?.methods?.find(m => m.name === methodStr); // If impl, it will be found here (see ClassMod)
        								// thus allowing override
        if (method == null) {
          // Search classes
          
          let classes = [];
          const newClasses = context.lookup(subjType); // Works for classobjecttype (see ClassDecl)
          classes.push(...newClasses);
          
          while (method == null) {
            for (const [_, classs] of classes) {
              // Output: undefined (dark gray)
              method = classs.modules.find(m => m.name === methodStr);
              // TypeError: Cannot read properties of undefined (reading 'find')
              
              if (method != null) {
                break;
              }
            }
            
            const oldClasses = classes;
            classes = [];
            let failure = false;
            for (const [_, classs] of oldClasses) {
              const newClasses = context.lookup(classs);
              if (newClasses == null) {
                failure = true;
                break;
              }
              classes.push(...newClasses);
            }
            if (failure || classes.length === 0) {
              must(false, "No method found", at);
            }
          }
        }
        mustNotBeNull(method.body, "Body not defined", at);
    
    return method;
  }




  // BUILDER
  // Before .map(), use .children if + or *
  // If list, use .asIteration.children

  const builder = match.matcher.grammar.createSemantics().addOperation("rep", {
    Program(sections) {
      return core.program(sections.children.map((s) => s.rep()));
    },
    _iter(...children) {  // Explicit `_iter` handler
      return children.map(child => child.rep());
    }, 
    _terminal() {  // Explicitly handle terminal nodes
      return this.sourceString;  // Return the raw string value
    },

    // 1. Data Declarations
    DataDeclLine(dataDecl, _semicolon) {
      return dataDecl.rep();
    },
    
    ValueDecl(_value, id, _colon, relationId) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });
      const value = core.value(idStr);
      context.add(idStr, value);

      if (relationId != null && relationId.children.length > 0) {
        const relation = relationId.rep();
        mustBeKindK(relation, "Relation", null, { at: id });
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

    StatementDecl(id, _colon, body) {
      const idStr = id.sourceString;
      const bodyRep = body.rep();
      mustBeStatement(bodyRep, { at: id });
      mustNotAlreadyBeDeclared(idStr, { at: id });

      const statement = core.namedStatement(idStr, bodyRep);
      context.add(idStr, statement);
      return core.statementDeclaration(statement);
    },

    InfixDecl(_infix, id, dataVar) {
      mustNotAlreadyBeDeclared(id.sourceString, { at: id });
      id = id.sourceString;
      
      const operation = dataVar.rep();
      mustNotBeNull(operation?.type, "Operation must have type", { at: dataVar} );
      mustBeKindK(operation.type, "Operation", null, {at: dataVar});

      const infix = core.infix(id, operation);
      context.add(id, infix);
      return core.infixDeclaration(infix);
    },

    Assume(_assume, id, truth, _colon, body) {
      mustBeBoolean(truth, { at: body });
      let statement = body.rep();
      context.addStatement(statement, truth);

      if (id != null) {
        const idStr = id.sourceString;
        mustNotAlreadyBeDeclared(idStr, { at: id });

        statement = core.namedStatement(idStr, statement);
        context.add(idStr, statement);
      }
      
      return core.assumptionDeclaration(statement, truth);
    },
    
    // 2. Data expressions
    DataVar(id) {
      return id.rep();
    },
    
    Statement(s) {
      const statement = s.rep();
      if (typeof(statement) === core.stringType) {
        return core.customStatement(s.sourceString); // Includes quotes for now
      }
      
      mustBeStatement(statement, {at: s});
      return statement;
    },
    
    ParenStatement(_left, s, _right) {
      return s.rep();
    },

    Equality(v1, _equals, v2) {
      const v1Rep = v1.rep();
      const v2Rep = v2.rep();
      mustBeTypeT(v1Rep, core.valueType, null, { at: v1 });
      mustBeTypeT(v2Rep, core.valueType, null, { at: v2 });
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
      
      const result = core.standardLibrary[idStr];
      
      mustNotBeNull(result, "No default infix?!", {at: id});
      return result;
    },
    Infix(id) {
      const defaultInfix = id.rep();
      if (defaultInfix?.kind === "Infix") {
        return defaultInfix;
      }
      
      const idStr = id.sourceString;
      const infix = context.lookup(idStr);
      
      mustNotBeNull(infix, "Not an infix", { at: id });
      return infix;
    },
    
    FilledOperation(id, _leftBracket, statements, _rightBracket) {
      const operation = id.rep();
      mustBeKindK(operation, "Operation", null, { at: id });
      const statementsRep = statements.rep();
      return core.filledOperation(operation, statementsRep);
    },
    FilledRelationOrProp(relOrProp, _leftBracket, args, _rightBracket) {
      const subject = relOrProp.rep();
      
      if (subject?.kind === "Relation") {
        const argsRep = [];
        for (const arg in args.asIteration().children) {
          const value = arg.rep();
          mustBeTypeT(value, core.valueType, null, {at: arg});
          argsRep.push(value);
        }
        if (subject.kind.number !== null) {
          mustHaveLength(argsRep, subject.kind.number, {at: args});
        }
        return core.filledRelation(subject, argsRep);
        
      } else if (subject?.kind === "Property") {
        const argsRep = [];
        for (const [i,arg] in args.asIteration().children.entries()) {
          const relation = arg.rep();
          mustBeKindK(relation, "Relation", null, {at: arg});
          
          if (subject.kind.numbers !== null) {
            const relationArgsNumber = subject.kind.numbers[i];
            mustNotBeNull(relationArgsNumber, "Too many relation arguments", {at: args});
            mustHaveNumber(relation.type, relationArgsNumber, {at: args});
          }
          
          argsRep.push(relation);
        }
        
        return core.filledProperty(subject, argsRep);
      
      } else {
        must(false, "Not a relation or property", { at: id });
      }
    },
    
    Negation(_not, statementId) {
      let statement = statementId.rep();
      mustBeStatement(statement, { at: statementId });
      
      if (statement.kind === "NamedStatement") {
        statement = statement.inner;
      }
      
      if (isBoolean(statement)) {
        return !statement;
      }
      
      if (statement.kind === "Negation") {
        return statement.inner;
      }
        
      return core.negation(statement);
    },
    
    
    // 3. Types
    defaultType(id) {
      const idStr = id.sourceString;
      if (idStr === "bool") {
        return core.boolType;
      } else if (idStr === "int") {
        return core.intType;
      } else if (idStr === "string") {
        return core.stringType;
      } else {
        const type = context.lookup(idStr);
        let message = "No type?!";
        if (idStr === "Self") {
          message = "No Self type";
        }
        mustNotBeNull(type, message, { at: id });
        
        return type;
      }
      
    },

    Numbering(_leftArrow, number, _rightArrow) {
      return numerifyNonzero(number.sourceString);
    },
    MultiNumbering(_left, numbers, _right) {
      return numbers.asIteration().children.map(n => numerifyNonzero(n.sourceString));
    },
    
    NumberedRelation(_, numbering) {
      return core.relationType(numbering.rep());
    },
    NumberedOperation(_, numbering) {
      return core.operationType(numbering.rep());
    },
    NumberedProperty(_, multinumbering) {
      return core.propertyType(multinumbering.rep());
    },
    
    ArrayType(_left, fullType, _colon, number, _right) {
      const basicType = fullType.rep();
      if (number == null) {
        return core.listType(basicType);
      }
      return core.arrayType(basicType, number.rep());
    },
    
    CustomType(id) {
      const idStr = id.sourceString;
      let type = context.lookup(idStr);
      
      mustNotBeNull(type, "No type found", { at: id });
      mustBeCustomType(type, false, { at: id }); // tho allowStd == false is required by parser anyway
      return type;
    },
    
    ColonSuperClass(_colon, superClass) {
      return superClass.rep();
    },
    SuperClass(firstType, _plus, rest) {
      const types = [firstType];
      for (const type of rest.children) {
        if (type.sourceString != null) {
          types.push(type.child(1));
        }
      }
      
      //must(types.length !== 0, "Expected at least one type?!", { at: firstType });
      
      const typesRep = [];
      for (const type of types) {
        typesRep.push(type.rep());
      }
      
      if (typesRep.length === 1 && typesRep[0]?.kind !== "Class") {
        return typesRep[0];
      }
      
      for (const type in typesRep) {
        mustBeKindK(type, "Class", null, { at: firstType });
      }
      const classObjectType = core.classObjectType(types);
      for (const type in typesRep) {
        context.classify(classObjectType, type);
      }
      return classObjectType;
    },
    
    ReturnType(_arrow, fullType) {
      return fullType.rep();
    },
    FullType(mutref, basicType) {
      const type = basicType.rep();
      mustBeAType(type, { at: basicType });
      
      if (mutref.sourceString === "mut") { // TODO: Check
        return core.mutRefType(type);
      }
      
      return type;
    },
        
    // 4. Expressions in mods
    Literal(l) {
      return l.rep();
    },
    Readable(r) {
      const rep = r.rep();
      mustBeObject(rep, "?!", { at: r });
      
      return rep;
    },
    Expr(mutable, readable) {
      let obj = readable.rep();
      
      const content = getMutRefContent(obj);
      
      if (mutable.sourceString === "mut") {
        must(content != null, {at: mutable }); // Must be variable or field
        
        if (obj.type?.kind === "MutRefType") {
          // Then obj is a variable containing a mutref to another variable
          obj = content.variable;
        }
        
        return core.mutRef(obj);
      }
      
      if (content != null) {  // is variable or field
        
        obj = content;
        
        if (obj?.kind === "MutRef") {
          obj = obj.variable;
          
          const content2 = getMutRefContent(obj);
          mustNotBeNull(content2, "Non-variable inside mutref?!", {at: readable});
          return content2;
        }
      }
      
      return obj;
    },
    
    Construct(id, _brack1, exprs, _brack2) {
      const rep = id.rep();
      const args = exprs.asIteration().children.map(e => e.rep());
      
      if (rep?.kind === "Enum") {
        mustHaveLength(args, 1);
        
        const enumCase = enumeration.cases.find(c => compatible(c.type, rep.type));
        mustNotBeNull(enumCase, "No case found", {at: id});
        
        return core.enumCase(rep, args[0], enumCase);
        
      } else {
        mustBeKindK(rep, "Struct", null, {at: id});
        for (const [index, field] in rep.fields.entries()) {
          mustBeAssignable(field, args[index].type, { at: id });
        }
        mustHaveEqualLength(rep.fields, args, { at: id });
        
        return core.construct(rep, args);
      }
      
    },
    
    ArrayNumber(_colon, inner) {
      let num = inner.rep();
      if (isVariable(num)) {
        num = num.content;
      }
      
      mustBeInteger(num, { at: inner });
      return num;
    },
    
    LogosArray(a) {
      return a.rep();
    },
    FullArray(_left, readables, number, _right) {
      const contents = readables.asIteration().children.map(r => r.rep());
      if (contents.length === 0) {
        return core.emptyArray();
      } else {
        mustAllHaveSameType(contents, {at: readables});
        return core.logosArray(contents);
      }
    },
    
    CopiedArray(_left, readable, number, _right) {
      let len = number.rep();
      if (isVariable(len)) {
        mustBeInteger(len);
        len = len.content;
      }
      // Fortunately, Array.fill() does not clone, only copies reference
      const contents = Array(len).fill(readable.rep());
      return core.logosArray(contents);
    },
    
    Variable(v) {
      return v.rep();
    },
    AssignedVariable(name) {
      const variable = context.lookup(name.sourceString);
      //mustBeAssignedVar(variable, { at: name }); // Can be data-declared
      mustNotBeNull(variable, "No variable found", {at: name});
      return variable; 
    },

    VarField(variable, _dot, fieldId) {
      let called = variable.rep();
      const fieldStr = fieldId.sourceString; // .trim()?
      
      const calledType = called.type;
      mustNotBeNull(calledType, "Var has no type?!", { at: variable });
      
      
      

        if (fieldStr === "len") {
          if (called.type?.kind === "ArrayType") {
            return called.type.len;
          } else {
            if (called?.kind === "Operation" || called?.kind === "Relation") {
              return called.type.number;
            }
            
            must(false, "Expected array", {at: variable}); // Not list
          }
        } else if (fieldStr === "lens") {
          mustBeKindK(called?.kind, "Property", null, {at: variable});
          return called.type.numbers;
        }
        
        if (called?.type?.kind === "Struct") {
          let i = 0;
          let field = null;
          for (const f of called.type.fields) {
            if (f.name === fieldStr) {
              field = f;
              break;
            }
            i++;
          }
          
          mustNotBeNull(field, "No such field", {at: variable});
          
          const content = getMutRefContent(called); // variable content or variable field content
          if (content != null) {
            called = content;
          }
          
          // VarField is a misnomer. It's actually an expression field
          // We store the content inside, not the variable
          // to make this easier, because we allow expressions' fields to be called
          return core.varField(called, field, i);
          
        } else {
          // Must be list index
          
          const number = numerify(fieldStr, { at: fieldId });
          
          mustHaveIndex(called, number, {at: fieldId});
          let type = called.basicType;
          if (isString(called)) {
            type = core.stringType;
          }
          
          return core.varIndex(called, number, type);
        }
    },

    // Misnomer: Actually a field, or a case for enums
    Param(id, _colon, type) {
      let idStr = "0";
      if (id != null) {
        idStr = id.sourceString.slice(0,-1); // remove ":" at the end
      }
      const param = core.field(idStr, type.rep());
      return param;
    },
    
    Call(_left, args, _right) {
      return args.asIteration().children.map(a => a.rep());
    },

    ModCall(id, args) {
      const idStr = id.sourceString;
      const module = context.lookup(idStr);
      
      mustNotBeNull(module, "No module found");
      mustBeKindK(module, "Module", "Expected a module", {at: id});
      
      const modCall = core.modCall(module, args.rep());
      
      mustHaveEqualLength(module.params, modCall.args);
      for (const [i, arg] in modCall.args.entries()) {
        mustBeAssignable(arg, module.params[i].type);
      }
      
      if (module.name === "drop") {
        mustAlreadyBeDeclared(modCall.args[0], {at: args});
        context.remove(idStr);
      }
      
      return modCall;
    },

    MethodCall(varOrType, _dot, methodId, args) {
      const subj = varOrType.rep();
      const methodStr = methodId.sourceString;
      const argsRep = args.rep();
      
      let method = null;
      let methodCall = null;
      if (subj?.isType === true) {
        mustBeKindK(subj, "Struct", null, {at: varOrType});
        
        method = subj.methods.find(m => m.name === methodStr);
        must(method.mutSelf == null, "Method takes self parameter", {at: methodId});
        
        methodCall = core.associatedMethodCall(subj, method, argsRep);
      } else {
        let subjType = subj.type;
        if (subjType?.kind === "MutRefType") {
          subjType = subj.content.variable.type;
        }
        
        //mustBeCustomType(subjType, true, {at: varOrType});
        must(subjType?.kind !== "Enum", "Enum has no methods", {at: varOrType});
        
        const method = findMethod(subjType, methodStr, {at: methodId});
        
        // Check mut self 
        mustNotBeNull(method.mutSelf, "Method must take self parameter", { at: methodId});
        if (method.mutSelf === true) {
          mustBeKindK(subj.type, "MutRefType", null, {at: varOrType});
        }
        // If method is not mutSelf, can use a mut still
        
        methodCall = core.methodCall(subj, method, argsRep);
      }
      
      mustHaveEqualLength(method.params, methodCall.args);
      for (const [i, arg] in methodCall.args.entries()) {
        mustBeAssignable(arg, method.params[i].type);
      }
      
      return methodCall;
    },

    // 5. Type declarations
    StructDecl(
      _struct,
      id,
      _leftBracket,
      parameters,
      _rightBracket
    ) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });

      // Allow recursion (but not for the type params)
      const struct = core.struct(idStr, []);
      context.add(idStr, struct);

      // Analyze parameters with typeParams
      context = context.newChildContext({
        inLoop: false,
        module: struct,
      });
      context.add("Self", struct);

      const params = parameters.asIteration().children.map((p) => p.rep());
      let i = 0;
      for (const p in params) {
        if (p.name === "0") {
          p.name = i.toString();
          i++;
        }
      }
      mustBeDistinct(params, "Fields must be distinct", {at: id});
      struct.fields = params;
      mustNotBeSelfContaining(struct, { at: id });

      context = context.parent;
      return core.structDeclaration(struct);
    },

    EnumDecl(
      _enum,
      id,
      _leftBracket,
      parameters,
      _rightBracket
    ) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });

      const enumeration = core.enumeration(idStr, []);
      context.add(idStr, enumeration);

      context = context.newChildContext({
        inLoop: false,
        module: enumeration,
      });
      context.add("Self", enumeration);

      const params = parameters.asIteration().children.map((p) => p.rep());
      let i = 0;
      for (const p in params) {
        if (p.name === "0") {
          p.name = i.toString();
          i++;
        }
      }
      mustBeDistinct(params, "Cases must be distinct", {at: id});
      enumeration.cases = params;
      casesMustHaveDistinctTypes(enumeration, {at: id});
      enumMustNotBeSelfContaining(enumeration, { at: id });

      context = context.parent;
      return core.enumDeclaration(enumeration);
    },

    // 6. Modules
    SelfParam(mut, _self) {
      return (mut != null);
    },
    ModParam(id, _colon, type) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });
      return core.parameter(idStr, type.rep());
    },
    ModParamWithSelf(m) {
      return m.rep();
    },
    ModHead(_paren1, params, _paren2, returnType) {
      let returnTypeRep = core.voidType;
      if (returnType != null && returnType.children.length !== 0) {
        returnTypeRep = returnType.child(0).rep();
      }
      
      const mod = core.module(
        null,
        null,
        [],
        returnTypeRep,
        null
      );
      
      for (const param of params.asIteration().children) {
        const paramRep = param.rep();
        if (isBoolean(paramRep)) {
          if (mod.mutSelf == null) {
            mod.mutSelf = paramRep;
          } else {
            must(false, "Cannot use self repeatedly", { at: param });
          }
        } else {
          mod.params.push(paramRep);
        }
      }
      
      return mod;
    },
    
    ModBody(_brack1, actionLines, _brack2) {
      const modBody = core.modBody([], core.voidType);
      
      context = context.newChildContext({
        inLoop: false,
        module: modBody
      });
      
      for (const action of actionLines.children) {
        const actionRep = action.rep();
        mustBeAction(actionRep, { at: action });
        modBody.actions.push(actionRep);
        // returnLine sets type/returnType via context
        // This allows us to use closures as objects
      }
      
      context = context.parent;
      
      
      return modBody;
    },
    
    ModDecl(_mod, id, head, body) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });
      
      const mod = head.rep();
      must(mod.mutSelf == null, "No self parameter", { at: head }); 
      
      mod.name = idStr;
      context.add(idStr, mod); // Allow recursion

      context = context.newChildContext({
        inLoop: false,
        module: mod,
      });
      for (const param of mod.params) {
        context.add(param.name, param);
      }
      
      const bodyRep = body.rep();
      mod.body = bodyRep;

      context = context.parent;
      
      must(
        compatible(bodyRep.returnType, mod.returnType), 
        "Incorrect type returned from closure", 
        {at: body}
      );
      
      return core.moduleDeclaration(mod);
    },
    
    
    // 7. Classes and methods

    MethodDecl(_mod, customType, _dot, id, head, body) {
      const struct = customType.rep();
      mustBeKindK(struct, "Struct", null, { at: customType });
      
      const idStr = id.sourceString;
      const contextualName = struct.name + "." + idStr;
      mustNotAlreadyBeDeclared(contextualName, { at: id });
      
      const mod = head.rep();
      mod.name = idStr;
      struct.methods.push(mod);
      context.add(contextualName, mod);

      context = context.newChildContext({
        inLoop: false,
        module: mod,
      });
      
      for (const param in mod.params) {
        mustNotAlreadyBeDeclared(param.name, { at: head });
        context.add(param.name, param);
      }
      if (mod.mutSelf != null) {
        const selfParam = core.parameter("self", struct);
        if (mod.mutSelf === true) {
          core.parameter.type = core.mutRefType(struct);
        }
        context.add("self", selfParam);
      }
      context.add("Self", struct); // Self type
      
      const bodyRep = body.rep();
      mod.body = bodyRep;

      context = context.parent;
      return core.methodDeclaration(struct, mod);
    },

    ClassMod(_mod, id, head, body) {
      let selfType = core.selfType();
      if (context.module?.kind === "ClassImpl") {
        selfType = context.module.subjectType;
      }
      
      const idStr = id.sourceString;
      let subjName = context.module.name;
      if (context.module.kind === "ClassImpl") {
        subjName = context.module.subjectType.name;
      }
      const contextualName = subjName + "." + idStr;
      mustNotAlreadyBeDeclared(contextualName, { at: id });

      const mod = head.rep();
      mod.name = idStr;
      context.add(contextualName, mod);
      
      context.module.modules.push(mod); // whether Class or ClassImpl
      if (context.module.kind === "ClassImpl") {
        mustHaveModule(context.module.classs, mod);
        context.module.subjectType.methods.push(mod);
      }

      context = context.newChildContext({
        inLoop: false,
        module: mod,
      });
      for (const param in mod.params) {
        context.add(param.name, param);
      }
      if (mod.mutSelf != null) {
        const selfParam = core.parameter("self", selfType);
        if (mod.mutSelf === true) {
          core.parameter.type = core.mutRefType(selfType);
        }
        context.add("self", selfParam);
      }
      context.add("Self", selfType);

      let bodyRep = null;
      if (body.sourceString !== ";") {
        bodyRep = body.rep();
      }
      mod.body = bodyRep;
      if (context.module.kind === "ClassImpl") {
        mustNotBeNull(bodyRep, "Must have body", {at: body});
      }

      context = context.parent;
      return mod;
    },
    
    ClassBody(_left, classMods, _right) {
      // All checks are in ClassMod
      return classMods.children.map(m => m.rep());
    },

    ClassDecl(_class, id, superClass, classBody) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, { at: id });

      const classs = core.classs(idStr, []);
      context.add(idStr, classs);
      
      const superClassRep = superClass.rep();
      mustBeKindK(superClassRep, "ClassObjectType", null, {at: superClass});
      const superClasses = superClassRep.classes;
      for (const superC in superClasses) {
        context.classify(classs, superC);
      }

      context = context.newChildContext({
        inLoop: false,
        module: classs,
      });
      
      classs.modules = classBody.rep();

      context = context.parent;
      

      // Check superclasses
      for (const superC in superClasses) {
        // classs.modules are filtered by whether the superclass has them, not whether the body in the superclass is null
        // allowing override
        const implModules = classs.modules.filter(mod => hasModule(superC, mod));
        
        const impl = core.classImpl(classs, superC, implModules);
        
        mustNotAlreadyBeDeclared(impl.name, {at: superClass});
        context.add(impl.name, impl);
        
        mustImplAllModules(impl, superC, {at: superClass});
      }
      
      return core.classDeclaration(classs);
    },

    ClassImpl(structId, _impl, classId, body) {
      const struct = structId.rep();
      mustBeKindK(struct, "Struct", null, {at: structId});
      
      const classs = classId.rep();
      mustBeKindK(classs, "Class", null, {at: classId});
      
      const impl = core.classImpl(struct, classs, []);
      mustNotAlreadyBeDeclared(impl.name, {at: structId});
      context.add(impl.name, impl);

      context.classify(struct, classs);

      context = context.newChildContext({
        inLoop: false,
        module: impl,
      });
      
      impl.modules = body.rep();
      
      mustImplAllModules(impl, classRep, { at: type });

      context = context.parent;
      return impl;
    },
    // TODO: Check modbody return type
    
    // 8. Actions
    
    Action(act) {
      return act.rep();
    },

    ActionLine(action, _semicolon) {
      return action.rep();
    },

    Assignment(id, _eq, expr) {
      const idStr = id.sourceString;
      const subj = context.lookup(idStr);
      if (isData(subj)) {
        must(isVariable(subj), "Cannot assign to data", {at: id});
      }
      
      let obj = expr.rep();
      // All conversion of muts and all is taken care of in Expr
      
      if (subj == null) {
        const newVar = core.variable(idStr, obj);
        context.add(newVar.name, newVar);
        return core.variableDeclaration(newVar);
      } else {
        mustBeAssignable(obj, subj.type, {at: id} );
        
        return core.assignment(subj, obj);
        // At the next stage:
        // x: bool = b: bool --> x.content = b;
        // x: mut bool = b: bool --> x.variable.content = b;
        // x: bool = b: mut bool --> x.content = read(b);
        // x: mut bool = b: mut bool --> x.variable = b.variable
      }
    },

    Return(returnOrYield, degreeUp, expr) {
      mustBeInAModule({ at: returnOrYield });
      const yielding = (returnOrYield.sourceString === "yield");
      
      
      let content = core.nullObject();
      if (expr.children.length > 0) {
        content = expr.child(0).rep();
        mustBeObject(content, null, {at: expr});
      }
      
      let contentType = content.type;
      if (yielding) {
        contentType = core.listType(contentType);
      }
      
      let modBodyContext = context;
      if (degreeUp == null) {
        // Go to highest mod
        while (modBodyContext.parent?.module?.kind === "ModBody") {
          modBodyContext = modBodyContext.parent;
        }
      } else {
        let d = degreeUp.rep();
        //mustNotBeNull(d, "Degree cannot be null?!", {at: degreeUp});
        while (d > 0) {
          mustNotBeNull(modBodyContext.parent, "Too high a degree to return", {at: degreeUp});
          modBodyContext = modBodyContext.parent;
          d--;
        }
      }
      
      const modBody = modBodyContext.module;
      if (modBody.returnType == core.voidType) { 
        modBody.returnType = contentType;
      } else {
        if (!compatible(contentType, modBody.returnType)) {
          must(
            compatible(modBody.returnType, contentType), 
            "Incompatible return type", 
            {at: expr}
          );
          modBody.returnType = contentType;
        }
      }
      
      if (yielding) {
        return core.yieldLine(content, modBody);
      } else {
        return core.returnLine(content, modBody);
      }
    },
    
    change(pm) {
      return pm.sourceString === "++";
    },

    Crement(id, change) {
      const variable = context.lookup(id.sourceString);
      mustBeInteger(variable, { at: id });
      if (change.rep()) {
        return core.increment(variable);
      } else {
        return core.decrement(variable);
      }
    },
    
    BreakLine(breaks) {
      mustBeInLoop(true, {at: breaks});
      let n = breaks.children.length;
      let modBodyContext = context;
      while (n > 0) {
        if (context.inLoop) {
          n = n - 1;
        }
        modBodyContext = context.parent;
        mustNotBeNull(modBodyContext, "Too many breaks", {at: breaks});
        if (modBodyContext.module?.kind !== "ModBody") {
          must(false, "Too many breaks", {at: breaks});
        }
      }
      return core.breakLine(modBodyContext.module);
    },
    
    ContinueLine(conts) {
      mustBeInLoop(true, {at: conts});
      let n = conts.children.length;
      let modBodyContext = context;
      while (n > 0) {
        if (context.inLoop) {
          n = n - 1;
        }
        modBodyContext = context.parent;
        mustNotBeNull(modBodyContext, "Too many continues", {at: conts});
        if (modBodyContext.module?.kind !== "ModBody") {
          must(false, "Too many continues", {at: conts});
        }
      }
      return core.continueLine(modBodyContext.module);
    },
    
    
    // 9. ControlFlow and EvalBool
    Collection(x) {
      const rep = x.rep();
      mustBeObject(rep, null, {at: x});
      if (isArrayOrList(rep?.type) !== true) {
        mustHaveClass(rep?.type, "Collection", {at: x});
      }
      return rep;
    },
    InEqual(id) {
      return core.comparison(id.sourceString);
    },
    InEqualities(exp1, cmp, exp2) {
      const e1 = exp1.rep();
      const e2 = exp2.rep();
      const c = cmp.rep();
      
      mustHaveClass(e1?.type, c.classReq);
      mustHaveClass(e2?.type, c.classReq);
      mustBothHaveSameType(e1, e2);
      
      return core.inEquality(exp1.rep(), comp1.rep(), exp2.rep());
    },
    Not(_exclam, exp1) {
      return core.notVariable(exp1.rep());
    },
    And(exp1, _and, exp2) {
      return core.andVariable(exp1.rep(), exp2.rep());
    },
    Or(exp1, _or, exp2) {
      return core.orVariable(exp1.rep(), exp2.rep());
    },
    ParenEvalBool(_left, exp1, _right) {
      return exp1.rep();
    },
    EvalBool(e) {
      const rep = e.rep();
      mustBeBoolean(rep, {at: e});
      return rep;
    },
    
    ElseFlow(_else, action) {
      return action.rep();
    },
    IfFlow(_if, condition, _then, action, alternate) {
      const act = action.rep();
      const ifFlow = core.ifFlow(condition.rep(), act, null);
      if (alternate == null) {
        return ifFlow;
      }
      
      const alt = alternate.rep();
      if (!compatible(alt.type, act.type)) {
        if (compatible(act.type, alt.type)) {
          ifFlow.type = alt.type;
        } else {
          must(false, "Incompatible types", {at: alternate});
        }
      }
      ifFlow.alternate = alt;
      return ifFlow;
    },
    ForFlow(_for, id, _in, collection, _do, action) {
      const idStr = id.sourceString;
      mustNotAlreadyBeDeclared(idStr, {at: id});
      
      const coll = collection.rep();
      
      let first = null;
      if (isArrayOrList(coll?.type)) {
        if (coll?.kind === "EmptyArray" || coll?.kind === "EmptyList" ) {
          first = core.nullObject();
        } else {
          first = coll.contents[0];
        }
      } else {
        first = core.methodCall(
          coll,
          findMethod(coll.type, "get", {at: collection}),
          [0]
        );
      }
      
      // Can't create new child context cuz it should only be "modBody"s (for return & break)
      context.add(idStr, core.variable(idStr, first));
      const act = action.rep();
      context.remove(idStr);
      
      return core.forFlow(idStr, coll, act);
    },
    WhileFlow(_while, condition, _do, action) {
      return core.whileFlow(condition.rep(), action.rep());
    },
    IfEvalBool(_if, condition) {
      return condition.rep();
    },
    MatchStart(x) {
      let condition = x.rep();
      if (condition?.type !== core.boolType) {
        condition = core.matchConditionType(rep.type);
      }
      return condition;
    },
    MatchLine(conditions, _arrow, action) {
      return core.matchLine(conditions.asIteration().children.map(c => c.rep()), action.rep());
    },
    MatchFlow(_match, variable, _colon, matchLines) {
      return core.matchFlow(variable.rep(), matchLines.asIteration().children.map(l => l.rep()));
    },
    
    
    // 10. Literals
    
    none(_) {
      return core.nullObject();
    },
    true(_) {
      return true;
    },
    false(_) {
      return false;
    },
    number(digits) {
      return core.logosInt(numerify(this.sourceString, {at: digits}));
    },
    string(_quote1, _chars, _quote2) {
      return core.logosString(this.sourceString); // includes quotes for now
    }
  });

  return builder(match).rep();
}
