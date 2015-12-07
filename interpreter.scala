package lang

object Interpreter {

    def interpret(prog: Prog) = {
        //Parser packages programs in a function with an empty name, so we don't need any global memory
        //For testing only
        var testRet = List.empty[Any]

        def matchType(value: Value, typ: Type): Boolean = value match {
            case IntVal(n) => typ == NumType
            case StrVal(s) => typ == StrType
            case BoolVal(b) => typ == BoolType
            case _ => throw new Exception("Unsupported type")
        }

        def eval(expr: Expr, env: Env): Value = expr match {
            case Num(num) => IntVal(evalNum(expr, env))
            case Add(left, right) => IntVal(evalNum(expr, env))
            case Sub(left, right) => IntVal(evalNum(expr, env))
            case Mult(left, right) => IntVal(evalNum(expr, env))
            case Div(left, right) => IntVal(evalNum(expr, env))
            case Mod(left, right) => IntVal(evalNum(expr, env))
            case Expon(left, right) => IntVal(evalNum(expr, env))
            case Neg(num) => IntVal(evalNum(expr, env))
            case Str(str) => StrVal(evalStr(expr, env))
            case Bool(bool) => BoolVal(evalBool(expr, env))
            case PGT(left, right) => BoolVal(evalBool(expr, env))
            case PLT(left, right) => BoolVal(evalBool(expr, env))
            case PGTE(left, right) => BoolVal(evalBool(expr, env))
            case PLTE(left, right) => BoolVal(evalBool(expr, env))
            case PEqual(left, right) => BoolVal(evalBool(expr, env))
            case PNotEqual(left, right) => BoolVal(evalBool(expr, env))
            case POr(left, right) => BoolVal(evalBool(expr, env))
            case PAnd(left, right) => BoolVal(evalBool(expr, env))
            case PNot(bool) => BoolVal(evalBool(expr, env))
            case Ident(name) => env.getOrElse(name, throw new Exception("Unassigned variable")).get
            case FnCall(id, args) =>
                val inputs = args.zip(env(id.name).get.params)
                if (args.size != params.size) throw new Exception("Incorrect number of parameters!")                
                for ((arg, param) <- inputs) {
                    val argVal = eval(arg.expr, env)
                    if (!(matchType(argVal, param._2))) throw new Exception("Types don't match!")
                    else env(id.name).get.env += (param._1.name -> arg)
                }
                exec(env(id.name).get.body, id.name)
                val returnVal = fnEnv(id.name).retVal
                if (returnVal != None) returnVal.get
                else VoidVal
            case _ => throw new Exception("Not a valid expression")   
            
                // var cbvrMap = Map.empty[String, String] // All these values will be strings
                //val parentIsGlobal = (fnEnv(id.name).parent == "")
                /* for (i <- 0 until args.size) {
                    val argExpr = args(i).expr; val argIsCBVR = args(i).cbvr
                    val argi = eval(argExpr, env)
                    val parami = params(i)
                    if (!(matchType(argi, parami._2))) throw new Exception("Types don't match!")
                    else {
                        fnEnv(id.name).env += (parami._1.name -> argi)
                        if (argIsCBVR) 
                            cbvrMap += (parami._1.name -> getArgName(args(i)))
                    }
                } */
                //exec(env(id.name).get.body, id.name)
                //for ((param, argName) <- cbvrMap) {
                //    if (parentIsGlobal) globalMem += (argName -> fnEnv(id.name).env(param))
                //    else fnEnv(fnEnv(id.name).parent).env += (argName -> fnEnv(id.name).env(param))
                //} // Changes parent variables to those determined in the function; call by value result
                //val returnVal = fnEnv(id.name).retVal
                //if (returnVal != None) returnVal.get
                //else VoidVal
        }

        def evalNum(expr: Expr, env: Env): Int = expr match {
            case Num(num) => num
            case Add(left, right) => evalNum(left, env) + evalNum(right, env)
            case Sub(left, right) => evalNum(left, env) - evalNum(right, env)
            case Mult(left, right) => evalNum(left, env) * evalNum(right, env)
            case Div(left, right) => evalNum(left, env) / evalNum(right, env)
            case Mod(left, right) => evalNum(left, env) % evalNum(right, env)
            case Expon(left, right) => math.pow(evalNum(left, env), evalNum(right, env)).toInt
            case Neg(num) => -(evalNum(num, env))
            case Ident(name) => eval(expr, env) match {
                case IntVal(n) => n
                case _ => throw new Exception("Not a num variable")
            }
            case FnCall(id, args) => eval(expr, env) match {
                case IntVal(n) => n
                case _ => throw new Exception("Not a num variable")
            }
            case _ => throw new Exception("Not a valid num expression")
        }

        def evalBool(expr: Expr, env: Env): Boolean = expr match {
            case Bool(bool) => bool
            case PGT(left, right) => evalNum(left, env) > evalNum(right, env)
            case PLT(left, right) => evalNum(left, env) < evalNum(right, env)
            case PGTE(left, right) => evalNum(left, env) >= evalNum(right, env)
            case PLTE(left, right) => evalNum(left, env) <= evalNum(right, env)
            case PEqual(left, right) => eval(left, env) == eval(right, env)
            case PNotEqual(left, right) => eval(left, env) != eval(right, env)
            case POr(left, right) => evalBool(left, env) || evalBool(right, env)
            case PAnd(left, right) => evalBool(left, env) && evalBool(right, env)
            case PNot(bool) => !evalBool(bool, env)
            case Ident(name) => eval(expr, env) match {
                case BoolVal(b) => b
                case _ => throw new Exception("Not a bool variable")
            }
            case FnCall(id, args) => eval(expr, env) match {
                case BoolVal(b) => b
                case _ => throw new Exception("Not a bool variable")
            }
            case _ => throw new Exception("Not a valid bool expression")
        }
        
        def evalStr(expr: Expr, env: Env): String = expr match {
            case Str(s) => s
            case FnCall(id, args) => eval(expr, env) match {
                case StrVal(s) => s
                case _ => throw new Exception("Not a str variable")
            }
            case _ => throw new Exception("Not a valid string expression")
        }
        
        def getArgName(arg: Argument): String = arg.expr match {
            case Ident(name) => name
            case _ => throw new Exception("Not a candidate for CBVR")
        }

        def exec(stmt: Stmt, env: Env): Env = {
            stmt match {
                case Ret(expr) => // Change to use exceptions
                    val ans = eval(expr, env)
                    ans match {
                        case BoolVal(b) => 
                            fnEnv(curFn).retVal = Some(BoolVal(b))
                            testRet = b +: testRet
                        case IntVal(n) => 
                            fnEnv(curFn).retVal = Some(IntVal(n))
                            testRet = n +: testRet
                        case StrVal(s) => 
                            fnEnv(curFn).retVal = Some(StrVal(s))
                            testRet = s +: testRet
                        case _ => throw new Exception("invalid return")
                    }
                case Body(stmts) => 
                    for (stmt <- stmts) exec(stmt, curFn)
                case PWhile(cond, bod) => 
                    while (evalBool(cond, env)) exec(bod, curFn)
                case PFor(dec, cond, count, bod) =>
                    exec(dec, curFn)
                    while (evalBool(cond, env)) {
                        exec(bod, curFn)
                        exec(count, curFn)
                    }
                case PIf(cond, body, pElse) =>
                    if (evalBool(cond, env))
                        exec(body, curFn)
                    else for (elseStmt <- pElse) exec(elseStmt, curFn)
                case Assign(id, value) => 
                    if (curFn == "") {
                        if (globalMem.get(id.name) != None) globalMem += (id.name -> eval(value, globalMem))
                        else throw new Exception(id.name + " not in global memory!")
                    }
                    else if (env.get(id.name) != None) fnEnv(curFn).env += (id.name -> eval(value, env))
                    else throw new Exception(id.name + " not in current environment!")
                    

                case ExprAsStmt(expr) => eval(expr, env)
                case _ => println("Broken")
            }
        }
        
        def declare(decl: Decl, env: Env): Env = {
            decl match {
                case VarDef(id, value) => 
                    if (curFn == "") globalMem += (id.name -> eval(value, globalMem))
                    else fnEnv(curFn).env += (id.name -> eval(value, env))
                case ConstDef(id, value) =>
                    
                case FnDef(typ, id, args, bod) =>
                    fnEnv += (id.name -> Closure(typ, None, args, bod, env, curFn))
                case _ => println("Uh oh")
            }
        }
        
        val mainEnv = declare(prog.def, Map.empty[String, Location])
        exec(prog.call, mainEnv)
        testRet.reverse
    }
}