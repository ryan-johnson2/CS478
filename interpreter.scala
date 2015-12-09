package lang

object Interpreter {

    def interpret(body: Prog) = {
        //Parser packages programs in a function with an empty name, so we don't need any global memory
        //For testing only
        //Reserved: RETURN, maps to current return value
        type Env = Map[String, Location]
        
        case class ReturnInt(retInt: String) extends Exception(retInt)
        case class ReturnBool(retBool: String) extends Exception(retBool)
        case class ReturnStr(retStr: String) extends Exception(retStr)
        
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
            case GT(left, right) => BoolVal(evalBool(expr, env))
            case LT(left, right) => BoolVal(evalBool(expr, env))
            case GTE(left, right) => BoolVal(evalBool(expr, env))
            case LTE(left, right) => BoolVal(evalBool(expr, env))
            case Equal(left, right) => BoolVal(evalBool(expr, env))
            case NotEqual(left, right) => BoolVal(evalBool(expr, env))
            case Or(left, right) => BoolVal(evalBool(expr, env))
            case And(left, right) => BoolVal(evalBool(expr, env))
            case Not(bool) => BoolVal(evalBool(expr, env))
            case Ident(name) => env.getOrElse(name, throw new Exception("Unassigned variable")).get
            case FnCall(id, args) =>
                println("Function call to " + id.name)
                println(env)
                env(id.name).get match {
                    case Closure(retType, params, body, fnEnv, parent) =>
                        println("Function environment")
                        println(fnEnv)
                        var curEnv = env
                        val inputs = args.zip(params)
                        if (args.size != params.size) throw new Exception("Incorrect number of parameters!")                
                        for ((arg, param) <- inputs) {
                            val paramName = param.ident match {
                                case Ident(s) => s; 
                                case _ => throw new Exception("NOPE TRY AGAIN")
                            }
                            val argVal = eval(arg, fnEnv)
                            if (!(matchType(argVal, param.typ))) throw new Exception("Types don't match!")
                            else curEnv += (paramName -> new Location(argVal))
                        }
                        try {exec(body, curEnv, id.name)}
                        catch {
                            case e: ReturnInt => return IntVal(e.getMessage.toInt)
                            case e: ReturnBool => return BoolVal(e.getMessage.toBoolean)
                            case e: ReturnStr => return StrVal(e.getMessage)                    
                        }
                        return VoidVal
                    case _ => throw new Exception("Not a function!")
                }
                
            case _ => throw new Exception("Not a valid expression")
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
            case GT(left, right) => evalNum(left, env) > evalNum(right, env)
            case LT(left, right) => evalNum(left, env) < evalNum(right, env)
            case GTE(left, right) => evalNum(left, env) >= evalNum(right, env)
            case LTE(left, right) => evalNum(left, env) <= evalNum(right, env)
            case Equal(left, right) => eval(left, env) == eval(right, env)
            case NotEqual(left, right) => eval(left, env) != eval(right, env)
            case Or(left, right) => evalBool(left, env) || evalBool(right, env)
            case And(left, right) => evalBool(left, env) && evalBool(right, env)
            case Not(bool) => !evalBool(bool, env)
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
        
        def getArgName(arg: Argument): String = arg.ident match {
            case Ident(name) => name
            case _ => throw new Exception("Not a candidate for CBVR")
        }

        def exec(stmt: Stmt, curEnv: Env, curFn: String): Env = {
            var env = curEnv
            stmt match {
                case Ret(expr) => // Change to use exceptions
                    val ans = eval(expr, env)
                    ans match {
                        case IntVal(n) => throw new ReturnInt(n.toString)
                        case BoolVal(b) => throw new ReturnBool(b.toString)
                        case StrVal(s) => throw new ReturnStr(s)
                        case _ => throw new Exception("Not a supported type!")
                    }
                case Body(stmts) => 
                    for (stmt <- stmts) env = exec(stmt, env, curFn)
                case While(cond, bod) => 
                    while (evalBool(cond, env)) env = exec(bod, env, curFn)
                case For(dec, cond, count, bod) =>
                    env = exec(dec, env, curFn)
                    while (evalBool(cond, env)) {
                        env = exec(bod, env, curFn)
                        env = exec(count, env, curFn)
                    }
                case If(cond, body, pElse) =>
                    if (evalBool(cond, env))
                        env = exec(body, env, curFn)
                    else for (elseStmt <- pElse) env = exec(elseStmt, env, curFn)
                case Assign(id, value) => id match {
                    case Ident(s) => env.getOrElse(s, throw new Exception(id + " not defined!")).set(eval(value, env))
                    case _ => throw new Exception("NO")}
                case VarDef(typ, id, expr) => id match {
                    case Ident(s) => env += (s -> new Location(eval(expr, env)))
                    case _ => throw new Exception("NO")}   
                case ConstDef(typ, id, expr) => id match {
                    case Ident(s) => env += (s -> new Location(eval(expr, env), true))
                    case _ => throw new Exception("NO")}                    
                case FnDef(typ, id, args, bod) => id match {
                    case Ident(s) => 
                    val newEnv = env + (s -> new Location(Closure(typ, args, bod, env, curFn)))
                    env += (s -> new Location(Closure(typ, args, bod, newEnv, curFn)))
                    case _ => throw new Exception("NO")} 
                case ExprAsStmt(expr) => eval(expr, env)
                case Print(d) => 
                    println("Print environment")
                    println(env)
                    println()
                    val ans = eval(d, env)
                    println(ans)
                case _ => println("Broken")
            }
            //env.foreach(x => println(x._1, x._2.get))
            //println(env)
            env
        }
        
        val mainEnv = exec(body.fndef, Map.empty[String, Location], "")
        //println("Main environment")
        //println(mainEnv)
        val e = exec(body.fncall, mainEnv, "")
        //testRet.reverse
        //val e = exec(body, Map.empty[String, Location], "")
    }
}