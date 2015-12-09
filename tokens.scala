package lang

/*----------*/
/*  Traits  */
/*----------*/

//Tokens
sealed trait Token

//AST
sealed trait Type
sealed trait Expr
sealed trait Stmt

/*-----------*/
/*  Tokens   */
/*-----------*/

//Keywords
case object AndTok extends Token     // and
case object OrTok extends Token      // or
case object NotTok extends Token     // not
case object IfTok extends Token      // if
case object ElseTok extends Token    // else 
case object ForTok extends Token     // for
case object WhileTok extends Token   // while
case object ReturnTok extends Token  // return
case object MatchTok extends Token   // match
case object CaseTok extends Token    // case
case object NullTok extends Token    // null
case object TrueTok extends Token    // true
case object FalseTok extends Token   // false

//Symbols
case object AssgnTok extends Token       // =
case object LTTok extends Token          // <
case object GTTok extends Token          // >
case object LTEqTok extends Token         // <=
case object GTEqTok extends Token         // >=
case object LParenTok extends Token      // (
case object RParenTok extends Token      // )
case object LCurlTok extends Token       // {
case object RCurlTok extends Token       // }
case object LSquareTok extends Token     // [
case object RSquareTok extends Token     // ]
case object SemiColTok extends Token     // ;
case object MultTok extends Token        // *
case object AddTok extends Token        // +
case object SubTok extends Token       // -
case object DivTok extends Token       // /
case object MultEqTok extends Token      // *=
case object AddEqTok extends Token      // +=
case object SubEqTok extends Token     // -=
case object DivEqTok extends Token     // /=
case object ConstTok extends Token       // `   
case object ModTok extends Token        // %
case object ExpTok extends Token         // **
case object EqualTok extends Token       // ==
case object NotEqualTok extends Token    // !=
case object CommaTok extends Token       // ,
case object ColonTok extends Token       // :
case object PrintTok extends Token
case object ArrayTok extends Token
case object ArrowTok extends Token 

//Types
case object NumType extends Token with Type
case object StrType extends Token with Type
case object BoolType extends Token with Type
case object VoidType extends Token with Type
case class ArrayType(typ: Type) extends Token with Type

//Literals
case class Str(str: String) extends Token with Expr
case class Num(num: Int) extends Token with Expr
case class Ident(name: String) extends Token with Expr
case class Bool(bool: Boolean) extends Token with Expr
case class Arr(arr: Option[List[Expr]]) extends Token with Expr

/*------------------------*/
/*  Abstract Syntax Tree  */
/*------------------------*/

//Expressions
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Mult(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Mod(left: Expr, right: Expr) extends Expr
case class Expon(left: Expr, right: Expr) extends Expr
case class GT(left: Expr, right: Expr) extends Expr
case class LT(left: Expr, right: Expr) extends Expr
case class GTE(left: Expr, right: Expr) extends Expr
case class LTE(left: Expr, right: Expr) extends Expr
case class Equal(left: Expr, right: Expr) extends Expr
case class NotEqual(left: Expr, right: Expr) extends Expr
case class Or(left: Expr, right: Expr) extends Expr
case class And(left: Expr, right: Expr) extends Expr
case class Not(expr: Expr) extends Expr
case class Neg(expr: Expr) extends Expr
case class FnCall(id: Ident, args: List[Expr]) extends Expr
case class Pattern(itm: Expr, cases: List[Case]) extends Expr
case class Case(valu: Expr, retVal: Expr)


//Statements
case class Assign(id: Expr, expr: Expr) extends Stmt
case class Ret(expr: Expr) extends Stmt
case class Body(stuff: List[Stmt]) extends Stmt
case class While(cond: Expr, bod: Stmt) extends Stmt
case class For(dec: Stmt, cond: Expr, count: Stmt, bod: Stmt) extends Stmt
case class If(cond: Expr, bod: Stmt, pElse: Option[Stmt])  extends Stmt
case class ExprAsStmt(expr: Expr) extends Stmt
case class Print(data: Expr) extends Stmt

//Declarations
case class VarDef(typ: Type, id: Expr, expr: Expr) extends Stmt
case class ConstDef(typ: Type, id: Expr, expr: Expr) extends Stmt
case class FnDef(typ: Type, id: Expr, args: List[Argument], bod: Stmt) extends Stmt

//Programs
case class Prog(fndef: FnDef, fncall: Stmt)

/*--------------*/
/*  Eval Types  */
/*--------------*/

//Evaluation
sealed trait Value

//Values
case class IntVal(n: Int) extends Value
case class BoolVal(b: Boolean) extends Value
case class StrVal(s: String) extends Value 
case class ArrayVal(a: Array[Value]) extends Value
case object VoidVal extends Value

//Arguments
case class Argument(ident: Expr, typ: Type, cbvr: Boolean = false)

//Closure
case class Closure(retType: Type, params: List[Argument], body: Stmt, var env: Map[String, Location], parent: String) extends Value

//Location
class Location(value: Value, const: Boolean = false) {
    private var contents = value
    val isConst = const
    def get: Value = contents
    def set(newVal: Value) {
        if (!isConst)
            contents = newVal
        else
            throw new Exception("Can't change a constant!")
    }
}