package lang

import parsing._

object Parser extends parsing.Parsers[Token] {

    def parse(toks: List[Token]): Prog = {
        val parsed = pBody(toks)
        //parsed._1
        Prog(FnDef(VoidType, Ident(""), List.empty[Argument], parsed._1), ExprAsStmt(FnCall(Ident(""), List.empty[Expr])))
    }

    def pExpr: Parser[Expr] =
        pFuncCall |
        pIdent |
        pLiteral |
        pArray |
        LParenTok ~ pExpr7 ~ RParenTok ^^ { case _ ~ expr ~ _ => expr }

    def pExpr0: Parser[Expr] = 
        pExpr ~ (ExpTok ~ pExpr).* ^^ { case expr1 ~ list => 
            if (list.nonEmpty) Expon(expr1, convert(list)) 
            else expr1
        }

    def pExpr1: Parser[Expr] =
        SubTok ~ pExpr0 ^^ { case _ ~ expr => Neg(expr) } |
        pExpr0

    def pExpr2: Parser[Expr] = 
        pExpr1 ~ (MultTok ~ pExpr1 | DivTok ~ pExpr1 | ModTok ~ pExpr1).* ^^ { case expr1 ~ list =>
            if (list.nonEmpty) {
                val wList = (list.head._1, expr1) +: list
                retExpr(wList, list.head._1, convert(list))
            }
            else expr1
        }


    def pExpr3: Parser[Expr] = 
        pExpr2 ~ (AddTok ~ pExpr2 | SubTok ~ pExpr2).* ^^ { case expr1 ~ list => 
            if (list.nonEmpty) {
                val wList = (list.head._1, expr1) +: list
                retExpr(wList, list.head._1, convert(list))
            }
            else expr1
        }


    def pExpr4: Parser[Expr] = 
        NotTok ~ pExpr3 ^^ { case _ ~ expr => Not(expr) } | pExpr3

    def pExpr5: Parser[Expr] = 
        pExpr4 ~ LTTok ~ pExpr4 ^^ { case expr1 ~ _ ~ expr2 => LT(expr1, expr2) } |
        pExpr4 ~ GTTok ~ pExpr4 ^^ { case expr1 ~ _ ~ expr2 => GT(expr1, expr2) } |
        pExpr4 ~ LTEqTok ~ pExpr4 ^^ { case expr1 ~ _ ~ expr2 => LTE(expr1, expr2) } |
        pExpr4 ~ GTEqTok ~ pExpr4 ^^ { case expr1 ~ _ ~ expr2 => GTE(expr1, expr2) } |
        pExpr4 ~ EqualTok ~ pExpr4 ^^ { case expr1 ~ _ ~ expr2 => Equal(expr1, expr2) } |
        pExpr4 ~ NotEqualTok ~ pExpr4 ^^ { case expr1 ~ _ ~ expr2 => NotEqual(expr1, expr2) } |
        pExpr4

    def pExpr6: Parser[Expr] = 
        pExpr5 ~ (AndTok ~ pExpr5).* ^^ { case expr1 ~ list => 
            if (list.nonEmpty) And(expr1, convert(list)) 
            else expr1
        }

    def pExpr7: Parser[Expr] = 
        pExpr6 ~ (OrTok ~ pExpr6).* ^^ { case expr1 ~ list => 
            if (list.nonEmpty) Or(expr1, convert(list)) 
            else expr1
        }

    def pLiteral: Parser[Expr] = 
        any.filter(_.isInstanceOf[Str]) ^^ { case str => tok2expr(str)} |
        any.filter(_.isInstanceOf[Num]) ^^ { case num => tok2expr(num)} |
        any.filter(_.isInstanceOf[Bool]) ^^ { case bool => tok2expr(bool) }

    def pIdent: Parser[Expr] = 
        any.filter(_.isInstanceOf[Ident]) ^^ { case ident => tok2ident(ident) }

    def pFuncCall: Parser[Expr] = 
        any.filter(_.isInstanceOf[Ident]) ~ LParenTok ~ (pExpr7 ~ (CommaTok ~ pExpr7).*).? ~ RParenTok ^^ { 
            case ident ~ _ ~ opt ~ _  => opt match {
                case Some(list) => FnCall(tok2ident(ident), list._1 +: list._2.map(_._2)) 
                case _ => FnCall(tok2ident(ident) , List.empty[Expr])
            }
        }

    def pArray: Parser[Expr] =
        ArrayTok ~ LParenTok ~ (pExpr7 ~ (CommaTok ~ pExpr7).*).? ~ RParenTok ^^ { case _ ~ _ ~ opt ~ _ =>
            opt match {
                case Some(list) => Arr(Some(list._1 +: list._2.map(_._2)))
                case _ => Arr(None)       
            }
        }


    def tok2expr(tok: Token): Expr = tok match {
        case Str(s) => Str(s)
        case Num(n) => Num(n)
        case Bool(b) => Bool(b)
        case Ident(n) => Ident(n)
        case _ => throw new Exception("Cant convert!")
    }

    def tok2ident(tok: Token): Ident = tok match {
        case Ident(n) => Ident(n)
        case _ => throw new Exception("Cant convert!")
    }

    def convert(list: List[(Token, Expr)]): Expr = {
        if (list.tail.isEmpty) list.head._2
        else if (list.tail.tail.isEmpty) retExpr(list, list.tail.head._1, list.tail.head._2)
        else retExpr(list, list.tail.head._1, convert(list.tail))
    }

    def retExpr(list: List[(Token, Expr)], opTok: Token, secondArg: Expr): Expr = {
        opTok match {
            case OrTok => Or(list.head._2, secondArg)
            case AndTok => And(list.head._2, secondArg)
            case AddTok => Add(list.head._2, secondArg)
            case SubTok => Sub(list.head._2, secondArg)
            case MultTok => Mult(list.head._2, secondArg)
            case DivTok => Div(list.head._2, secondArg)
            case ModTok => Mod(list.head._2, secondArg)
            case ExpTok => Expon(list.head._2, secondArg)
            case _ => throw new Exception("LOL BROKEN")
        }
    }

    def pPattern: Parser[Stmt] = 
        MatchTok ~ pExpr7 ~ LCurlTok ~ pCase.+ ~ RCurlTok ^^ {
            case _ ~ expr1 ~ _ ~ cases ~ _ => Pattern(expr1, cases)
        }

    def pCase: Parser[Case] = 
        CaseTok ~ pExpr7 ~ ArrowTok ~ pStmt  ^^ {
            case _ ~ expr1 ~ _ ~ stmt => Case(expr1, stmt)
        }

    // Parse Statements
    def pStmt: Parser[Stmt] = 
        pExpr7 ~ SemiColTok ^^ { case expr ~ _ => ExprAsStmt(expr) } |
        pAssign |
        ReturnTok ~ pExpr7 ~ SemiColTok ^^ { case _ ~ expr ~ _ => Ret(expr) } |
        pWhile |
        pFor |
        pIf |
        pPrint |
        pPattern |
        pDeclar
        

    def pAssign: Parser[Stmt] =
        pExpr ~ AssgnTok ~ pExpr7 ~ SemiColTok ^^ { case ident ~ _ ~ expr ~ _ => Assign(ident, expr) } |
        pExpr ~ AddEqTok ~ pExpr7 ~ SemiColTok ^^ { case ident ~ _ ~ expr ~ _ => Assign(ident, Add(ident, expr)) } |
        pExpr ~ SubEqTok ~ pExpr7 ~ SemiColTok ^^ { case ident ~ _ ~ expr ~ _ => Assign(ident, Sub(ident, expr)) } |
        pExpr ~ DivEqTok ~ pExpr7 ~ SemiColTok ^^ { case ident ~ _ ~ expr ~ _ => Assign(ident, Div(ident, expr)) } |
        pExpr ~ MultEqTok ~ pExpr7 ~ SemiColTok ^^ { case ident ~ _ ~ expr ~ _ => Assign(ident, Mult(ident, expr)) }

    def pWhile: Parser[Stmt] = 
        WhileTok ~ LParenTok ~ pExpr7 ~ RParenTok ~ pBody ^^ {
            case _ ~ _ ~ expr ~ _ ~ body => While(expr, body)
        }

    def pFor: Parser[Stmt] =
        ForTok ~ LParenTok ~ pDeclar ~ pExpr7 ~ SemiColTok ~ pStmt ~ RParenTok ~ pBody ^^ {
            case _ ~ _ ~ declar ~ expr ~ _ ~ stmt ~ _ ~ body => For(declar, expr, stmt, body) 
        }

    def pIf: Parser[Stmt] =
        IfTok ~ LParenTok ~ pExpr7 ~ RParenTok ~ pBody ~ (ElseTok ~ pBody).? ^^ {
            case _ ~ _ ~ expr1 ~ _ ~ body1 ~ opt => opt match {
                case None => If(expr1, body1, None)
                case _ => If(expr1, body1, Some(opt.get._2))
            }
        }

    def pBody: Parser[Stmt] = 
        LCurlTok ~ (pStmt | pDeclar).* ~ RCurlTok ^^ { 
            case _ ~ list ~ _ => Body(list)
        }

    def tok2type(tok: Token): Type = tok match {
        case NumType => NumType
        case StrType => StrType
        case BoolType => BoolType
        case VoidType => VoidType
        case _ => throw new Exception("Not a Type")
    }

    def pPrint: Parser[Stmt] = 
        PrintTok ~ pExpr7 ~ SemiColTok ^^ { case _ ~ expr ~ _ => Print(expr) }

    // Parse Declarations
    def pDeclar: Parser[Stmt] = 
        pVarDef |
        pConstDef |
        pFuncDef

    def pType: Parser[Type] = 
        NumType ^^ { case num => tok2type(num) } |
        StrType ^^ { case str => tok2type(str) } |
        BoolType ^^ { case bool => tok2type(bool) } |
        VoidType ^^ { case void => tok2type(void) } |
        pArrayType

    def pArrayType: Parser[Type] = 
        ArrayTok ~ LSquareTok ~ pType ~ RSquareTok ^^ { case _ ~ _  ~ typ ~ _ => ArrayType(typ) }

    def pVarDef: Parser[Stmt] =
        pType ~ pIdent ~ AssgnTok ~ pExpr7 ~ SemiColTok ^^ {
            case typ ~ ident ~ _ ~ expr ~ _ => VarDef(typ, ident, expr)
        }

    def pConstDef: Parser[Stmt] =
        ConstTok ~ pType ~ pIdent ~ AssgnTok ~ pExpr7 ~ SemiColTok ^^ {
            case _ ~ typ ~ ident ~ _ ~ expr ~ _ => ConstDef(typ, ident, expr)
        }

    def pFuncDef: Parser[Stmt] = 
        pType ~ pIdent ~ LParenTok ~ (pIdent ~ ColonTok ~ pType ~ (CommaTok ~ pIdent ~ ColonTok ~ pType).*).? ~ RParenTok ~ pBody ^^ {
            case typ ~ ident ~ _ ~ opt ~ _ ~ body => opt match {
                case Some(list) => 
                    val (first, second) = list
                    val tmpList = (first._1._1, first._2) +: second.map(x => (x._1._1._2, x._2))
                    val argList = tmpList.map(x => Argument(x._1, x._2))
                    FnDef(typ, ident, argList, body)
                case _ => FnDef(typ, ident, List.empty[Argument], body)
            }
        }
}

//(((lang.Expr, lang.Token), lang.Type), List[(((lang.Token, lang.Expr), lang.Token), lang.Type)])
// (Expr, Type)
// first = opt._1
// (first._1._1, first._2)
// second = opt._2
// second.map((_._1._1, _._2))
//second.map(x => (x._1._1._2, x._2))

//(opt._1._1._1, opt._1._2) +: opt._2.map((_._1._1, _._2))
// dear lord