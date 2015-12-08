package lang

import parsing._

object ExprParser extends parsing.Parsers[Token] {

    def pExpr: Parser[Expr] =
        pLiteral |
        pIdent |
        pFuncCall |
        pExpr7 

    def pExpr0: Parser[Expr] = 
        pExpr0 ~ ExpTok ~ pExpr ^^ { case expr1 ~ _ ~ expr2 => Expon(expr1, expr2) } |
        pExpr

    def pExpr1: Parser[Expr] =
        SubTok ~ pExpr1 ^^ { case _ ~ expr => Neg(expr) } |
        pExpr0

    def pExpr2: Parser[Expr] = 
        pExpr2 ~ MultTok ~ pExpr1 ^^ { case expr1 ~ _ ~ expr2 => Mult(expr1, expr2) } |
        pExpr2 ~ DivTok ~ pExpr1 ^^ { case expr1 ~ _ ~ expr2 => Div(expr1, expr2) } |
        pExpr2 ~ ModTok ~ pExpr1 ^^ { case expr1 ~ _ ~ expr2 => Mod(expr1, expr2) } |
        pExpr1

    def pExpr3: Parser[Expr] = 
        pExpr3 ~ AddTok ~ pExpr2 ^^ { case expr1 ~ _ ~ expr2 => Add(expr1, expr2) } |
        pExpr3 ~ SubTok ~ pExpr2 ^^ { case expr1 ~ _ ~ expr2 => Sub(expr1, expr2) } |
        pExpr2


    def pExpr4: Parser[Expr] = 
        NotTok ~ pExpr4 ^^ { case _ ~ expr => Not(expr) } |
        pExpr3

    def pExpr5: Parser[Expr] = 
        pExpr ~ LTTok ~ pExpr ^^ { case expr1 ~ _ ~ expr2 => LT(expr1, expr2) } |
        pExpr ~ GTTok ~ pExpr ^^ { case expr1 ~ _ ~ expr2 => GT(expr1, expr2) } |
        pExpr ~ LTEqTok ~ pExpr ^^ { case expr1 ~ _ ~ expr2 => LTE(expr1, expr2) } |
        pExpr ~ GTEqTok ~ pExpr ^^ { case expr1 ~ _ ~ expr2 => GTE(expr1, expr2) } |
        pExpr ~ EqualTok ~ pExpr ^^ { case expr1 ~ _ ~ expr2 => Equal(expr1, expr2) } |
        pExpr ~ NotEqualTok ~ pExpr ^^ { case expr1 ~ _ ~ expr2 => NotEqual(expr1, expr2) } |
        pExpr4

    def pExpr6: Parser[Expr] = 
        pExpr6 ~ AndTok ~ pExpr5 ^^ { case expr1 ~ _ ~ expr2 => And(expr1, expr2) } |
        pExpr5

    def pExpr7: Parser[Expr] = 
        pExpr7 ~ OrTok ~ pExpr6 ^^ { case expr1 ~ _ ~ expr2 => Or(expr1, expr2) } | 
        pExpr6

    def pLiteral: Parser[Expr] = 
        Str(s) ^^ { case str => str} |
        Num(n) ^^ { case num => num} |
        Bool(b) ^^ { case bool => bool }

    def pIdent: Parser[Expr] = 
        Ident(name) ^^ { case ident => ident }

    def pFuncCall: Parser[Expr] = 
        Ident(n) ~ LParenTok ~ (pExpr7 ~ (CommaTok ~ pExpr7).*).? ~ RParenTok ~ SemiColTok ^^ { 
            case ident ~ _ ~ opt ~ _ ~ _ => opt match {
                case Some(list) => FnCall(ident, list) 
                case _ => FnCall(Ident(n), list.empty[Expr])
            }
        }
}