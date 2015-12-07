package lang

import parsing._

object StmtParser extends parsing.Parsers[Token] {

    def pStmt: Parser[Stmt] = 
        ExprParser.pExpr ~ SemiColTok ^^ { case expr ~ _ => ExprAsStmt(expr) } |
        pAssign |
        ReturnTok ~ ExprParser.pExpr ~ SemiColTok ^^ { case _ ~ expr ~ _ => Ret(expr) } |
        pWhile |
        pFor |
        pIf
        

    def pAssign: Parser[Stmt] =
        ExprParser.pIdent ~ EqualTok ~ ExprParser.pExpr ~ SemiColTok ^^ { case ident ~ _ ~ expr ~ _ => Assign(ident, expr) } |
        ExprParser.pIdent ~ AddEqTok ~ ExprParser.pExpr ~ SemiColTok ^^ { case ident ~ _ ~ expr ~ _ => Assign(ident, Add(ident, expr)) } |
        ExprParser.pIdent ~ SubEqTok ~ ExprParser.pExpr ~ SemiColTok ^^ { case ident ~ _ ~ expr ~ _ => Assign(ident, Sub(ident, expr)) } |
        ExprParser.pIdent ~ DivEqTok ~ ExprParser.pExpr ~ SemiColTok ^^ { case ident ~ _ ~ expr ~ _ => Assign(ident, Div(ident, expr)) } |
        ExprParser.pIdent ~ MultEqTok ~ ExprParser.pExpr ~ SemiColTok ^^ { case ident ~ _ ~ expr ~ _ => Assign(ident, Mult(ident, expr)) }

    def pWhile: Parser[Stmt] = 
        WhileTok ~ LParenTok ~ ExprParser.pExpr ~ RParenTok ~ pBody ^^ {
            case _ ~ _ ~ expr ~ _ ~ body => While(expr, body)
        }

    def pFor: Parser[Stmt] =
        ForTok ~ LParenTok ~ DeclarParser.pDeclar ~ SemiColTok ~ ExprParser.pExpr ~ SemiColTok ~ pStmt ~ RParenTok ~ pBody ^^ {
            _ ~ _ ~ declar ~ _ ~ expr ~ _ ~ stmt ~ _ ~ body => For(declar, expr, stmt, body) 
        }

    def pIf: Parser[Stmt] =
        IfTok ~ LParenTok ~ ExprParser.pExpr ~ RParenTok ~ pBody ~ (ElseTok ~ pBody).? ^^ {
            case _ ~ _ ~ expr1 ~ _ ~ body1 ~ list ~ opt => If ()
        }

}