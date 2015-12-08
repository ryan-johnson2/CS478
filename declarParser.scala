package lang

import parsing._

object DeclarParser extends parsing.Parsers[Token] {

    def pDeclar: Parser[Stmt] = 
        pVarDef |
        pConstDef |
        pFuncDef

    def pType: Parser[Type] = 
        NumType ^^ { case num => num } |
        SrtType ^^ { case str => str } |
        BoolType ^^ { case bool => bool } |
        VoidType ^^ { case void => void } 

    def pVarDef: Parser[Stmt] =
        pType ~ ExprParser.pIdent ~ AssgnTok ~ ExprParser.pExpr ~ SemiColTok ^^ {
            case typ ~ ident ~ _ ~ expr ~ _ => VarDef(typ, ident, expr)
        }

    def pConstDef: Parser[Stmt] =
        ConstTok ~ pType ~ ExprParser.pIdent ~ AssgnTok ~ ExprParser.pExpr ~ SemiColTok ^^ {
            case _ ~ typ ~ ident ~ _ ~ expr ~ _ => ConstDef(typ, ident, expr)
        }

    def pFuncDef: Parser[Stmt] = 
        pType ~ ExprParser.pIdent ~ LParenTok ~ (ExprParser.pIdent ~ ColonTok ~ pType ~ (CommaTok ~ ExprParser.pIdent ~ ColonTok ~ pType).*).? ~ RParenTok ~ StmtParser.pBody ^^ {
            case typ ~ ident ~ _ ~ opt ~ _ ~ body => opt match {
                case Some(list) => FnDef(typ, ident, list, body)
                case _ => FnDef(typ, ident, list.empty[Argument], body)
            }
        }

}