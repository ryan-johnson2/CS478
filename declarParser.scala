package lang

import parsing._

object DeclarParser extends parsing.Parsers[Token] {

    def pDeclar = 
        

    def pType: Parser[Type] = 
        NumType ^^ { case num => num } |
        SrtType ^^ { case str => str } |
        BoolType ^^ { case bool => bool } |
        VoidType ^^ { case void => void } 

    def pVarDef: Parser[Stmt] =
        pType ~ ExprParser.pIdent ~ AssgnTok ~ ExprParser.pExpr ~ SemiColTok ^^ {
            case type ~ ident ~ _ ~ expr ~ _ => VarDef(type, ident, expr)
        }

    def pConstDef: Parser[Stmt] =
        ConstTok ~ pType ~ ExprParser.pIdent ~ AssgnTok ~ ExprParser.pExpr ~ SemiColTok ^^ {
            case _ ~ type ~ ident ~ _ ~ expr ~ _ => ConstDef(type, ident, expr)
        }

    def pFuncDef: Parser[Stmt] = 
        pType ~ ExprParser.pIdent ~ LParenTok ~ (ExprParser.pIdent ~ ColonTok ~ pType ~ (CommaTok ~ ExprParser.pIdent ~ ColonTok ~ pType).*).? ~ RParenTok ~ StmtParser.pBody ^^ {
            case type ~ ident ~ _ ~ opt ~ _ ~ body => opt match {
                case Some(list) => FnDef(type, ident, list, body)
                case _ => FnDef(type, ident, list.empty[Argument], body)
            }
        }

}