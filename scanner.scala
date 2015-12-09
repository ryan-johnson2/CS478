package lang

object Scanner {

    def scanner(text: String): List[Token] = {
        //variable storing the length of the text
        val tLen = text.length

        def scan(index: Int): List[Token] = {
            // make the index a var so we can change it later and check to see if 
            // we are at the end of text
            var i = index

            //return a list with the end of input token if we are at the end of the list
            if (i >= tLen) return List(RCurlTok)

            //get the character in text at the index
            val c = text(i)

            // This section of code will look at the charcter and
            // attempt to determine the token needed for it, storing data
            // when needed
            val str = text.slice(i, tLen)

            //Deal with all keywords and identifiers
            if (c.isLetter) {
                val start = i
                while (i < tLen && (text(i).isLetter || text(i).isDigit || text(i) == '_')) i += 1
                val strt = text.slice(start, i)
                strt match {
                    case "or" => OrTok +: scan(i)
                    case "if" => IfTok +: scan(i)
                    case "and" => AndTok +: scan(i)
                    case "int" => NumType +: scan(i)
                    case "for" => ForTok +: scan(i)
                    case "not" => NotTok +: scan(i)
                    case "void" => VoidType +: scan(i)
                    case "else" => ElseTok +: scan(i)      
                    case "while" => WhileTok +: scan(i)
                    case "return" => ReturnTok +: scan(i)
                    case "string" => StrType +: scan(i)
                    case "boolean" => BoolType +: scan(i)
                    case "true" => Bool(true) +: scan(i)
                    case "false" => Bool(false) +: scan(i)
                    case "print" => PrintTok +: scan(i)
                    case _ => Ident(strt) +: scan(i)
                }                
            }

            //Deal with symbols
            else if (str.startsWith("==")) EqualTok +: scan(i+2)
            else if (c == '=') AssgnTok +: scan(i + 1)
            else if (str.startsWith("<=")) LTEqTok +: scan(i+2)
            else if (c == '<') LTTok +: scan(i + 1)
            else if (str.startsWith(">=")) GTEqTok +: scan(i+2)
            else if (c == '>') GTTok +: scan(i + 1)
            else if (c == '(') LParenTok +: scan(i + 1)
            else if (c == ')') RParenTok +: scan(i + 1)
            else if (c == '{') LCurlTok +: scan(i + 1)
            else if (c == '}') RCurlTok +: scan(i + 1)
            else if (c == '[') LSquareTok +: scan(i + 1)
            else if (c == ']') RSquareTok +: scan(i + 1)
            else if (c == ':') ColonTok +: scan(i + 1)
            else if (str.startsWith("**")) ExpTok +: scan(i+2)
            else if (str.startsWith("*=")) MultEqTok +: scan(i+2)
            else if (c == '*') MultTok +: scan(i + 1)
            else if (c == ';') SemiColTok +: scan(i + 1)
            else if (str.startsWith("+=")) AddEqTok +: scan(i+2)
            else if (c == '+') AddTok +: scan(i + 1)
            else if (str.startsWith("-=")) SubEqTok +: scan(i+2)
            else if (c == '-') SubTok +: scan(i + 1)
            else if (str.startsWith("//")){
                while (i < tLen && text(i) != '\n') i += 1
                scan(i + 1)
            }
            else if (str.startsWith("/=")) DivEqTok +: scan(i+2)
            else if (c == '/') DivTok +: scan(i + 1)
            else if (c == '%') ModTok +: scan(i + 1)
            else if (c == ',') CommaTok +: scan(i + 1)
            else if (str.startsWith("!=")) NotEqualTok +: scan(i+2)
            else if (c == '`') ConstTok +: scan(i + 1)

            //creates token for a string literal
            else if (c == '"') {
                i += 1
                val start = i
                while (i < tLen && text(i) != '"') i += 1
                if (i < tLen) Str(text.slice(start, i)) +: scan(i + 1)
                else throw new Exception("Never ended the string")
            }

            //create token for a numerical literal and float literal
            else if (c.isDigit) {
                val start = i
                while (i < tLen && text(i).isDigit) i += 1
                Num(text.slice(start, i).toInt) +: scan(i)
            } 

            //skips whitespace chars
            else if (c.isWhitespace) scan(i + 1)
            //throws exception if not accounted for
            else throw new Exception("Invalid character: '" + c + "'")
        }

        //run scan starting at the beginning of text
        LCurlTok +: scan(0)
    }
}