package lang

import io.Source

object Wrapper {
    
    def scanProgram(fpath: String): List[Token] = {
        val text = Source.fromFile(fpath).mkString
        Scanner.scanner(text)
    }

    def parseProgram(fpath: String): Prog = {
        val toks = scanProgram(fpath)
        Parser.parse(toks)
    }

    def interpretProgram(fpath: String): Unit = {
        val prog = parseProgram(fpath)
        val ans = Interpreter.interpret(prog)
        println(ans)
    }

}