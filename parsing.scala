// CS478 Homework 14
// <<< !!!INCLUDE YOUR NAME HERE!!! >>>

package parsing // see note A

class ParseError(msg: String) extends Exception(msg) { // like in HW13
  override def toString = s"Parse Error: $msg"
}

object ~ { // see notes B and C
  def unapply[A,B](pair: (A,B)): Option[(A,B)] = Some(pair)
}

trait Parsers[Token] { // see note D

  object Parser { // see note E
    def apply[A](logic: List[Token] => (A, List[Token])): Parser[A] =
      new Parser(logic)
  }

  // see note F about the class
  // see note G about the +A
  class Parser[+A](logic: List[Token] => (A, List[Token])) {

    // see note H on apply
    def apply(toks: List[Token]) = logic(toks)

    // see note I about the [B >: A]
    def |[B >: A](pb: => Parser[B]): Parser[B] = Parser{ toks =>
      try this(toks)
      catch { case _: ParseError => pb(toks) }
    }

    // see note J about the => in the type for pb
    /* def ~[B](pb: => Parser[B]): Parser[(A,B)] = Parser{ toks =>
      val (a, toks2) = this(toks)
      val (b, toks3) = pb(toks2)
      ((a, b), toks3)
    } */

    /* def ~[B](pb: => Parser[B]): Parser[(A,B)] = 
      this >> { a => pb ^^ { b => (a, b) } } */

    /* def ~[B](pb: => Parser[B]): Parser[(A,B)] = 
      this >> { a => pb >> { b => succeed(a, b) } } */

    def ~[B](pb: => Parser[B]): Parser[(A,B)] = 
      for (a <- this; b <- pb) yield(a, b)

    // see note K
    /* def map[B](f: A => B): Parser[B] = Parser{ toks =>
      val (a, toks2) = this(toks)
      (f(a), toks2)
    } */

    def map[B](f: A => B): Parser[B] = this >> { a => succeed(f(a)) }

    // see note L
    def ^^[B](f: A => B): Parser[B] = this.map(f)
    def >>[B](f: A => Parser[B]): Parser[B] = this.flatMap(f)

    // see note M
    def ? : Parser[Option[A]] = this ^^ { a => Some(a) } | succeed(None)
    def * : Parser[List[A]] = this.+ | succeed(List.empty)
    def + : Parser[List[A]] = this  ~ this.* ^^ { case a ~ list => a +: list }

    // see notes N and O
    /* def filter(predicate: A => Boolean): Parser[A] = Parser{ toks =>
      val (a, toks2) = this(toks)
      if (predicate(a)) (a, toks2)
      else throw new ParseError("Wrong")
    } */

    def filter(predicate: A => Boolean): Parser[A] = 
      this >> { a => if (predicate(A)) succeed(a) else fail("Wrong") }

    def flatMap[B](f: A => Parser[B]): Parser[B] = Parser{ toks =>
      val (a, toks2) = this(toks)
      f(a)(toks2)
    }

    // avoids warning when using for-comprehensions
    def withFilter(predicate: A => Boolean): Parser[A] = filter(predicate)

    // see note P
    def parse(toks: List[Token]): A = ???

    // Extra Credit methods
    def !~[B](pb: => Parser[B]): Parser[B] = ???
    def ~![B](pb: => Parser[B]): Parser[A] = ???
    def separate[B](pb: => Parser[B]): Parser[List[A]] = ???
    def separate1[B](pb: => Parser[B]): Parser[List[A]] = ???
    def lookahead: Parser[A] = ???
  } // end of class Parser

  // see note Q
  def succeed[A](a:A): Parser[A] = Parser{ toks => (a, toks) }
  def fail(msg: String): Parser[Nothing] = Parser{ toks => throw new ParseError(msg) }
  def any: Parser[Token] = Parser{ toks => 
    if (toks.nonEmpty) (toks.head, toks.tail)
    else throw new ParseError("Empty list of toks!") 
  }

  // Extra Credit function
  def endOfInput: Parser[Unit] = ???

  // see note R on implicits
  import scala.language.implicitConversions
  implicit def token(tok: Token): Parser[Token] = any.filter(_ == tok)
} // end of trait Parsers
