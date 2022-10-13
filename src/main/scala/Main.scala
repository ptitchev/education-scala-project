import fr.esiee.parser_combinator.Input

@main
def hello: Unit =
  findDataType("Le sdkjsd 123")


def findDataType(x: Any) = x match {
 case x : String => println("String identified")
 case x : Int => println("Integer identified")
 case x : Float => println("Float identified")
 case x : Double => println("Double identified")
 case _ => println("DataType not identified")
}

// Common interface for parsers
trait Parser[A]:
 /** parse with this and then parse with pb. */
 def ~[B](pb: => Parser[B]): Parser[(A, B)] = ???

 /** parse with this, or parse with pb if this fails. */
 def |[B](pb: Parser[B]): Parser[Either[A, B]] = ???

 /** try to parse with this. It does not fail if the parsing did not work. */
 def ? : Parser[Option[A]] = ???

 /** use this to parse multiple times, until it does not apply. */
 def repeat: Parser[List[A]] = ???

 /** convert the value output by the parser. */
 def map[B](f: A => B): Parser[B] = ???

 def flatMap[B](f: A => Parser[B]): Parser[B] = ???

 final def parse(s: String): ParseResult[A] = parse(Input(s))

 def parse(input: Input): ParseResult[A]

// Declaration of some basic parsers
object Parser:
 def createParser[A](f: Input => ParseResult[A]): Parser[A] = input => f(input)

 /** parse an integer. */
 def int: Parser[Int] = createParser(???)

 /** parse exactly the string s */
 def string(s: String): Parser[String] = createParser(???)

 /** parse according to a regular expression */
 def regex(r: String): Parser[String] = createParser(???)

// Result of parse
enum ParseResult[+A]:
 case ParseFailure(onInput: Input) extends ParseResult[Nothing]
 case ParseSucceed(value: A, remainingInput: Input) extends ParseResult[A]

 def map[B](f: A => B): ParseResult[B] = ???

 def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = ???
 